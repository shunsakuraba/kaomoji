open Api
open Type

let parse_train_string s =
  let scan_kv_list kv_list =
    let id = ref "" in
    let size = ref 0 in
    let operators = ref ([], [], []) in
    let challenge = ref "" in

    let parse_kv = function
      | "id", `String(s) -> id := s
      | "size", `Int(i) -> size := i
      | "operators", `List(l) -> operators := parse_operator_string_list l
      | "challenge", `String(s) -> challenge := s
      | key, _ ->
        print_endline ("Failed to parse " ^ key);
        raise Parse_error
    in

    List.iter parse_kv kv_list;
    !id, !size, !operators, !challenge
  in

  let train_json = Yojson.Safe.from_string s in
  match train_json with
    | `Assoc(kv_list) ->
      scan_kv_list kv_list
    | _ ->
      raise Parse_error

let format_train (id, size, operators, challenge) index =
  Printf.sprintf
    "%4d %s: %2d %s %s"
    index
    id
    size
    (format_operator_tuple operators)
    challenge

let fetch_train size operators =
  print_endline "Fetching trains";
  let url = api_site ^ "/train?auth=" ^ auth in
  let json =
    if operators = "-fold" then
      Yojson.Safe.to_string
        (`Assoc(
          [("size", `Int(size));
           ("operators", `List([]))]))
    else if operators = "" then
      Yojson.Safe.to_string
	(`Assoc(
          [("size", `Int(size))]))
    else if operators = "bonus" then
      Yojson.Safe.to_string
	(`Assoc(
          [("size", `Int(42))]))
    else
      Yojson.Safe.to_string
        (`Assoc(
          [("size", `Int(size));
           ("operators", `List([`String(operators)]))])) in
  let _ = print_endline json in
  let call =
    new Http_client.post_raw url json in
  let pipeline = new Http_client.pipeline in
  pipeline # add call;
  pipeline # run();
  let status = call # response_status_code in
  if status <> 200 then
    begin
      print_endline ("fetch_train failed status: " ^ (string_of_int status));
      raise Fetch_error
    end
  else
    call # response_body # value

let rec fetch_train_with_retry size operator =
  let train_string =
    if operator = "bonus" then
      fetch_train 42 ""
    else
      fetch_train size operator
  in
  let train = parse_train_string train_string in
  let (t_id, t_size, t_ops, t_challenge) = train in
  if t_size <> size then
    fetch_train_with_retry size operator
  else
    let (unops, binops, statements) = t_ops in
    if Util.is_match_ops_limit statements operator then
      train
    else
      fetch_train_with_retry size operator

let read_train_from_file path =
  let in_channel = open_in path in
  let train_string = input_line in_channel in
  parse_train_string train_string
