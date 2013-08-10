open Api
open Type

let parse_problem_json j =
  let scan_kv_list kv_list =
    let id = ref "" in
    let size = ref 0 in
    let operators = ref ([], [], []) in
    let solved = ref false in
    let time_left = ref "" in

    let parse_kv = function
      | "id", `String(s) -> id := s
      | "size", `Int(i) -> size := i
      | "operators", `List(l) -> operators := parse_operator_string_list l
      | "solved", `Bool(b) -> solved := b
      | "timeLeft", _ -> ()
      | key, _ ->
        print_endline ("Failed to parse " ^ key);
        raise Parse_error
    in

    List.iter parse_kv kv_list;
    !id, !size, !operators, !solved, !time_left
  in

  match j with
    | `Assoc(kv_list) ->
      scan_kv_list kv_list
    | _ ->
      print_endline "Failed to parse problem json";
      raise Parse_error

let parse_problems_json = function
  | `List(l) -> List.map parse_problem_json l
  | _ -> raise Parse_error

let format_problem (id, size, operators, solved, time_left) index =
  Printf.sprintf
    "%4d %s: %2d %s %s %s"
    index
    id
    size
    (format_operator_tuple operators)
    (if solved then "T" else "F")
    time_left

let fetch_problems () =
  print_endline "Fetching problems";
  let problem_url = api_site ^ "/myproblems?auth=" ^ auth in
  Http_user_agent.get problem_url

let is_match_ops_limit statements ops_limit =
  match ops_limit with
      "" -> true
    | "fold" -> List.mem SFold statements
    | "tfold" -> List.mem STfold statements
    | _ -> failwith "Unsupported ops_limit"

let fetch_good_problems size_limit ops_limit =
  let problems_body = fetch_problems () in
  let problems_json = Yojson.Safe.from_string problems_body in
  let problems = parse_problems_json problems_json in
  let good_problems = List.find_all
    (fun x ->
      let (id, size, (unops, binops, statements), solved, time_left) = x in
      if solved then
        false
      else if size <> size_limit then
        false
      else
is_match_ops_limit statements ops_limit)
    problems in
  good_problems

let read_local_problems () =
  print_endline "Reading local problem";
  let problem_string = read_line () in
  Yojson.Safe.from_string problem_string
