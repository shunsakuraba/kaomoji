open Api;;
open Type;;

let fetch_train size operators =
  let problem_url = api_site ^ "/train?auth=" ^ auth in
  let call =
    new Http_client.post_raw
      problem_url
      (Yojson.Safe.to_string
         (`Assoc([("size", `Int(size));
                  ("operators", `String(operators))]))) in
  let pipeline = new Http_client.pipeline in
  pipeline # add call;
  pipeline # run();
  call # response_body # value
;;

let parse_train_string s =
  let scan_train_kv_list kv_list =
    let id = ref "" in
    let size = ref 0 in
    let operators = ref ([], [], []) in
    let challenge = ref "" in

    let parse_kv (key, value) =
      if key = "id" then
        match value with
            `String(s) -> id := s
          | _ ->
            raise Parse_error
      else if key = "size" then
        match value with
            `Int(i) -> size := i
          | _ ->
            raise Parse_error
      else if key = "operators" then
        match value with
            `List(l) -> operators := parse_operator_string_list l
          | _ ->
            raise Parse_error
      else if key = "challenge" then
        match value with
            `String(s) -> challenge := s
          | _ ->
            raise Parse_error
      else
        raise Parse_error in

    List.iter parse_kv kv_list;
    (!id, !size, !operators, !challenge) in

  let train_json = Yojson.Safe.from_string s in
  match train_json with
    | `Assoc(kv_list) ->
      scan_train_kv_list kv_list
    | _ ->
      raise Parse_error
;;

let format_train (id, size, (unops, binops, statements), challenge) index =
  Printf.sprintf
    "%4d %s: %d [%s] [%s] [%s] %s"
    index
    id
    size
    (String.concat "," (List.map unop_to_string unops))
    (String.concat "," (List.map binop_to_string binops))
    (String.concat "," (List.map statement_to_string statements))
    challenge
;;
