open Api;;
open Type;;

let parse_problem_json j =
  let scan_kv_list kv_list =
    let id = ref "" in
    let size = ref 0 in
    let operators = ref ([], [], []) in
    let solved = ref false in
    let time_left = ref "" in

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
      else if key = "solved" then
        match value with
            `Bool(b) -> solved := b
          | _ ->
            raise Parse_error
      else if key = "timeLeft" then
        match value with
            `Float(s) -> ()
          | _ ->
            raise Parse_error
      else
        raise Parse_error in

    List.iter parse_kv kv_list;
    (!id, !size, !operators, !solved, !time_left) in

  match j with
    | `Assoc(kv_list) ->
      scan_kv_list kv_list
    | _ ->
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
  let problem_url = api_site ^ "/myproblems?auth=" ^ auth in
  Http_user_agent.get problem_url

let is_match_ops_limit statements ops_limit =
  if ops_limit = "" then
    true
  else if ops_limit = "fold" then
    List.mem SFold statements
  else if ops_limit = "tfold" then
    List.mem STfold statements
  else
    failwith "Unsupported ops_limit"

let fetch_good_problems size_limit ops_limit =
  let problems_body = fetch_problems () in
  let problems_json = Yojson.Safe.from_string problems_body in
  let problems = parse_problems_json problems_json in
  let good_problems = List.find_all
    (fun x -> let (id, size, (unops, binops, statements), solved, time_left) = x in
	      if solved then
		false
	      else if size <> size_limit then
		false
	      else 
		is_match_ops_limit statements ops_limit)
    problems in
  good_problems

