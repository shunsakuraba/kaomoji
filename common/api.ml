open Type

let api_site = "http://icfpc2013.cloudapp.net"
let auth = "AUTHKEYvpsH1H"

exception Parse_error
exception Fetch_error

let parse_operator_string_list l =
  let unops = ref [] in
  let binops = ref [] in
  let statements = ref [] in
  List.iter
    (function
      | `String(s) ->
        if s = "not" then unops := Not :: !unops
        else if s = "shl1" then unops := Shl1 :: !unops
        else if s = "shr1" then unops := Shr1 :: !unops
        else if s = "shr4" then unops := Shr4 :: !unops
        else if s = "shr16" then unops := Shr16 :: !unops
        else if s = "and" then binops := And :: !binops
        else if s = "or" then binops := Or :: !binops
        else if s = "xor" then binops := Xor :: !binops
        else if s = "plus" then binops := Plus :: !binops
        else if s = "if0" then statements := SIf0 :: !statements
        else if s = "fold" then statements := SFold :: !statements
        else if s = "tfold" then statements := STfold :: !statements
        else raise Parse_error
      | _ -> raise Parse_error
    )
    l;
  !unops, !binops, !statements

let format_operator_tuple (unops, binops, statements) =
  Printf.sprintf
    "[%s] [%s] [%s]"
    (String.concat "," (List.map unop_to_string unops))
    (String.concat "," (List.map binop_to_string binops))
    (String.concat "," (List.map statement_to_string statements))

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
            `Intlit(s) -> ()
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
