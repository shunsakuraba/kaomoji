open Api;;
open Type;;

let scan_problem_kv_list kv_list =
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
          print_endline "no";
          raise Not_found
    else if key = "size" then
      match value with
          `Int(i) -> size := i
        | _ ->
          print_endline "no";
          raise Not_found
    else if key = "operators" then
      match value with
          `List(l) -> operators := parse_operator_string_list l
        | _ ->
          print_endline "no";
          raise Not_found
    else if key = "solved" then
      match value with
          `Bool(b) -> solved := b
        | _ ->
          print_endline "no";
          raise Not_found
    else if key = "timeLeft" then
      match value with
          `Intlit(s) -> ()
        | _ ->
          print_endline "c";
          raise Not_found
    else
      raise Not_found in

  List.iter parse_kv kv_list;
  (!id, !size, !operators, !solved, !time_left)
;;

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

