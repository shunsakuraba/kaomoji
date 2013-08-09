let prefix = "http://icfpc2013.cloudapp.net"
let auth = "AUTHKEYvpsH1H"

let problem_url = prefix ^ "/myproblems?auth=" ^ auth

(* let fetch () = *)
(*   Http_user_agent.get problem_url;; *)

(* print_endline (fetch ()) *)

let convert_string_list l =
  List.map
    (function
      | `String(s) -> s
      | _ -> raise Not_found
    )
    l
;;

let scan_kv_list kv_list =
  let id = ref "" in
  let size = ref 0 in
  let operators = ref [] in
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
          `List(l) -> operators := convert_string_list l
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

let parse = function
  | `Assoc(kv_list) ->
    scan_kv_list kv_list
  | _ ->
    print_endline "b";
    raise Not_found
;;

let problems = Yojson.Safe.from_file "problems.json";;

let print_problem (id, size, operators, solved, time_left) =
  let problem_string = Printf.sprintf "%s: %d [%s] %s %s" id size (String.concat "," operators) (if solved then "T" else "F") time_left in
  print_endline problem_string
;;

match problems with
    `List(l) ->
      (
        let parsed_problems = List.map parse l in
        let sorted_problems =
          List.sort
            (fun (_, x_size, _, _, _) (_, y_size, _, _, _) -> x_size - x_size)
            parsed_problems in
        List.iter
          (fun x -> print_problem x)
          sorted_problems
      )
  | _ -> raise Not_found;;
