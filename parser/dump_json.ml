open Api;;

(* tuple format *)
(* id, size, operators, solved, timeleft *)

let problem_json = Yojson.Safe.from_channel stdin in
let problems = parse_problems_json problem_json in
let num_solved = List.length(List.filter (fun (_, _, _, solved, _) -> solved) problems) in
let not_solved = List.filter (fun (_, _, _, solved, _) -> not solved) problems in
let sorted_problems =
  List.sort
    (fun (_, x_size, (_, _, x_s), _, _) (_, y_size, (_, _, y_s), _, _) ->
      let l_diff = (List.length x_s) - (List.length y_s) in
      if l_diff != 0
      then l_diff
      else x_size - y_size)
    not_solved in
let _ = print_endline
  (Printf.sprintf "Solved: %4d/%4d" num_solved
     (List.length problems)) in
List.iteri
  (fun i x -> print_endline (format_problem x i))
  sorted_problems
;;
