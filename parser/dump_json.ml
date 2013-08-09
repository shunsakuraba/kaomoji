open Api;;

let problem_json = Yojson.Safe.from_channel stdin in
let problems = parse_problems_json problem_json in
let sorted_problems =
  List.sort
    (fun (_, x_size, (_, _, x_s), _, _) (_, y_size, (_, _, y_s), _, _) ->
      let l_diff = (List.length x_s) - (List.length y_s) in
      if l_diff != 0
      then l_diff
      else x_size - y_size)
    problems in
List.iteri
  (fun i x -> print_problem x i)
  sorted_problems
;;
