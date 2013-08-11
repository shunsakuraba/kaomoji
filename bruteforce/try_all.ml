open ExtList
open Type

let main =
  let good_problems = Remote.fetch_good_problems 0 "" in
  let target_problems = List.filter (fun problem ->
    let (id, size, ops, solved, time_left) = problem in
    let (unops, binops, statements) = ops in
    (size < 20) || ((List.mem STfold statements) && size < 25))
    good_problems in
  List.iter (fun problem ->
    let (id, size, ops, solved, time_left) = problem in
    print_endline (Printf.sprintf "./run.sh %s" id)) target_problems
