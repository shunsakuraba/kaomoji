let solve_one_problem problem =
  let _ = print_endline (Problem.format_problem problem 0) in
  let (id, size, ops, solved, time_left) = problem in
  let core_problem = (id, size, ops) in
  Solver.solve core_problem

let main =
  let _ = Random.self_init() in
  let good_problems = Remote.fetch_good_problems 0 "" in
  if (List.length good_problems) > 0 then
    let prob_index = 0 in
    (* let prob_index = Random.int (List.length good_problems) in *)
    let problem = (List.nth good_problems prob_index) in
    solve_one_problem problem false

