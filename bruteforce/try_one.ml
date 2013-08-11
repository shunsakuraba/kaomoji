open ExtList

let solve_one_problem problem =
  let _ = print_endline (Problem.format_problem problem 0) in
  let (id, size, ops, solved, time_left) = problem in
  let core_problem = (id, size, ops) in
  Solver.solve core_problem false

let main =
  let _ = Random.self_init() in
  let prob_id = Sys.argv.(1) in
  let good_problems = Remote.fetch_good_problems 0 "" in
  let matched_problems = List.filter (fun p ->
    let (id, size, ops, solved, time_left) = p in
    id = prob_id) good_problems in
  let problem = List.nth matched_problems 0 in
  try
    solve_one_problem problem
  with Brute.CandidateSizeLooksTooBigException ->
    prerr_endline "candidate size too big"
