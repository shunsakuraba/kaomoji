let solve_one_problem problem =
  let _ = print_endline (Problem.format_problem problem 0) in
  let (id, size, ops, solved, time_left) = problem in
  let core_problem = (id, size, ops) in
  Solver.solve core_problem false size

let main =
  let _ = Random.self_init() in
  let good_problems = Remote.fetch_good_problems 0 "" in
  for prob_index = 0 to (List.length good_problems) do
    let problem = (List.nth good_problems prob_index) in
    try
      solve_one_problem problem
    with Brute.CandidateSizeLooksTooBigException ->
      prerr_endline "candidate size too big"
  done
