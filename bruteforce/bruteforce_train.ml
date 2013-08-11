let main =
  let _ = Random.self_init() in
  let core_problem = Remote.fetch_one_core_problem 10 "" "train" in
  let () = print_endline (Remote.format_core_problem core_problem) in
  Solver.solve core_problem false

