open ExtList

let solve_one_problem problem risk_mode generate_from =
  let _ = print_endline (Problem.format_problem problem 0) in
  let (id, size, ops, solved, time_left) = problem in
  let core_problem = (id, size, ops) in
  let actual_generate_from = 
    if generate_from = 0 then size else generate_from in
  let _ = prerr_endline (Printf.sprintf "risk_mode:%s" (if risk_mode then "T" else "F")) in
  let _ = prerr_endline (Printf.sprintf "generate_from:%i" actual_generate_from) in
  Solver.solve core_problem risk_mode actual_generate_from

let main =
  let generate_from = ref 0 in
  let risk_mode = ref false in

  Arg.parse
    [("-risk", Arg.Set(risk_mode), "Risk mode");
     ("-generate_from", Arg.Set_int(generate_from), "Generate start depth")]
    (fun x -> ())
    "help me";

  let _ = Random.self_init() in
  let prob_id = Sys.argv.(1) in
  let good_problems = Remote.fetch_good_problems 0 "" in
  let matched_problems = List.filter (fun p ->
    let (id, size, ops, solved, time_left) = p in
    id = prob_id) good_problems in
  let problem = List.nth matched_problems 0 in
  try
    solve_one_problem problem !risk_mode !generate_from
  with Brute.CandidateSizeLooksTooBigException ->
    prerr_endline "candidate size too big"
