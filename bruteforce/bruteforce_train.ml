let main =
  let size = ref 10 in
  let operators = ref "" in
  let mode = ref "train" in

  Arg.parse
    [("-size", Arg.Set_int(size), "Size");
     ("-operators", Arg.Set_string(operators), "Operators");
     ("-mode", Arg.Set_string(mode), "Mode")]
    (fun x -> ())
    "help me";

  let _ = Random.self_init() in
  let core_problem = Remote.fetch_one_core_problem !size !operators !mode in
  let () = print_endline (Remote.format_core_problem core_problem) in
  Solver.solve core_problem false

