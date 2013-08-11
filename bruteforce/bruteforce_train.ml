let main =
  let size = ref 10 in
  let operators = ref "" in
  let mode = ref "train" in
  let generate_from = ref 0 in

  Arg.parse
    [("-size", Arg.Set_int(size), "Size");
     ("-operators", Arg.Set_string(operators), "Operators");
     ("-mode", Arg.Set_string(mode), "Mode");
     ("-generate_from", Arg.Set_int(generate_from), "Generate start depth")]
    (fun x -> ())
    "help me";

  if !generate_from = 0 then
    generate_from := !size;

  let _ = Random.self_init() in
  let core_problem = Remote.fetch_one_core_problem !size !operators !mode in
  let () = print_endline (Remote.format_core_problem core_problem) in
  Solver.solve core_problem false !generate_from
