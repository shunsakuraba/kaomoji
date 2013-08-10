open ExtList

let print_candidate candidate =
  if true then
    print_endline (Print.print candidate)
  else
    ()

let main = 
  (* AcSmrDU445Uu5ewnrBZ3rn9N*)
  (* Yj5nGIfonf3VCIiAn2pEARdw *)
  let core_problem = Remote.fetch_one_core_problem 8 "" "train_local" in
  let () = print_endline (Remote.format_core_problem core_problem) in
  let id, size, (unops, binops, statements) = core_problem in
  let cand_gen_start_time = Sys.time() in
  let allowed = (unops, binops, statements) in
  let alllist_initial = Brute.gen allowed size in
  let alllist = List.unique (List.map Simplifier.simplify alllist_initial) in
  let _ = List.iter print_candidate alllist in
  let num_candidates = List.length alllist in
  let () = Printf.printf "Initialized candidate list (%d elements)\n"
  (List.length alllist_initial) in
  let () = Printf.printf "Compressed candidate list (%d elements)\n" num_candidates in
  let start_time = Sys.time() in
  let cand_gen_time = start_time -. cand_gen_start_time in
  Printf.printf "Candidate generation time: %fs\n" cand_gen_time
