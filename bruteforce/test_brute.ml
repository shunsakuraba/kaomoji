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
  let () = prerr_endline (Remote.format_core_problem core_problem) in
  let alllist = Brute.get_candidates core_problem in
  List.iter print_candidate alllist

