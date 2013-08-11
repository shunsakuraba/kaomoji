open Remoteeval

open Util
open ExtList

let main =
  if false then
    let bitseq = 
      Array.to_list
	(Array.init 64 (fun x -> Int64.shift_left 1L x)) in
    let syntax = 
      let open Type in
	  Fold (Input, Zero, 0, 1, Op2 (And, Ident 0, Op1 (Not, Ident 1))) in
      let outputs = List.map (Eval.eval syntax) bitseq in
      let merge = List.combine bitseq outputs in
      List.iter
	(fun (i,o) -> Printf.printf "%LX => %LX\n" i o)
	merge
  else
    ()

let main =
  let _ = Random.self_init() in
  let core_problem = Remote.fetch_one_core_problem 10 "" "train" in
  let () = print_endline (Remote.format_core_problem core_problem) in
  Solver.solve core_problem
