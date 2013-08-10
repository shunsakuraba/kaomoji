open Remoteeval
open Remoteguess
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

exception Again

let main = 
  let _ = Random.self_init() in
  let core_problem = Remote.fetch_one_core_problem 12 "" "train" in
  let () = print_endline (Remote.format_core_problem core_problem) in
  let id, size, (unops, binops, statements) = core_problem in
  let initialguess =
    let bitseq = 
      Array.to_list
	(Array.init 64 (fun x -> Int64.shift_left 1L x)) in
    (* let fixedseq = *)
    (*   [0x0000000000000000L; *)
    (*    0x0000000000000001L; *)
    (*    0x0000000000000002L; *)
    (*    0x0000000000000003L; *)
    (*    0x0000000000000004L; *)
    (*    0x0000000000000005L; *)
    (*    0x0000000000000006L; *)
    (*    0x0000000000000007L; *)
    (*    0x0000000000000008L; *)
    (*    0x0000000000000009L; *)
    (*    0x000000000000000aL; *)
    (*    0x000000000000000bL; *)
    (*    0x00000000FFFFFFFFL; *)
    (*    0xFFFFFFFF00000000L; *)
    (*    0x0000FFFF0000FFFFL; *)
    (*    0xFFFF0000FFFF0000L; *)
    (*    0x00FF00FF00FF00FFL; *)
    (*    0xFF00FF00FF00FF00L; *)
    (*    0x0F0F0F0F0F0F0F0FL; *)
    (*    0xF0F0F0F0F0F0F0F0L; *)
    (*    0x3333333333333333L; *)
    (*    0xCCCCCCCCCCCCCCCCL; *)
    (*    0x5555555555555555L; *)
    (*    0xAAAAAAAAAAAAAAAAL; *)
    (*    0x0000FFFFFFFF0000L; *)
    (*    0xFFFF00000000FFFFL; *)
    (*    0xFF0000FFFF0000FFL; *)
    (*    0x00FFFF0000FFFF00L; *)
    (*    0xF00FF00FF00FF00FL; *)
    (*    0x0FF00FF00FF00FF0L; *)
    (*    0xDEADBEEFDEADBEEFL; *)
    (*    0xFFFFFFFFFFFFFFFFL *)
    (*   ] in *)
    let randseq = 
      Array.to_list
	(Array.init
           192
           (* (256 - (List.length bitseq) - (List.length fixedseq)) *)
	   (fun _ -> rand64 ())) in
    bitseq @ randseq in
    (* bitseq @ fixedseq @ randseq in *)
  let alllist = Brute.get_candidates core_problem in
  let _ = prerr_endline "Requesting initial guess..." in
  let start_time = Sys.time() in
  let (status, outputs, message) = Remote.eval_id id initialguess in
  if status <> Remoteeval.EvalStatusOk then
    begin
      Printf.printf "Status: \"%s\"\n" (Remoteeval.print_eval_status status);
      failwith "Eval returned error"
    end
  else
    let rec guess_function x c =
      let guess_submit () = 
	let (status, values, message, _lightning) = Remote.guess id (Print.print x) in
	print_endline message;
	match status with
	    GuessStatusWin -> Feedback.Success
	  | GuessStatusMismatch -> 
	    begin
	      match values with
		  [input; output; _youroutput] -> 
		    Feedback.Fail (input, output)
		| _ -> failwith "Should not happen: not three values"
	    end
	  | GuessStatusError -> raise Again
	  | st -> failwith "Unknown status."
      in
      try guess_submit ()
      with Again -> (Unix.sleep 20; guess_function x c)
    in
    let _ = GuessCaller.guess_call (List.combine initialguess outputs)
      guess_function
      (unops, binops, statements)
      size
      alllist
    in
    let end_time = Sys.time() in
    Printf.printf "Execution time: %fs\n" (end_time -. start_time)
