open Remoteeval
open Remoteguess
open Util

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
  let core_problem = Remote.fetch_one_core_problem 3 "" "real" in
  let () = print_endline (Remote.format_core_problem core_problem) in
  let id, size, (unops, binops, statements) = core_problem in
  let initialguess = 
    let bitseq = 
      Array.to_list
	(Array.init 64 (fun x -> Int64.shift_left 1L x)) in
    let randseq = 
      Array.to_list
	(Array.init 192
	   (fun _ -> rand64 ())) in
    bitseq @ randseq in
  let (status, outputs, message) = Remote.eval_id id initialguess in
  if status <> Remoteeval.EvalStatusOk then
    begin
      Printf.printf "Status: \"%s\"\n" (Remoteeval.print_eval_status status);
      failwith "Eval returned error"
    end
  else
    let rec guess_function x =
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
      with Again -> (Unix.sleep 20; guess_function x)
    in
    GuessCaller.guess_call (List.combine initialguess outputs)
      guess_function
      (unops, binops, statements)
      size
