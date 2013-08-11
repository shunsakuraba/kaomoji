open Remoteguess
exception Again

let get_initialguess () =
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
	 (fun _ -> Util.rand64 ())) in
  bitseq @ randseq
(* bitseq @ fixedseq @ randseq in *)

let rec solve_internal accept_risk id size (unops, binops, statements) alllist db existing_inputs existing_outputs =
  if (List.length alllist) > 100000000 then
    prerr_endline "Abandoned: too many candidates."
  else
  let _ = prerr_endline "Requesting initial guess..." in
  let start_time = Sys.time() in
  let (status, outputs, message), initialguess =
    if List.length existing_inputs = 0 then
      let initialguess = get_initialguess () in
      Remote.eval_id id initialguess, initialguess
    else
      (Remoteeval.EvalStatusOk, existing_outputs, "retry dayo-"), existing_inputs
  in
  if status <> Remoteeval.EvalStatusOk then
    begin
      Printf.printf "Status: \"%s\"\n" (Remoteeval.print_eval_status status);
      failwith "Eval returned error"
    end
  else
    begin
      if accept_risk then
        begin
	  let solver_place = 
	    let argv0 = Sys.argv.(0) in
	    Filename.concat (Filename.dirname argv0)
	      "../zombie/zombieloop -auto"
	  in
          let child_in, child_out = Unix.open_process solver_place in
          output_string
	    child_out
	    (Printf.sprintf
               "%s\n%d\n%s\n%s\n%s\n%s\n%s\n"
               id
               size
               (String.concat " " (List.map Type.unop_to_string unops))
               (String.concat " " (List.map Type.binop_to_string binops))
               (String.concat " " (List.map Type.statement_to_string statements))
               (String.concat " " (List.map (Printf.sprintf "0x%016Lx") initialguess))
               (String.concat " " (List.map (Printf.sprintf "0x%016Lx") outputs))
	    );
          flush child_out
        end;

      prerr_endline
        (Printf.sprintf
           "Case OCaml dump:\n(\"%s\",\n%d,\n([%s], [%s], [%s]),\n[%s],\n[%s])\n"
           id
           size
           (String.concat "; " (List.map Type.unop_to_cstring unops))
           (String.concat "; " (List.map Type.binop_to_cstring binops))
           (String.concat "; " (List.map Type.statement_to_cstring statements))
           (String.concat "; " (List.map (Printf.sprintf "0x%016LxL") initialguess))
           (String.concat "; " (List.map (Printf.sprintf "0x%016LxL") outputs)));

      let rec guess_function x c =
        Printf.eprintf
          "Guessed: %s\n"
          (Print.print x);
        let guess_submit () = 
	  let (status, values, message, _lightning) = Remote.guess id (Print.print x) in
	  print_endline message;
	  match status with
	      GuessStatusWin -> Feedback.Success
	  | GuessStatusMismatch -> 
	    begin
	      match values with
		  [input; output; _youroutput] -> 
                    (* output_string child_out (Printf.sprintf "0x%016Lx 0x%016Lx\n" input output); *)
                    (* flush child_out; *)
		    Feedback.Fail (input, output)
		| _ -> failwith "Should not happen: not three values"
	    end
	  | GuessStatusError -> raise Again
	  | st -> failwith "Unknown status."
        in
        try guess_submit ()
        with Again -> (Unix.sleep 20; guess_function x c)
      in
      try
        let _ = GuessCaller.guess_call (List.combine initialguess outputs)
          guess_function
          (unops, binops, statements)
          size
          alllist
        in
        let end_time = Sys.time() in
        Printf.eprintf "Execution time: %fs\n" (end_time -. start_time)
      with Not_found ->
        begin
          let _ = Brute.expand_db (unops, binops, statements) db 10 in
          let alllist = Brute.generate_candidates_from_db (unops, binops, statements) db in
          let alllist = Brute.cleanup_candidates alllist in
          solve_internal accept_risk id size (unops, binops, statements) alllist db initialguess outputs
        end
    end

let solve core_problem accept_risk generate_from =
  let id, size, (unops, binops, statements) = core_problem in
  let alllist, db = Brute.get_candidates core_problem generate_from in
  solve_internal accept_risk id size (unops, binops, statements) alllist db [] []
