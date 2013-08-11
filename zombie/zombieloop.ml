open Remoteguess
open Util

let automode = ref false
let preferredsize = ref 12
let probtype = ref "-fold"
let servermode = ref "train"

let argspec = 
  ["-auto", Arg.Set automode, " Auto-mode (for accept_isk in solver.ml)";
   "-size", Arg.Set_int preferredsize, "[n]  Preferred problem size";
   "-operators", Arg.Set_string probtype, "[str]  Preferred problem type (tfold, bonus or -fold)";
   "-mode", Arg.Set_string servermode, "[str]  \"train\" or \"real\"";
  ]

let final_timeout = 300.0

let solver_place is_tfold = 
  let argv0 = Sys.argv.(0) in
  Filename.concat (Filename.dirname argv0)
    (if is_tfold then "solver_tfold.py" else "solver.py")

let opcodemap = [| "not"; "shl1"; "shr1"; "shr4"; "shr16";
		   "and"; "or"; "xor"; "plus";
		   "if0" |] 
let nopcodes = Array.length opcodemap

let cost nop is_tfold = 
  let costarr = [| 1; 1; 1; 1; 1;
		   2; 2; 2; 2;
		   3 |] in
  let ret = ref (if is_tfold then 3 else 1) in
  Array.iteri (fun i x -> ret := !ret + x * costarr.(i) ) nop;
  !ret

let print_num_operators ar = 
  let () = assert(Array.length ar == nopcodes) in
  let b = Buffer.create 16 in
  for i = 0 to nopcodes - 1 do
    Buffer.add_string b (Printf.sprintf "%s %d\n" opcodemap.(i) ar.(i))
  done;
  Buffer.contents b

(* Following code comes from unix.ml of OCaml *)
let try_set_close_on_exec fd =
  try Unix.set_close_on_exec fd; true with Invalid_argument _ -> false

let open_proc cmd input output toclose =
  let cloexec = List.for_all try_set_close_on_exec toclose in
  match Unix.fork() with
     0 -> if input <> Unix.stdin then begin Unix.dup2 input Unix.stdin; Unix.close input end;
          if output <> Unix.stdout then begin Unix.dup2 output Unix.stdout; Unix.close output end;
          if not cloexec then List.iter Unix.close toclose;
          begin try Unix.execv "/usr/bin/python" [| "/usr/bin/python"; cmd |]
            with _ -> exit 127
          end
  | id -> id

let rec waitpid_non_intr pid =
  try Unix.waitpid [] pid
  with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_non_intr pid

let open_process cmd =
  let (in_read, in_write) = Unix.pipe() in
  let fds_to_close = ref [in_read;in_write] in
  try
    let (out_read, out_write) = Unix.pipe() in
    fds_to_close := [in_read;in_write;out_read;out_write];
    let inchan = Unix.in_channel_of_descr in_read in
    let outchan = Unix.out_channel_of_descr out_write in
    let pid = 
      open_proc cmd out_read in_write
        [in_read; out_write] in
    Unix.close out_read;
    Unix.close in_write;
    (inchan, outchan, pid)
  with e ->
    List.iter Unix.close !fds_to_close;
    raise e

let close_process (inchan, outchan) pid =
  close_in inchan;
  begin try close_out outchan with Sys_error _ -> () end;
  snd(waitpid_non_intr pid)

let open_solver size nops is_tfold =
  let solver = solver_place is_tfold in
  let (ich, och, pid) = open_process (solver) in
  let () = Printf.eprintf "Started solver (pid = %d)" pid in
  let () = Printf.fprintf och "%d\n" size in
  let () =
    Array.iteri
      (fun i x -> Printf.fprintf och "%s %d\n" opcodemap.(i) x)
      nops in
  let () = flush och in
  let _ = input_line ich in
  (ich, och, pid)

let send_solver_oracle ch iolist =
  let () = Printf.fprintf ch "%d\n" (List.length iolist) in
  List.iter
    (fun (i, o) ->
      let () = Printf.eprintf "Sending: 0x%016LX 0x%016LX\n" i o in
      let () = flush stdout in
      let () = Printf.fprintf ch "0x%016LX 0x%016LX\n" i o in
      let () = flush ch in
      ()) iolist

let read_stmt ich och pid timeout is_tfold = 
  let (selin, selout, selerr) = Unix.select [Unix.descr_of_in_channel ich] [] [] timeout in
  if selin = [] 
  then
    (* Forced timeout *)
    let () = Printf.eprintf "Forced timeout (pid = %d)\n" pid in
    let () = Unix.kill pid Sys.sigkill in
    None
  else 
    let n = int_of_string (input_line ich) in
    if n = 0 
    then None
    else 
      let stmts = Array.init n 
	(fun _i ->
	  let l = input_line ich in
	  Scanf.sscanf l "%d %s %s" 
	    (fun _x y z -> 
	      (y, if z = "_" then [] else ExtString.String.nsplit z ","))) in
      let open Type in
      let rec iter v = 
	match stmts.(v) with
	    ("0", []) -> Zero
	  | ("1", []) -> One
	  | (op1, [x]) -> 
	    let opcode = match op1 with
		"not" -> Not
	      | "shl1" -> Shl1
	      | "shr1" -> Shr1
	      | "shr4" -> Shr4
	      | "shr16" -> Shr16
	      | _ -> failwith "Opcode: op1"
	    in
	    Op1 (opcode, convert_to_ref x)
	  | (op2, [x; y]) ->
	    let opcode = match op2 with
		"and" -> And
	      | "or" -> Or
	      | "xor" -> Xor
	      | "plus" -> Plus
	      | _ -> failwith "Opcode: op2"
	    in
	    Op2 (opcode, convert_to_ref x, convert_to_ref y)
	  | ("if0", [x;y;z]) -> 
	    If0 (convert_to_ref x,
		 convert_to_ref y,
		 convert_to_ref z)
	  | _ ->
	    failwith "Unknown statement pattern"
      and convert_to_ref str = 
	match str with
	    "0" -> Type.Zero
	  | "1" -> Type.One
	  | "input" -> Type.Input
	  | "byte" -> Type.Ident 0
	  | "accum" -> Type.Ident 1
	  | _ -> 
	    let id = Scanf.sscanf str "x%d" (fun x -> x) in
	    iter id
      in
      if is_tfold 
      then Some (Fold (Input, Zero, 0, 1, (iter (n - 1))))
      else Some (iter (n - 1))
	
type feedback = 
    Success
  | Fail of instance
and instance = Int64.t * Int64.t

let rec remote_guess_wrap id tree = 
  let (status, values, message, _lightning) = Remote.guess id (Print.print tree) in
  match status with
      GuessStatusWin -> 
	Success
    | GuessStatusMismatch ->
      begin 
	match values with
	    [input; output; _youroutput] -> 
	      Fail (input, output)
	  | _ -> failwith "Should not happen: not three values"
      end
    | GuessStatusError -> 
      (Unix.sleep 20; remote_guess_wrap id tree)
    | _ -> failwith "Unknown Status"

let core_loop id size unops binops statements initial_candidates = 
  let is_tfold = List.mem Type.STfold statements in
  let has_fold = List.mem Type.Sfold statements  in
  let () = if has_fold then failwith "fold is unsupported " else () in
  let t0 = Unix.gettimeofday () in
  let oracles = ref [List.hd initial_candidates] in
  let evals = ref initial_candidates in
  let init_nops = 
    let nops = Array.make nopcodes 0 in
    (* Initialize nops with only 1 ops on used operations *)
    let () = 
      Array.iteri 
	(fun i x -> 
	  if
	    List.mem x 
	      ((List.map Type.unop_to_string unops) @
		  (List.map Type.binop_to_string binops) @
		  (List.map Type.statement_to_string statements) )
	  then nops.(i) <- 1)
	opcodemap in
    nops
  in
  let nopsqueue = 
    let q = Queue.create () in
    let () = Queue.push init_nops q in
    q
  in
  let visited = Hashtbl.create 10 in
  let tout = ref 15.0 in
  let rec nop_change_loop () = 
    let nops = Queue.pop nopsqueue in
    let () = tout := !tout +. 5.0 in
    let () = 
      if Unix.gettimeofday () > t0 +. final_timeout
      then failwith "Solver timed out"
      else ()
    in
    if cost nops is_tfold > size || Hashtbl.mem visited nops 
    then ()
    else
      let () = Hashtbl.replace visited nops true in
      let (ich, och, pid) = open_solver size nops is_tfold in
      let () = send_solver_oracle och !oracles in
      let rec find_solution_loop () =
	match read_stmt ich och pid !tout is_tfold with
	    Some tree ->
	      (* found solution. Try with local mismatch lists *)
	      let () = Printf.eprintf "%s\n" (Print.print tree) in
	      let mismatch = 
		List.filter (fun (ix, ox) -> (Eval.eval tree ix) <> ox) !evals in
	      begin
		match mismatch with
		    ((i, o) as x) :: _ ->
		    (* there still be a mismatched solution *)
		      let () = oracles := x :: !oracles in
		      let () = send_solver_oracle och [x] in
		      find_solution_loop ()
		  | [] ->
		  (* no local mismatched solution. get remote one *)
		    begin
		      match remote_guess_wrap id tree with
			  Success -> (* Oh, you won! *)
			    let () = Printf.fprintf och "Q\n" in
			    let () = flush och in
			    let _ = close_process (ich, och) pid in
			    raise Exit
			| Fail (i, o) ->
			  let () = oracles := (i, o) :: !oracles in
			  let () = evals := (i, o) :: !evals in
			  let () = send_solver_oracle och [i, o] in
			  find_solution_loop ()
		    end
	      end
	  | None -> (* no solution == bad model *)
	  (* expand model and iterate *)
	    Array.iteri
	      (fun i x ->
		if x > 0
		then
		  let newnops = Array.copy nops in
		  newnops.(i) <- x + 1;
		  Queue.push newnops nopsqueue 
		else
		  ())
	      nops;
	    let () = Printf.fprintf och "Q\n" in
	    let () = flush och in
	    let _ = close_process (ich, och) pid in
	    nop_change_loop ()
      in
      find_solution_loop ()
  in
  try 
    nop_change_loop ()
  with
      Exit -> 
	let () = Printf.eprintf "successfully found solution!\n" in
	()

let normal_boot () = 
  let _ = Random.self_init() in
  let nsize = !preferredsize in
  let cond = !probtype in
  let core_problem = Remote.fetch_one_core_problem nsize cond !servermode in
  let () = print_endline (Remote.format_core_problem core_problem) in
  let id, size, (unops, binops, statements) = core_problem in
  let initialguess =
    let bitseq = 
      Array.to_list
	(Array.init 64 (fun x -> Int64.shift_left 1L x)) in
    let artificial_seq = 
      [ 0x0000_0000_0000_0000L;
	0x0000_0000_0000_0003L;
	0x0000_0000_0000_000FL;
	0x0000_0000_0000_00FFL;
	0x0000_0000_0000_FFFFL;
	0x0000_0000_FFFF_FFFFL;
	0xFFFF_FFFF_FFFF_FFFFL;
	0x5555_5555_5555_5555L;
	0xAAAA_AAAA_AAAA_AAAAL; ] in
    let randseq = 
      Array.to_list
	(Array.init (256 - (List.length bitseq) - (List.length artificial_seq))
	   (fun _ -> rand64 ())) in
    bitseq @ artificial_seq @ randseq in
  let _allowed = (unops, binops, statements) in
  let (status, outputs, message) = Remote.eval_id id initialguess in
  let () = 
    if status <> Remoteeval.EvalStatusOk then (* FIXME TODO *)
      begin
	Printf.eprintf "Status: \"%s\"\n" (Remoteeval.print_eval_status status);
	failwith "Eval returned error"
      end
    else () in
  let initial_candidates = (List.combine initialguess outputs) in
  core_loop id size unops binops statements initial_candidates

let auto_boot () = 
  let open ExtString in
  let id = read_line () in
  let size = int_of_string (read_line ()) in
  let unops = 
    List.map Type.unop_of_string (String.nsplit (read_line ()) " ") in
  let binops = 
    List.map Type.binop_of_string (String.nsplit (read_line ()) " ") in
  let statements = 
    List.map Type.statement_of_string (String.nsplit (read_line ()) " ") in
  let initialguess = 
    let strs = String.nsplit (read_line ()) " " in
    List.map (fun s -> Scanf.sscanf s "0x%LX" (fun x -> x)) strs in
  let initialoutputs = 
    let strs = String.nsplit (read_line ()) " " in
    List.map (fun s -> Scanf.sscanf s "0x%LX" (fun x -> x)) strs in
  let initial_candidates = (List.combine initialguess initialoutputs) in
  core_loop id size unops binops statements initial_candidates

let main = 
  let () = Arg.parse argspec (fun _ -> failwith "") "" in
  if !automode then
    auto_boot ()
  else 
    normal_boot()
    
    
  
  

