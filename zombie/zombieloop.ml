open Remoteguess
open Util

let solver_place () = 
  let argv0 = Sys.argv.(0) in
  Filename.concat (Filename.dirname argv0) "solver.py"

let open_solver size unops binops statements =
  let solver = solver_place () in
  let (ich, och) = Unix.open_process ("python " ^ solver) in
  let () = Printf.fprintf och "%d\n" size in
  let () = Printf.fprintf och "%s\n" (String.concat " " (List.map Type.unop_to_string unops)) in
  let () = Printf.fprintf och "%s\n" (String.concat " " (List.map Type.binop_to_string binops)) in
  let () = Printf.fprintf och "%s\n" (String.concat " " (List.map Type.statement_to_string statements)) in
  let () = flush och in
  let _ = input_line ich in
  (ich, och)

let read_stmt ich = 
  let n = int_of_string (input_line ich) in
  let stmts = Array.init n 
    (fun _i ->
      let l = input_line ich in
      Scanf.sscanf l "%d %s %s" 
	(fun _x -> fun y -> fun z -> 
	  (y, ExtString.String.nsplit z ","))) in
  let open Type in
  let rec iter v = 
    match stmts.(v) with
	(op1, [x]) -> 
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
      | _ -> 
	let id = Scanf.sscanf str "x%d" (fun x -> x) in
	iter id
  in
  iter (n - 1)

let main = 
  let _ = Random.self_init() in
  let core_problem = Remote.fetch_one_core_problem 13 "-tfold" "train" in
  let () = print_endline (Remote.format_core_problem core_problem) in
  let id, size, (unops, binops, statements) = core_problem in
  let (ich, och) = open_solver size unops binops statements in
  let initialguess =
    let bitseq = 
      Array.to_list
	(Array.init 64 (fun x -> Int64.shift_left 1L x)) in
    let randseq = 
      Array.to_list
	(Array.init 192
	   (fun _ -> rand64 ())) in
    bitseq @ randseq in
  let _allowed = (unops, binops, statements) in
  let (status, outputs, message) = Remote.eval_id id initialguess in
  let () = 
    if status <> Remoteeval.EvalStatusOk then (* FIXME TODO *)
      begin
	Printf.printf "Status: \"%s\"\n" (Remoteeval.print_eval_status status);
	failwith "Eval returned error"
      end
    else () in
  let candidates = ref (List.combine initialguess outputs) in
  let tree = ref Type.Input in
  let rec initial_filter_loop () =
    match !candidates with
	(i, o) :: xs ->
	  let () = Printf.printf "Sending: 0x%016LX 0x%016LX\n" i o in
	  let () = flush stdout in
	  let () = Printf.fprintf och "0x%016LX 0x%016LX\n" i o in
	  let () = flush och in
	  let () = tree := read_stmt ich in
	  let () = Printf.printf "Current syntax tree: %s\n" (Print.print !tree) in
	  let () = 
	    candidates := 
	      List.filter (fun (ix, ox) -> (Eval.eval !tree ix) <> ox) xs in
	  initial_filter_loop ()
      | [] -> 
	!tree
  in
  let tree = initial_filter_loop () in
  let rec guess_loop tree = 
    let (status, values, message, _lightning) = Remote.guess id (Print.print tree) in
    match status with
	GuessStatusWin -> let () = 
			    Printf.fprintf och "Q\n";
			    flush och;
			    let _ = Unix.close_process (ich, och) in
			    ()
			  in
			  print_endline (Print.print tree)
      | GuessStatusMismatch ->
	begin 
	  match values with
	      [input; output; _youroutput] -> 
		let () = Printf.fprintf och "0x%016LX 0x%016LX\n" input output in
		let () = flush och in
		guess_loop (read_stmt ich)
	    | _ -> failwith "Should not happen: not three values"
	end
      | GuessStatusError -> 
	(Unix.sleep 20; guess_loop tree)
      | _ -> failwith "Unknown Status"
  in
  guess_loop tree


