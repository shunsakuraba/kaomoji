open ExtList
open Type

exception CandidateSizeLooksTooBigException

let (@@) f x = f x

let partition elms k =
  let ret = ref [] in
  let rec iter elms k cur = 
    match elms with
	0 ->
	  ret := cur :: !ret
      | 1 ->
	iter 0 k (k::cur)
      | elms ->
	for i = 1 to k - elms + 1 do
	  iter (elms - 1) (k - i) (i :: cur)
	done
  in
  if k < elms then []
  else
    (iter elms k [];
     !ret)

let op1list = 
  [Not; Shl1; Shr1; Shr4; Shr16]
let op2list = 
  [And; Or; Xor; Plus]

let unused_un_bit = function
  | Not -> 1
  | Shl1 -> 2
  | Shr1 -> 4
  | Shr4 -> 8
  | Shr16 -> 16

let unused_bin_bit = function
  | And -> 32
  | Or -> 64
  | Xor -> 128
  | Plus -> 256

let unused_stmts_bit = function
  | SIf0 -> 512
  | SFold -> 1024
  | STfold -> 2048

let needed_depth bits =
  (
    (bits land 1) +
      ((bits lsr 1) land 1) +
      ((bits lsr 2) land 1) +
      ((bits lsr 3) land 1) +
      ((bits lsr 4) land 1)
  ) * 1 +
    (
      ((bits lsr 5) land 1) +
        ((bits lsr 6) land 1) +
        ((bits lsr 7) land 1) +
        ((bits lsr 8) land 1)
    ) * 1 +
    ((bits lsr 9) land 1) * 1 +
    (((bits lsr 10) land 1) +
        ((bits lsr 11) land 1)) * 2

let is_good_binop_cand cand =
  match cand with
  | Op2 (Xor, One, One) -> false (* = Xor Zero Zero *)
  | Op2 (Xor, Input, Input) -> false (* = Xor Zero Zero *)
  | Op2 (Xor, Ident (a), Ident (b)) -> a > b (* = Xor Zero Zero *)
  | Op2 (And, Op2 (And, _, _), _) -> false
  | Op2 (And, _, Op2 (And, _, _)) -> true
  | Op2 (Or, Op2 (Or, _, _), _) -> false
  | Op2 (Or, _, Op2 (Or, _, _)) -> true
  | Op2 (Plus, Op2 (Plus, _, _), _) -> false
  | Op2 (Plus, _, Op2 (Plus, _, _)) -> true
  | Op2 (_, a, b) -> (a >= b)
  | _ -> true

let is_good_unop_cand cand =
  match cand with
  | Op1 (Shr1, Op1 (Shr4, _)) -> false
  | Op1 (Shr4, Op1 (Shr16, _)) -> false
  | Op1 (Shr1, Op1 (Shr16, _)) -> false
  | Op1 (Shr1, One) -> false  (* = Shr1 Zero *)
  | Op1 (Shr4, One) -> false  (* = Shr4 Zero *)
  | Op1 (Shr16, One) -> false  (* = Shr16 Zero *)
  | Op1 (Shr1, Op1 (Shl1, One)) -> false  (* = Shr1 (Shl1 Zero) *)
  | Op1 (Shr4, Op1 (Shl1, One)) -> false  (* = Shr4 (Shl1 Zero) *)
  | Op1 (Shr16, Op1 (Shl1, One)) -> false  (* = Shr16 (Shl1 Zero) *)
  | Op1 (Shl1, Op1 (Shr1, Zero)) -> false  (* = Shr1 (Shl1 Zero) *)
  | Op1 (Shl1, Op1 (Shr4, Zero)) -> false  (* = Shr4 (Shl1 Zero) *)
  | Op1 (Shl1, Op1 (Shr16, Zero)) -> false  (* = Shr16 (Shl1 Zero) *)
  | _ -> true

let redundant (allowed_uns, allowed_bins, allowed_stmts) = function
    | Op1 (Shr1, Zero) -> true
    | Op1 (Shr4, Zero) -> true
    | Op1 (Shr16, Zero) -> true
    | Op1 (Shr1, One) -> true
    | Op1 (Shr4, One) -> true
    | Op1 (Shr1, Op1 (Shr1, Op1 (Shr1, Op1 (Shr1, a)))) -> List.mem Shr4 allowed_uns
    | Op1 (Shr4, Op1 (Shr4, Op1 (Shr4, Op1 (Shr4, a)))) -> List.mem Shr16 allowed_uns
    | Op1 (Shr16, One) -> true
    | Op1 (Shl1, Zero) -> true
    | Op1 (Shr16, (Ident 0)) -> true
    | Op1 (Shr16, Op1 (Not, (Ident 0))) -> true  (* Shr16 Not Zero *)
    | Op1 (Shr4, Op1 (Shr4, (Ident 0))) -> true
    | Op1 (Shr4, Op2 (And, One, _)) -> true
    | Op1 (Shr4, Op2 (And, _, One)) -> true
    | Op1 (Shr16, Op2 (And, One, _)) -> true
    | Op1 (Shr16, Op2 (And, _, One)) -> true
    | Op1 (Shr4, Op1 (Shl1, One)) -> true
    | Op1 (Shr16, Op1 (Shl1, One)) -> true
    (* | Op1 (Shr16, Op1 (Shr1, _)) -> true *)
    (* | Op1 (Shr4, Op1 (Shr1, _)) -> true *)
    (* | Op1 (Shr16, Op1 (Shr4, _)) -> true *)
    | Op1 (shr, Op2 (o, One, _)) when
        (shr = Shr1 || shr = Shr4 || shr = Shr16) &&
        (o = Or || o = And) -> true
    | Op1 (shr, Op2 (o, _, One)) when
        (shr = Shr1 || shr = Shr4 || shr = Shr16) &&
        (o = Or || o = And) -> true
    | Op1 (Not, Op1 (Not, _)) -> true
    | Op2 (Plus, a, Zero) -> true
    | Op2 (Plus, Zero, a) -> true
    | Op2 (Plus, a, b) when a = b -> List.mem Shl1 allowed_uns
    | Op2 (And, _, Zero) -> true
    | Op2 (And, Zero, _) -> true
    | Op2 (And, a, b) when a = b -> true
    (* | Op2 (op1, a, Op2 (op2, b, c)) when op1 = op2 && (a > b || a > c) -> true *)
    (* | Op2 (op1, Op2 (op2, a, b), c) when op1 = op2 && (c > b || c > a) -> true *)
    | Op2 (And, Op2 (And, a, b), c) when b = c -> true
    | Op2 (And, Op2 (And, a, b), c) when a = c -> true
    | Op2 (And, a, Op2 (And, b, c)) when a = b -> true
    | Op2 (And, a, Op2 (And, b, c)) when a = c -> true
    | Op2 (And, Op1 (Not, Zero), a) -> true
    | Op2 (And, a, Op1 (Not, Zero)) -> true
    | Op2 (Or, a, b) when a = b -> true
    | Op2 (Or, Zero, a) -> true
    | Op2 (Or, Op1 (Not, Zero), a) -> true
    | Op2 (Or, a, Op1 (Not, Zero)) -> true
    | Op2 (Or, a, Zero) -> true
    | Op2 (Or, Op1 (Not, a), b) when a = b -> true
    | Op2 (Or, a, Op1 (Not, b)) when a = b -> true
    | Op2 (Or, Op2 (Or, a, b), c) when b = c -> true
    | Op2 (Or, a, Op2 (Or, b, c)) when a = b -> true
    | Op2 (Xor, a, b) when a = b -> true
    | Op2 (Xor, a, Zero) -> true
    | Op2 (Xor, Zero, a) -> true
    | Op2 (Xor, Op1 (Not, Zero), a) -> true
    | Op2 (Xor, a, Op1 (Not, Zero)) -> true
    | Op2 (Xor, Op1 (Not, a), b) when a = b -> true
    | Op2 (Xor, Op2 (Xor, a, b), c) when a = c || b = c -> true
    | Op2 (Xor, a, Op2 (Xor, b, c)) when a = b || a = c-> true
    | If0 (Zero, a, b) -> true
    | If0 (a, b, c) when a = b -> true
    | If0 (Op1 (Not, Zero), a, b) -> true
    | If0 (Op1 (Not, One), a, b) -> true
    | If0 (One, a, b) -> true
    | If0 (a, b, c) when b = c -> true
    | If0 (a, If0 (b, c, d), e) when a = b -> true
    | If0 (a, b, If0 (c, d, e)) when a = c -> true
    | _ -> false

let gen2 allowed_ops depth =
  (* let redundancy_checker _ = false in *)
  let redundancy_checker = redundant allowed_ops in
  let allowed_uns, allowed_bins, allowed_stmts = allowed_ops in

  Printf.eprintf
    "Generating(gen2) size=%d %s\n"
    depth
    (Api.format_operator_tuple (allowed_uns, allowed_bins, allowed_stmts));
  flush_all ();

  let start_time = Sys.time() in

  let fold_used_bit = 1 lsl 30 in

  let build = if List.mem STfold allowed_stmts then depth - 5 else depth - 1 in

  let groups = Array.init (build + 1) (fun _ -> []) in

  let num_ids =
    if List.mem STfold allowed_stmts then 2
    else if List.mem SFold allowed_stmts then 2
    else 0 in
  let ids = Array.to_list (Array.init num_ids (fun x -> x)) in

  Array.set
    groups
    1
    (if List.mem STfold allowed_stmts then
        (Zero, 0) :: ((One, 0) :: (List.map (fun x -> ((Ident x), 1 lsl x)) ids))
     else if List.mem SFold allowed_stmts then
        (Input, 0) :: ((Zero, 0) :: ((One, 0) :: (List.map (fun x -> ((Ident x), 1 lsl x)) ids)))
     else
        (Input, 0) :: ((Zero, 0) :: [(One, 0)]));

  for i = 2 to build do
    let target = ref (Array.get groups i) in

    if List.mem SIf0 allowed_stmts then
      List.iter
        (fun part ->
          match part with
              [d1; d2; d3] ->
                let conds = Array.get groups d1 in
                let ifcases = Array.get groups d2 in
                let elsecases = Array.get groups d3 in
                List.iter
                  (fun (cond, cond_flag) ->
                    if cond <> Zero && cond <> One then
                      List.iter
                        (fun (ifcase, ifcase_flag) ->
                          if (not ((cond = Ident(0) && (ifcase_flag land 1) = 1))) &&
                            (not ((cond = Ident(1) && (ifcase_flag land 2) = 2))) then
                          List.iter
                            (fun (elsecase, elsecase_flag) ->
                              if (not ((ifcase_flag land fold_used_bit = fold_used_bit) &&
                                          (elsecase_flag <> 0))) &&
                                (not ((elsecase_flag land fold_used_bit = fold_used_bit) &&
                                         (ifcase_flag <> 0)))
                              then
                                let new_node = If0 (cond, ifcase, elsecase) in
                                let new_flag = cond_flag lor ifcase_flag lor elsecase_flag in
                                if not (redundancy_checker new_node) then
                                  target := (new_node, new_flag) :: !target)
                            elsecases)
                        ifcases)
                  conds
            | _ -> failwith "partition bug"
        )
        (partition 3 (i - 1));

    if List.mem SFold allowed_stmts then
      List.iter
        (fun part ->
          match part with
              [d1; d2; d3] ->
                let e0s = Array.get groups d1 in
                let e1s = Array.get groups d2 in
                let e2s = Array.get groups d3 in
                List.iter
                  (fun (e0, e0_flag) ->
                    if e0_flag land fold_used_bit = 0 then
                    List.iter
                      (fun (e1, e1_flag) ->
                        if e1_flag land fold_used_bit = 0 then
                        List.iter
                          (fun (e2, e2_flag) ->
                            if e2_flag land fold_used_bit = 0 then
                              let new_node = Fold (e0, e1, 0, 1, e2) in
                              let new_flag =
                                e0_flag lor
                                  e1_flag lor
                                  (e2_flag land
                                     (lnot ((1 lsr 0) lor (1 lsl 1)))) in
                              if not (redundancy_checker new_node) then
                                target := (new_node, new_flag):: !target)
                          e2s)
                      e1s)
                  e0s
            | _ -> failwith "partition bug"
        )
        (partition 3 (i - 2));

    if List.length allowed_uns <> 0 then
      begin
        let children = Array.get groups (i - 1) in
        List.iter
          (fun (child, child_flag) ->
            List.iter
              (fun op ->
                let new_node = Op1 (op, child) in
                if not (redundancy_checker new_node) then
                  target := (new_node, child_flag):: !target)
              allowed_uns)
          children
      end;

    if List.length allowed_bins <> 0 then
      List.iter
        (fun part ->
          match part with
              [d1; d2] ->
                let lefts = Array.get groups d1 in
                let rights = Array.get groups d2 in
                List.iter
                  (fun (left, left_flag) ->
                    List.iter
                      (fun (right, right_flag) ->
                        if left <= right then
                          if (not ((left_flag land fold_used_bit = fold_used_bit) &&
                                     (right_flag <> 0))) &&
                            (not ((right_flag land fold_used_bit = fold_used_bit) &&
                                     (left_flag <> 0)))
                          then
                            List.iter
                              (fun op ->
                                if (not (left = Zero || right = Zero)) &&
                                  (not ((op <> Plus) && left = right)) &&
                                  (not (op <> Plus &&
                                          ((left = Zero || left = One) &&
                                              (right = Zero || right = One))))
                                then
                                  let new_node = Op2 (op, left, right) in
                                  if not (redundancy_checker new_node) then
                                    target := (new_node, left_flag lor right_flag) :: !target)
                              allowed_bins)
                      rights)
                  lefts
            | _ -> failwith "partition bug"
        )
        (partition 2 (i - 1));

    let new_group = !target in
    let new_group_size = List.length new_group in
    if (float_of_int new_group_size) *. (3.0 ** (float_of_int(build -
    i))) >= 100000000.0 then
      raise CandidateSizeLooksTooBigException;
    Array.set groups i new_group;
    Printf.eprintf "  depth=%d size=%d\n" i new_group_size;
    flush_all ()
  done;


  let merged = ref [] in

  for j = 1 to depth do
    if List.mem STfold allowed_stmts then
      begin
        if j > 5 then
          List.iter
            (fun (y, y_flag) ->
              if y_flag land (lnot 3) = 0 then
                merged := Fold (Input, Zero, 0, 1, y) :: !merged)
            (Array.get groups (j - 5))
      end
    else
      List.iter
        (fun (y, y_flag) ->
          if y_flag land (lnot fold_used_bit) = 0 then
            if y_flag = 0 then
              merged := y :: !merged)
        (Array.get groups (j - 1))
  done;

  let end_time = Sys.time() in
  Printf.eprintf
    "Generated(gen2) %fs size=%d\n"
    (end_time -. start_time)
    (List.length !merged);
  flush_all ();

  !merged

let gen (allowed_un, allowed_bin, allowed_stmts) depth =
  Printf.eprintf
    "Generating(gen) size=%d %s\n"
    depth
    (Api.format_operator_tuple (allowed_un, allowed_bin, allowed_stmts));
  flush_all ();

  let start_time = Sys.time() in

  let if0_in_ops = List.mem SIf0 allowed_stmts in
  let tfold_in_ops = List.mem STfold allowed_stmts in
  let rec gen input_shadowed nid unused can_use_fold depth = 
    if needed_depth unused > depth then
      begin
        (* print_endline "edagare"; *)
        []
      end
    else
    match depth with
	0 -> []
      | 1 ->
	(if input_shadowed then [Zero, 0; One, 0] else [Zero, 0; One, 0; Input, 0])
	 @ Array.to_list (Array.init nid (fun x -> Ident x, 0))
      | k ->
	(* I will regret this spaghetti *)
	(* NEWS: I did *)
	let if0cands = 
	  if if0_in_ops 
	  then
	    let if0cands = 
	      let partitions = partition 3 (k - 1) in
	      let cands = ref [] in
              let new_unused = unused land (lnot (unused_stmts_bit SIf0)) in
	      List.iter (fun part ->
		match part with
		    [d1; d2; d3] ->
	              List.iter
                        (fun (x, x_used) ->
			  if x <> One then  (* same as if0 Zero *)
	                  List.iter
                            (fun (y, y_used) ->
		              List.iter 
		                (fun (z, z_used) -> cands := (If0 (x, y, z), x_used lor y_used lor z_used lor (unused_stmts_bit SIf0)) :: !cands)
		                (gen input_shadowed nid (new_unused land (lnot x_used) land (lnot y_used)) can_use_fold d3))
		            (gen input_shadowed nid 0 can_use_fold d2))
		        (gen input_shadowed nid 0 can_use_fold d1)
		  | _ -> failwith "partition bugged")
		partitions;
	      !cands
	    in
	    if0cands
	  else [] in
	let foldcands =
	  if can_use_fold then
	    let foldcands = 
	      let partitions = partition 3 (k - 2) in
	      let cands = ref [] in
              let new_unused = unused land (lnot (unused_stmts_bit SFold)) in
	      List.iter (fun part ->
		match part with
		    [d1; d2; d3] ->
		        List.iter
                          (fun (x, x_used) ->
			      List.iter
                                (fun (y, y_used) ->
			          List.iter
                                    (fun (z, z_used) -> cands := (Fold (x, y, nid, nid + 1, z), x_used lor y_used lor z_used lor (unused_stmts_bit SFold)) :: !cands)
			            (gen input_shadowed (nid + 2) (new_unused land (lnot x_used) land (lnot y_used)) false d3))
			        (gen input_shadowed nid 0 false d2))
			  (gen input_shadowed nid 0 false d1)
		  | _ -> failwith "partition bugged")
		partitions;
	      !cands in
	    foldcands
	  else [] in
	let unopcands = 
	  let cands = ref [] in
	  List.iter (fun op ->
	    List.iter (fun (x, x_used) ->
              let cand_exp = Op1 (op, x) in
              if (is_good_unop_cand cand_exp) then
		cands := (cand_exp, x_used lor (unused_un_bit op))  :: !cands)
	      (gen input_shadowed nid (unused land (lnot (unused_un_bit op))) can_use_fold (depth - 1)))
	    allowed_un;
	  !cands in
	let binopcands = 
	  let partitions = partition 2 (k - 1) in
	  let cands = ref [] in
	  List.iter (fun part ->
	    match part with
		[d1; d2] ->
		  List.iter
                    (fun (x, x_used) ->
		      List.iter
                        (fun (y, y_used) ->
		          List.iter
			    (fun op -> 
			      let cand = Op2 (op, x, y) in
			      if is_good_binop_cand cand then
				cands := (cand, x_used lor y_used lor (unused_bin_bit op)) :: !cands)
                            allowed_bin)
                        (gen input_shadowed nid 0 can_use_fold d2))
	            (gen input_shadowed nid 0 can_use_fold d1)
		  (* List.iter *)
                  (*   (fun op -> *)
                  (*     let new_unused = unused land (lnot (unused_bin_bit op)) in *)
		  (*       List.iter *)
                  (*         (fun (x, x_used) -> *)
		  (*           List.iter *)
                  (*             (fun (y, y_used) -> cands := (Op2 (op, x, y), x_used lor y_used lor (unused_bin_bit op)) :: !cands) *)
		  (*             (gen input_shadowed nid (new_unused land (lnot x_used)) d2)) *)
		  (*         (gen input_shadowed nid 0 d1)) *)
                  (*   allowed_bin *)
	      | _ -> failwith "partition bugged")
	    partitions;
	  !cands
	in
	let ( @++ ) = List.rev_append in 
	if0cands @++ foldcands @++ unopcands @++ binopcands
  in
  let unused =
    (List.fold_left
       (fun x y -> x lor unused_un_bit y)
       (List.fold_left
          (fun x y -> x lor unused_bin_bit y)
          ((List.fold_left
             (fun x y -> x lor unused_stmts_bit y)
             0
             allowed_stmts) land (lnot (unused_stmts_bit STfold)))
           allowed_bin)
       allowed_un) in
  let res =
    if tfold_in_ops then
      let inner = gen true 2 unused false (depth - 5) in (* -5 = -1 (lambda), -2 (fold / lambda), -1 (Input), -1 (Zero) *)
      List.map
        (fun (x, x_used) -> Fold (Input, Zero, 0, 1, x))
        inner
    else
      let fold_in_ops = List.mem SFold allowed_stmts in
      List.map (fun (x, x_used) -> x) (gen false 0 unused fold_in_ops (depth - 1) (* -1 is from the big "lambda" of outside *)) in
  let end_time = Sys.time() in
    Printf.eprintf
      "Generated(gen) %fs size=%d\n"
      (end_time -. start_time)
      (List.length res);
  res

let list_to_unique_list list =
  let rec list_to_unique_list list m dst =
    match list with
    | [] -> dst
    | (hd::tl) ->
      if PMap.mem hd m then
	list_to_unique_list tl m dst
      else
	list_to_unique_list tl (PMap.add hd true m) (hd::dst)
  in
  list_to_unique_list list PMap.empty []

let get_candidates core_problem =
  let id, size, (unops, binops, statements) = core_problem in
  let cand_gen_start_time = Sys.time() in
  let allowed = (unops, binops, statements) in
  let alllist_initial = gen2 allowed size in
  let () = prerr_endline (Printf.sprintf "Initialized candidate list (%d elements)"
			    (List.length alllist_initial)) in
  let simplified = List.map Simplifier.simplify alllist_initial in
  let () = prerr_endline "Simplification finished." in
  let alllist = list_to_unique_list simplified in
  (* let alllist = alllist_initial in *)
  let num_candidates = List.length alllist in
  let () = prerr_endline (Printf.sprintf "Compressed candidate list (%d elements)" num_candidates) in
  let start_time = Sys.time() in
  let cand_gen_time = start_time -. cand_gen_start_time in
  let _ = prerr_endline (Printf.sprintf "Candidate generation time:
  %fs" cand_gen_time) in
  alllist
