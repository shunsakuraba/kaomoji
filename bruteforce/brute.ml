open ExtList
open Type

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

let gen2 (allowed_uns, allowed_bins, allowed_stmts) depth =
  Printf.eprintf
    "Generating size=%d %s"
    depth
    (Api.format_operator_tuple (allowed_uns, allowed_bins, allowed_stmts));

  let fold_used_bit = 1 lsl 30 in

  let groups = Array.init (depth + 1) (fun _ -> []) in

  let num_ids =
    if List.mem STfold allowed_stmts then 2
    else if List.mem SFold allowed_stmts then 2
    else 0 in
  let ids = Array.to_list (Array.init num_ids (fun x -> x)) in

  Array.set
    groups
    1
    ((Input, 0) :: ((Zero, 0) :: ((One, 0) :: (List.map (fun x -> ((Ident x), 1 lsl x)) ids))));

  for i = 2 to depth do
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
                    List.iter
                      (fun (ifcase, ifcase_flag) ->
                        List.iter
                          (fun (elsecase, elsecase_flag) ->
                            let new_node = If0 (cond, ifcase, elsecase) in
                            let new_flag = cond_flag lor ifcase_flag lor elsecase_flag in
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
                            List.iter
                              (fun left ->
                                List.iter
                                  (fun right ->
                                    if left <> right then
                                      let new_node = Fold (e0, e1, left, right, e2) in
                                      let new_flag =
                                        e0_flag lor
                                          e1_flag lor
                                          (e2_flag land
                                             (lnot ((1 lsr right) lor (1 lsl left)))) in
                                      target := (new_node, new_flag):: !target)
                                  ids)
                              ids)
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
                target := (Op1 (op, child), child_flag):: !target)
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
                          List.iter
                            (fun op ->
                              target := (Op2 (op, left, right), left_flag lor right_flag) :: !target)
                            allowed_bins)
                      rights)
                  lefts
            | _ -> failwith "partition bug"
        )
        (partition 2 (i - 1));

    Array.set groups i !target
  done;

  prerr_endline "Before filtering";
  Array.iteri
    (fun i x -> Printf.eprintf "  depth=%d size=%d\n" i (List.length x))
    groups;

  let merged = ref [] in

  if depth > 5 && List.mem STfold allowed_stmts then
    List.iter
      (fun (y, y_flag) ->
        if y_flag land (lnot 3) = 0 then
          try
            (* let _ = Eval.eval y 0L in *)
            merged := Fold (Input, Zero, 0, 1, y) :: !merged
          with Not_found ->
            ())
      (Array.get groups (depth - 5));

  List.iter
    (fun (y, y_flag) ->
      if y_flag land (lnot fold_used_bit) = 0 then
        if y_flag = 0 then
          try
            (* let _ = Eval.eval y 0L in *)
            merged := y :: !merged
          with Not_found ->
            ())
    (Array.get groups (depth - 1));
  !merged

let gen (allowed_un, allowed_bin, allowed_stmts) depth =
  prerr_endline "Generating";
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
  if tfold_in_ops then
    let inner = gen true 2 unused false (depth - 5) in (* -5 = -1 (lambda), -2 (fold / lambda), -1 (Input), -1 (Zero) *)
    List.map
      (fun (x, x_used) -> Fold (Input, Zero, 0, 1, x))
      inner
  else
    let fold_in_ops = List.mem SFold allowed_stmts in
    List.map (fun (x, x_used) -> x) (gen false 0 unused fold_in_ops (depth - 1) (* -1 is from the big "lambda" of outside *))
