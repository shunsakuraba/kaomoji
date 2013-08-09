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

let gen (allowed_un, allowed_bin, allowed_stmts) depth =
  let if0_in_ops = List.mem SIf0 allowed_stmts in
  let fold_in_ops = List.mem SFold allowed_stmts in
  let tfold_in_ops = List.mem STfold allowed_stmts in
  let rec gen input_shadowed nid unused depth = 
    if needed_depth unused > depth then
      begin
        (* print_endline "edagare"; *)
        []
      end
    else
    match depth with
	0 -> []
      | 1 ->
	(if input_shadowed then [Zero; One] else [Zero; One; Input])
	@ Array.to_list (Array.init nid (fun x -> Ident x))
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
                      for i = 0 to new_unused do
		        List.iter
                          (fun x ->
                            for j = 0 to new_unused - i do
			      List.iter
                                (fun y ->
			          List.iter 
			            (fun z -> cands := If0 (x, y, z) :: !cands)
			            (gen input_shadowed nid (new_unused - i - j) d3))
			        (gen input_shadowed nid j d2)
                            done)
			  (gen input_shadowed nid i d1)
                      done
		  | _ -> failwith "partition bugged")
		partitions;
	      !cands
	    in
	    if0cands
	  else [] in
	let foldcands =
	  if fold_in_ops
	  then
	    let foldcands = 
	      let partitions = partition 3 (k - 2) in
	      let cands = ref [] in
              let new_unused = unused land (lnot (unused_stmts_bit SFold)) in
	      List.iter (fun part ->
		match part with
		    [d1; d2; d3] ->
                      for i = 0 to new_unused do
		        List.iter
                          (fun x ->
                            for j = 0 to new_unused - i do
			      List.iter
                                (fun y ->
			          List.iter
                                    (fun z -> cands := Fold (x, y, nid, nid + 1, z) :: !cands)
			            (gen input_shadowed (nid + 2) (new_unused - i - j) d3))
			        (gen input_shadowed nid j d2)
                            done)
			  (gen input_shadowed nid i d1)
                      done
		  | _ -> failwith "partition bugged")
		partitions;
	      !cands in
	    foldcands
	  else [] in
	let unopcands = 
	  let cands = ref [] in
	  List.iter (fun op ->
	    List.iter (fun x ->
	      cands := Op1 (op, x) :: !cands)
	      (gen input_shadowed nid (unused land (lnot (unused_un_bit op))) (depth - 1)))
	    allowed_un;
	  !cands in
	let binopcands = 
	  let partitions = partition 2 (k - 1) in
	  let cands = ref [] in
	  List.iter (fun part ->
	    match part with
		[d1; d2] ->
		  List.iter
                    (fun op ->
                      let new_unused = unused land (lnot (unused_bin_bit op)) in
                      for i = 0 to new_unused do
		        List.iter
                          (fun x ->
		            List.iter
                              (fun y -> cands := Op2 (op, x, y) :: !cands)
		              (gen input_shadowed nid (new_unused - i) d2))
		          (gen input_shadowed nid i d1)
                      done)
                    allowed_bin
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
    let inner = gen true 2 unused (depth - 5) in (* -5 = -1 (lambda), -2 (fold / lambda), -1 (Input), -1 (Zero) *)
    List.map (fun x ->
      Fold (Input, Zero, 0, 1, x))
      inner
  else
    gen false 0 unused (depth - 1) (* -1 is from the big "lambda" of outside *)
