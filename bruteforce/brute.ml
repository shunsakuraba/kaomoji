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

let gen (allowed_un, allowed_bin, allowed_stmts) depth =
  let if0_in_ops = List.mem SIf0 allowed_stmts in
  let fold_in_ops = List.mem SFold allowed_stmts in
  let tfold_in_ops = List.mem STfold allowed_stmts in
  let rec gen input_shadowed nid depth = 
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
	      List.iter (fun part ->
		match part with
		    [d1; d2; d3] ->
		      List.iter (fun x -> 
			List.iter (fun y ->
			  List.iter 
			    (fun z ->
			      cands := If0 (x, y, z) :: !cands)
			    (gen input_shadowed nid d3))
			  (gen input_shadowed nid d2))
			(gen input_shadowed nid d1)
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
	      List.iter (fun part ->
		match part with
		    [d1; d2; d3] ->
		      List.iter (fun x ->
			List.iter (fun y ->
			  List.iter (fun z ->
			    cands := Fold (x, y, nid, nid + 1, z) :: !cands)
			    (gen input_shadowed (nid + 2) d3))
			  (gen input_shadowed nid d2))
			(gen input_shadowed nid d1)
		  | _ -> failwith "partition bugged")
		partitions;
	      !cands in
	    foldcands
	  else [] in
	let unopcands = 
	  let cands = ref [] in
	  List.iter (fun x -> 
	    List.iter (fun op ->
	      cands := Op1 (op, x) :: !cands)
	      allowed_un)
	    (gen input_shadowed nid (depth - 1)); 
	  !cands in
	let binopcands = 
	  let partitions = partition 2 (k - 1) in
	  let cands = ref [] in
	  List.iter (fun part ->
	    match part with
		[d1; d2] ->
		  List.iter (fun x ->
		    List.iter (fun y ->
		      List.iter (fun op ->
			cands := Op2 (op, x, y) :: !cands)
			allowed_bin)
		      (gen input_shadowed nid d2))
		    (gen input_shadowed nid d1)
	      | _ -> failwith "partition bugged")
	    partitions;
	  !cands
	in
	let ( @++ ) = List.rev_append in 
	if0cands @++ foldcands @++ unopcands @++ binopcands
  in
  if tfold_in_ops then
    let inner = gen true 2 (depth - 5) in (* -5 = -1 (lambda), -2 (fold / lambda), -1 (Input), -1 (Zero) *)
    List.map (fun x ->
      Fold (Input, Zero, 0, 1, x))
      inner
  else	
    gen false 0 (depth - 1) (* -1 is from the big "lambda" of outside *)


     

  
