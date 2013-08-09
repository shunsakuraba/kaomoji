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
	      List.map (fun part ->
		match part with
		    [d1; d2; d3] ->
		      List.map (fun x -> 
			List.map (fun y ->
			  List.map 
			    (fun z -> If0 (x, y, z))
			    (gen input_shadowed nid d3))
			  (gen input_shadowed nid d2))
			(gen input_shadowed nid d1)
		  | _ -> failwith "partition bugged")
		partitions in
	    List.flatten @@ List.flatten @@ List.flatten if0cands
	  else [] in
	let foldcands =
	  if fold_in_ops
	  then
	    let foldcands = 
	      let partitions = partition 3 (k - 2) in
	      List.map (fun part ->
		match part with
		    [d1; d2; d3] ->
		      List.map (fun x ->
			List.map (fun y ->
			  List.map (fun z ->
			    Fold (x, y, nid, nid + 1, z))
			    (gen input_shadowed (nid + 2) d3))
			  (gen input_shadowed nid d2))
			(gen input_shadowed nid d1)
		  | _ -> failwith "partition bugged")
		partitions in
	    List.flatten @@ List.flatten @@ List.flatten foldcands
	  else [] in
	let unopcands = 
	  List.map (fun x -> 
	    List.map (fun op ->
	      Op1 (op, x))
	      allowed_un)
	    (gen input_shadowed nid (depth - 1)) in
	let unopcands = List.flatten unopcands in
	let binopcands = 
	  let partitions = partition 2 (k - 1) in
	  List.map (fun part ->
	    match part with
		[d1; d2] ->
		  List.map (fun x ->
		    List.map (fun y ->
		      List.map (fun op ->
			Op2 (op, x, y))
			allowed_bin)
		      (gen input_shadowed nid d2))
		    (gen input_shadowed nid d1)
	      | _ -> failwith "partition bugged")
	    partitions in
	let binopcands = List.flatten @@ List.flatten @@ List.flatten binopcands in
	if0cands @ foldcands @ unopcands @ binopcands
  in
  if tfold_in_ops then
    let inner = gen true 2 (depth - 5) in (* -5 = -1 (lambda), -2 (fold / lambda), -1 (Input), -1 (Zero) *)
    List.map (fun x ->
      Fold (Input, Zero, 0, 1, x))
      inner
  else	
    gen false 0 (depth - 1) (* -1 is from the big "lambda" of outside *)


     

  
