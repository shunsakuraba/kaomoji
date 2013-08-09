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

let gen depth =
  let rec gen nid depth = 
    match depth with
	0 -> []
      | 1 ->
	[Zero; One; Input] 
	@ Array.to_list (Array.init nid (fun x -> Ident x))
      | k ->
	(* I will regret this spaghetti *)
	(* NEWS: I did *)
	let if0cands = 
	  let partitions = partition 3 (k - 1) in
	  List.map (fun part ->
	    match part with
		[d1; d2; d3] ->
		  List.map (fun x -> 
		    List.map (fun y ->
		      List.map 
			(fun z -> If0 (x, y, z))
			(gen nid d3))
		      (gen nid d2))
		    (gen nid d1)
	      | _ -> failwith "partition bugged")
	    partitions in
	let if0cands = List.flatten @@ List.flatten @@ List.flatten if0cands in
	let foldcands =
	  let partitions = partition 3 (k - 2) in
	  List.map (fun part ->
	    match part with
		[d1; d2; d3] ->
		  List.map (fun x ->
		    List.map (fun y ->
		      List.map (fun z ->
			Fold (x, y, nid, nid + 1, z))
			(gen (nid + 2) d3))
		      (gen nid d2))
		    (gen nid d1)
	      | _ -> failwith "partition bugged")
	    partitions in
	let foldcands = List.flatten @@ List.flatten @@ List.flatten foldcands in
	let unopcands = 
	  List.map (fun x -> 
	    List.map (fun op ->
	      Op1 (op, x))
	      op1list)
	    (gen nid (depth - 1)) in
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
			op2list)
		      (gen nid d2))
		    (gen nid d1)
	      | _ -> failwith "partition bugged")
	    partitions in
	let binopcands = List.flatten @@ List.flatten @@ List.flatten binopcands in
	if0cands @ foldcands @ unopcands @ binopcands
  in
  gen 0 depth


     

  
