open Type

let eval expr input =
  let lookup env id =
    List.assoc id env
  in
  let rec eval env expr =
    let open Int64 in
	match expr with
	    Zero -> zero
	  | One -> one
	  | Ident id -> lookup env id
	  | Input -> input
	  | If0 (cond, ifc, elsec) ->
            let cond_value = eval env cond in
	    if cond_value = zero
	    then eval env ifc
	    else eval env elsec
	  | Fold (ein, estart, byteid, curid, e2) ->
	    let xin = eval env ein in
	    let es = eval env estart in
	    let now = ref es in
	    for i = 0 to 7 do
	      let cur = logand (shift_right_logical xin (i * 8)) 0xffL in
              let new_env = (byteid, cur)::(curid, !now)::env in
	      let newnow = eval new_env e2 in
	      now := newnow
	    done;
	    !now
	  | Op1 (op, e) ->
	    let v = eval env e in
	    begin
	      match op with
		  Not -> lognot v
		| Shl1 -> shift_left v 1
		| Shr1 -> shift_right_logical v 1
		| Shr4 -> shift_right_logical v 4
		| Shr16 -> shift_right_logical v 16
	    end
	  | Op2 (op, e1, e2) ->
	    let v1 = eval env e1 in
	    let v2 = eval env e2 in
	    begin
	      match op with
		  And -> logand v1 v2
		| Or -> logor v1 v2
		| Xor -> logxor v1 v2
		| Plus -> add v1 v2
	    end
  in
  let inienv = [] in
  eval inienv expr
