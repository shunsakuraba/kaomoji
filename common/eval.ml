open Type

let eval expr input = 
  let lookup env id =
    List.assoc id env
  in
  let rec eval input env expr = 
    let open Int64 in
	match expr with
	    Zero -> zero
	  | One -> one
	  | Ident id -> lookup env id
	  | Input -> input
	  | If0 (cond, ifc, elsec) -> 
	    if eval input env cond = zero 
	    then eval input env ifc
	    else eval input env elsec
	  | Fold (ein, estart, byteid, curid, e2) ->
	    let xin = eval input env ein in
	    let es = eval input env estart in
	    let now = ref es in
	    for i = 0 to 7 do
	      let cur = logand (shift_right_logical xin (i * 8)) 0xffL in
	      let newnow = eval input ((byteid, cur)::(curid, !now)::env) e2 in
	      now := newnow
	    done;
	    !now
	  | Op1 (op, e) ->
	    let v = eval input env e in
	    begin
	      match op with 
		  Not -> lognot v
		| Shl1 -> shift_left v 1
		| Shr1 -> shift_right_logical v 1
		| Shr4 -> shift_right_logical v 4
		| Shr16 -> shift_right_logical v 16
	    end
	  | Op2 (op, e1, e2) ->
	    let v1 = eval input env e1 in
	    let v2 = eval input env e2 in
	    begin
	      match op with
		  And -> logand v1 v2
		| Or -> logor v1 v2
		| Xor -> logxor v1 v2
		| Plus -> add v1 v2
	    end
  in
  let inienv = [] in
  eval input inienv expr

(* let normalize expr = *)
(*   let rec normalize expr = *)
(*     let open Int64 in *)
(*         match expr with *)
(*             Zero -> Imm(zero) *)
(*           | One -> Imm(one) *)
(*           | Ident id -> Ident id *)
(*           | Input -> Input *)
(*           | If0 (cond, ifc, elsec) -> *)
(*             begin *)
(*               let n_cond = normalize cond in *)
(*               match n_cond with *)
(*                   Imm(i) -> *)
(*                     begin *)
(*                       if i = zero then *)
(*                         normalize ifc *)
(*                       else *)
(*                         normalize elsec *)
(*                     end *)
(*                 | x -> If0 n_cond (normalize ifc) (normalize elsec) *)
(*             end *)
(*           | Fold (ein, estart, byteid, curid, e2) -> *)
(*             let xin = eval input env ein in *)
(*             let es = eval input env estart in *)
(*             let now = ref es in *)
(*             for i = 0 to 7 do *)
(*               let cur = logand (shift_right_logical xin (i * 8)) 0xffL in *)
(*               let newnow = eval input ((byteid, cur)::(curid, !now)::env) e2 in *)
(*               now := newnow *)
(*             done; *)
(*             !now *)
(*           | Op1 (op, e) -> *)
(*             let v = eval input env e in *)
(*             begin *)
(*               match op with  *)
(*                   Not -> lognot v *)
(*                 | Shl1 -> shift_left v 1 *)
(*                 | Shr1 -> shift_right_logical v 1 *)
(*                 | Shr4 -> shift_right_logical v 4 *)
(*                 | Shr16 -> shift_right_logical v 16 *)
(*             end *)
(*           | Op2 (op, e1, e2) -> *)
(*             let v1 = normalize e1 in *)
(*             let v2 = normalize e2 in *)
(*             begin *)
(*               match op with *)
(*                   And -> logand v1 v2 *)
(*                 | Or -> logor v1 v2 *)
(*                 | Xor -> logxor v1 v2 *)
(*                 | Plus -> add v1 v2 *)
(*             end *)
(*     in *)
(*     normalize expr *)
