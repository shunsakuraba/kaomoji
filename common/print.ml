open Type
open Printf

let print expr =
  let rec print expr =
    match expr with
	Zero -> "0"
      | One -> "1"
      | Ident id -> print_id id
      | Input -> sprintf "input"
      | If0 (cond, ifc, elsec) ->
	sprintf "(if0 %s %s %s)"
	  (print cond)
	  (print ifc)
	  (print elsec)
      | Fold (inexpr, startexpr, idbyte, idcur, performexpr) ->
	sprintf "(fold %s %s (lambda (%s %s) %s))"
	  (print inexpr)
	  (print startexpr)
	  (print_id idbyte)
	  (print_id idcur)
	  (print performexpr)
      | Op1 (uop, e) ->
	sprintf "(%s %s)"
	  (unop_to_string uop)
	  (print e)
      | Op2 (bop, e1, e2) ->
	sprintf "(%s %s %s)"
	  (binop_to_string bop)
	  (print e1)
	  (print e2)
  and print_id id =
    sprintf "x_%d" id
  in
  sprintf "(lambda (input) %s)" (print expr)
