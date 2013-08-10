open Type

let rec simp expr =
  match expr with
    Op1 (Shr1, Zero) -> Zero
    | Op1 (Shr4, Zero) -> Zero
    | Op1 (Shr16, Zero) -> Zero
    | Op1 (Shr1, One) -> Zero
    | Op1 (Shr4, One) -> Zero
    | Op1 (Shr16, One) -> Zero
    | Op1 (Shl1, Zero) -> Zero
    | Op1 (Not, Op1 (Not, a)) -> (simp a)
    | Op2 (And, _, Zero) -> Zero
    | Op2 (Or, a, b) when a = b -> (simp a)
    | Op2 (Or, Zero, a) -> (simp a)
    | Op2 (Or, Op1 (Not, Zero), a) -> (simp a)
    | Op2 (Or, a, Op1 (Not, Zero)) -> (simp a)
    | Op2 (Or, a, Zero) -> (simp a)
    | Op2 (Or, _, One) -> One
    | Op2 (Or, One, _) -> One
    | Op2 (Or, Op1 (Not, a), b) when a = b -> One
    | Op2 (Or, a, Op1 (Not, b)) when a = b -> One
    | Op2 (Or, Op2 (Or, a, b), c) when b = c -> Op2 (Or, (simp a), (simp b))
    | Op2 (Xor, a, b) when a = b -> Zero
    | Op2 (Xor, a, Zero) -> (simp a)
    | Op2 (Xor, Zero, a) -> (simp a)
    | Op2 (Xor, Op1 (Not, Zero), a) -> Op1 (Not, simp a)
    | Op2 (Xor, a, Op1 (Not, Zero)) -> Op1 (Not, simp a)
    | Op2 (Xor, Op1 (Not, a), b) when a = b -> Op1 (Not, Zero)
    | Op2 (o, a, b) ->
      let sa = simp a in
      let sb = simp b in
      if sa > sb then
	Op2 (o, sa, sb)
      else
	Op2 (o, sb, sa)
    | Op1 (o, a) -> Op1 (o, simp a)
    | Fold (a, b, c, d, e) -> Fold (simp a, simp b, c, d, simp e)
    | If0 (Zero, a, b) -> (simp a)
    | If0 (Op1 (Not, Zero), a, b) -> (simp b)
    | If0 (Op1 (Not, One), a, b) -> (simp b)
    | If0 (One, a, b) -> (simp b)
    | If0 (a, b, c) when b = c -> (simp b)
    | If0 (a, b, c)  -> If0 (simp a, simp b, simp c)
    | k -> k

let rec simplify expr =
  (* let _ = print_endline "--" in *)
  (* let _ = print_endline (Print.print expr) in *)
  let simplified = simp expr in
  (* let _ = print_endline (Print.print simplified) in *)
  if expr = simplified then
    expr
  else
    simplify simplified
