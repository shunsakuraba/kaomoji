
type expr = 
    Zero
  | One
  | Ident of id
  | Input
  | If0 of expr * expr * expr
  | Fold of expr * expr * id * id * expr
  | Op1 of unop * expr
  | Op2 of binop * expr * expr
and id = int
and unop = 
    Not
  | Shl1
  | Shr1
  | Shr4
  | Shr16
and binop = 
    And
  | Or
  | Xor
  | Plus


