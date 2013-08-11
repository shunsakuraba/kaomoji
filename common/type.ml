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

and statements =
    SIf0
  | SFold
  | STfold

let unop_to_string = function
  | Not -> "not"
  | Shl1 -> "shl1"
  | Shr1 -> "shr1"
  | Shr4 -> "shr4"
  | Shr16 -> "shr16"

let binop_to_string = function
  | And -> "and"
  | Or -> "or"
  | Xor -> "xor"
  | Plus -> "plus"

let statement_to_string = function
  | SIf0 -> "if0"
  | SFold -> "fold"
  | STfold -> "tfold"

let unop_to_cstring = function
  | Not -> "Not"
  | Shl1 -> "Shl1"
  | Shr1 -> "Shr1"
  | Shr4 -> "Shr4"
  | Shr16 -> "Shr16"

let binop_to_cstring = function
  | And -> "And"
  | Or -> "Or"
  | Xor -> "Xor"
  | Plus -> "Plus"

let statement_to_cstring = function
  | SIf0 -> "SIf0"
  | SFold -> "SFold"
  | STfold -> "STfold"
