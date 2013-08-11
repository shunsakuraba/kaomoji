val gen: (Type.unop list * Type.binop list * Type.statements list) -> int -> Type.expr list
val gen2: (Type.unop list * Type.binop list * Type.statements list) -> int -> Type.expr list
val get_candidates: (string * int * (Type.unop list * Type.binop list * Type.statements list)) -> Type.expr list

exception CandidateSizeLooksTooBigException
