val gen: (Type.unop list * Type.binop list * Type.statements list) -> int -> Type.expr list
val gen2: (Type.unop list * Type.binop list * Type.statements list) -> int -> (Type.expr * int) list DynArray.t -> Type.expr list * (Type.expr * int) list DynArray.t
val get_candidates: (string * int * (Type.unop list * Type.binop list * Type.statements list)) -> Type.expr list
val create_db: (Type.unop list * Type.binop list * Type.statements list) -> (Type.expr * int) list DynArray.t

exception CandidateSizeLooksTooBigException
