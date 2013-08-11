val gen: (Type.unop list * Type.binop list * Type.statements list) -> int -> Type.expr list
val gen2: (Type.unop list * Type.binop list * Type.statements list) -> int -> (Type.expr * int) list DynArray.t -> Type.expr list * (Type.expr * int) list DynArray.t
val get_candidates: (string * int * (Type.unop list * Type.binop list * Type.statements list)) -> int -> Type.expr list * (Type.expr * int) list DynArray.t
val create_db: (Type.unop list * Type.binop list * Type.statements list) -> (Type.expr * int) list DynArray.t
val expand_db: (Type.unop list * Type.binop list * Type.statements list) -> (Type.expr * int) list DynArray.t -> int -> unit
val cleanup_candidates: Type.expr list -> Type.expr list
val generate_candidates_from_db: (Type.unop list * Type.binop list * Type.statements list) -> (Type.expr * int) list DynArray.t -> Type.expr list

exception CandidateSizeLooksTooBigException
