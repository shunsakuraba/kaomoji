open Type

let rand64 () = 
  let bit32 = Int64.shift_left 1L 32 in
  Int64.logxor 
    (Int64.shift_left (Random.int64 bit32) 32)
    (Random.int64 bit32)

let is_match_ops_limit statements ops_limit =
  match ops_limit with
    | "" ->
      true
    | "fold" ->
      List.mem SFold statements
    | "-fold" ->
      not (List.mem SFold statements) &&
      not (List.mem STfold statements)
    | "tfold" ->
      List.mem STfold statements
    | _ ->
      failwith "Unsupported ops_limit"
