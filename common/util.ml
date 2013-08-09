let rand64 () = 
  let bit32 = Int64.shift_left 1L 32 in
  Int64.logxor 
    (Int64.shift_left (Random.int64 bit32) 32)
    (Random.int64 bit32)
