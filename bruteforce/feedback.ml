

type feedback = 
    Success
  | Fail of instance
and instance = Int64.t * Int64.t
