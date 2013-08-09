
let main = 
  let g = Brute.gen 6 in
  List.iter (fun e -> print_endline (Print.print e)) g

open Guess

let main = 
  guess [] (fun _ -> Feedback.Success) 3

