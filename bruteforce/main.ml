
let main = 
  let g = Brute.gen 3 in
  List.iter (fun e -> print_endline (Print.print e)) g
