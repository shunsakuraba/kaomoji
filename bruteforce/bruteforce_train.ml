open Api
open Train

let rand64 () = 
  let bit32 = Int64.shift_left 1L 32 in
  Int64.logxor 
    (Int64.shift_left (Random.int64 bit32) 32)
    (Random.int64 bit32)

let main =
  if false then
    let bitseq = 
      Array.to_list
	(Array.init 64 (fun x -> Int64.shift_left 1L x)) in
    let syntax = 
      let open Type in
	  Fold (Input, Zero, 0, 1, Op2 (And, Ident 0, Op1 (Not, Ident 1))) in
      let outputs = List.map (Eval.eval syntax) bitseq in
      let merge = List.combine bitseq outputs in
      List.iter
	(fun (i,o) -> Printf.printf "%LX => %LX\n" i o)
	merge
  else
    ()

let main = 
  let train_string = fetch_train 12 "tfold" in
  let train = parse_train_string train_string in
  let () = print_endline (format_train train 0) in
  let id, size, (unops, binops, statements), challenge = train in
  let initialguess = 
    let bitseq = 
      Array.to_list
	(Array.init 64 (fun x -> Int64.shift_left 1L x)) in
    let randseq = 
      Array.to_list
	(Array.init 192
	   (fun _ -> rand64 ())) in
    bitseq @ randseq in
  let (status, outputs, message) = eval_id id initialguess in
  if status <> "ok" then
    begin
      Printf.printf "Status: \"%s\"\n" status;
      failwith "Eval returned error"
    end
  else
    Guess.guess (List.combine initialguess outputs)
      (fun x -> 
	let () = print_endline (Print.print x) in
	Feedback.Success)
      (unops, binops, statements)
      size
