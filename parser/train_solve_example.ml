let train = Remote.fetch_train 3 "fold" in
print_endline (Train.format_train train 0);

let id, _, (_, _, _), _ = train in
(* let id, size, (unops, binops, statements), challenge = train in *)
let _ = Remote.eval_id id [0L; 100L; 10000L; 1000000L; 100000000L; 0xffffffffffffffffL] in
let _ = Remote.eval_program
  "(lambda (x) (not 1))" [0L; 100L; 10000L; 1000000L; 100000000L] in
let _ = Remote.guess id "(lambda (x) (not 1))" in
()
;

let id, _, _, challenge = train in
let guess_result = Remote.guess id challenge in
let status, _, message, _ = guess_result in
let status_string = Remoteguess.print_guess_status status in
print_endline (Printf.sprintf "%s %s" status_string message)
