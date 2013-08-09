open Api;;
open Train;;
open Remoteeval;;
open Remoteguess;;

let train_string = fetch_train 3 "fold" in
let train = parse_train_string train_string in
print_endline (format_train train 0);

let id, _, (_, _, _), _ = train in
(* let id, size, (unops, binops, statements), challenge = train in *)
let _ = eval_id id [0L; 100L; 10000L; 1000000L; 100000000L; 0xffffffffffffffffL] in
let _ = eval_program "(lambda (x) (not 1))" [0L; 100L; 10000L; 1000000L; 100000000L] in
let _ = guess id "(lambda (x) (not 1))" in
()
;

let id, _, _, challenge = train in
let guess_result = guess id challenge in
let status, _, message, _ = guess_result in
print_endline (Printf.sprintf "%s %s" status message)
