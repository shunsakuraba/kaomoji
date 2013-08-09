open Api;;

let train_string = fetch_train 3 "fold" in
let train = parse_train_string train_string in
print_endline (format_train train 0);

let id, _, (_, _, _), _ = train in
(* let id, size, (unops, binops, statements), challenge = train in *)
let _ = eval_id id [0; 100; 10000; 1000000; 100000000; 0xffffffffffffffff] in
let _ = eval_program "(lambda (x) (not 1))" [0; 100; 10000; 1000000; 100000000] in
let _ = guess id "(lambda (x) (not 1))" in
()
;;

let id, size, (unops, binops, statements), challenge = train in
let guess_result_string = guess id challenge in
let guess_result = parse_guess_result_string guess_result_string in
let (status, _, message, _)= guess_result in
print_endline (Printf.sprintf "%s %s" status message)
