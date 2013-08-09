open Api;;
open Train;;
open Remoteeval;;
open Remoteguess;;

let train_string = fetch_train 3 "fold" in
let train = parse_train_string train_string in
print_endline (format_train train 0);
let id, size, (unops, binops, statements), challenge = train in
let guess_result = guess id challenge in
let (status, _, message, _)= guess_result in
print_endline (Printf.sprintf "%s %s" status message)
