let train = Remote.fetch_train 3 "fold" in
print_endline (Train.format_train train 0);
let id, size, (unops, binops, statements), challenge = train in
let guess_result = Remote.guess id challenge in
let (status, _, message, _)= guess_result in
let status_string = Remoteguess.print_guess_status status in
print_endline (Printf.sprintf "%s %s" status_string message)
