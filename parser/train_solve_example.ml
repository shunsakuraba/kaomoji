open Api;;

let train_string = fetch_train 3 "fold" in
let train = parse_train_string train_string in
print_endline (format_train train 0);
let id, size, (unops, binops, statements), challenge = train in
()
