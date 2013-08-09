open Api;;
open Train;;
open Util;;
open Eval;;
open Brute;;
open Print;;

let rec consume () =
  let train_string = read_line () in
  let train = parse_train_string train_string in
  let id, size, (unops, binops, statements), challenge = train in
  let answers = gen (unops, binops, statements) size in
  let answer = List.nth answers (Random.int (List.length answers)) in
  let initialguess =
    let bitseq = 
      Array.to_list
        (Array.init 64 (fun x -> Int64.shift_left 1L x)) in
    let randseq = 
      Array.to_list
        (Array.init 192
           (fun _ -> rand64 ())) in
    bitseq @ randseq in
  let outputs = List.map (fun x -> eval answer x) initialguess in
  let guess_function x =
    print_endline (id ^ "\n  " ^ (format_operator_tuple (unops, binops, statements)) ^ "\n  " ^ (string_of_int size)^ "\n  original: " ^ challenge ^ "\n  guess:    " ^ (Print.print x) ^ "\n  answer:   " ^ (Print.print answer));
    Feedback.Success
  in
  GuessCaller.guess_call
    (List.combine initialguess outputs)
    guess_function
    (unops, binops, statements)
    size;
  consume () in
consume ()
