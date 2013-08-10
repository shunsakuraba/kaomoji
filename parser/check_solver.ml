open Api;;
open Train;;
open Util;;
open Eval;;
open Print;;
open Type;;

let check_brute id allowed_ops_tuple size challenge =
  let answers = Brute.gen allowed_ops_tuple size in
  let answer = List.nth answers (Random.int (List.length answers)) in
  let initialguess =
    let bitseq = 
      Array.to_list
        (Array.init 64 (fun x -> Int64.shift_left 1L x)) in
    let fixedseq =
      [0xFFFFFFFFFFFFFFFFL;
       0x0000000000000000L;
       0xAAAAAAAAAAAAAAAAL;
       0x5555555555555555L;
       0xFFFFFFFF00000000L;
       0x00000000FFFFFFFFL;
       0x0000FFFFFFFF0000L;
       0xFFFF00000000FFFFL;
       0xFF00FF00FF00FF00L
      ] in
    (* Random.init 0; *)
    let randseq = 
      Array.to_list
        (Array.init (256 - (List.length bitseq) - (List.length fixedseq))
           (fun _ -> rand64 ())) in
    bitseq @ fixedseq @ randseq in
  let outputs = List.map (fun x -> eval answer x) initialguess in

  let uns, bins, stmts = allowed_ops_tuple in
  Printf.printf
    "%s\n%d\n%s\n%s\n%s\n%s\n%s\n"
    (Print.print answer)
    size
    (String.concat " " (List.map unop_to_string uns))
    (String.concat " " (List.map binop_to_string bins))
    (String.concat " " (List.map statement_to_string stmts))
    (String.concat " " (List.map (Printf.sprintf "0x%016Lx") initialguess))
    (String.concat " " (List.map (Printf.sprintf "0x%016Lx") outputs));

  let guess_function x =
    (* Printf.printf *)
    (*   "%s\n  %s\n  %s\n  original: %s\n  guess:    %s\n  answer:   %s\n" *)
    (*   id *)
    (*   (format_operator_tuple allowed_ops_tuple) *)
    (*   (string_of_int size) *)
    (*   challenge *)
    (*   (Print.print x) *)
    (*   (Print.print answer); *)
    Feedback.Success
  in
  let alllist = Brute.gen allowed_ops_tuple size in
  GuessCaller.guess_call
    (List.combine initialguess outputs)
    guess_function
    allowed_ops_tuple
    size
    alllist

let rec check_by_train_list () =
  let train_string = read_line () in
  let train = parse_train_string train_string in
  let id, size, allowed_ops_tuple, challenge = train in

  check_brute id allowed_ops_tuple size challenge;
  check_by_train_list ()
;;

let check_by_problems () =
  let problems_string = read_line () in
  let problems_json = Yojson.Safe.from_string problems_string in
  let problems = Problem.parse_problems_json problems_json in

  let sorted_problems =
    List.sort
      (fun (_, x_size, (_, _, x_s), _, _) (_, y_size, (_, _, y_s), _, _) ->
        let l_diff = (List.length x_s) - (List.length y_s) in
        if l_diff != 0
        then l_diff
        else x_size - y_size)
      problems
  in

  List.iter
    (fun (id, size, allowed_ops_tuple, solved, time_over) ->
      if size = 10 then failwith "stop" else
      check_brute id allowed_ops_tuple size ((if solved then "T" else "F") ^ ", " ^ (string_of_float time_over)))
    sorted_problems
;;

check_by_problems ()
