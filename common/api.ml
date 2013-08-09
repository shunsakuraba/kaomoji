open Type

let api_site = "http://icfpc2013.cloudapp.net"
let auth = "AUTHKEYvpsH1H"

let parse_operator_string_list l =
  let unops = ref [] in
  let binops = ref [] in
  let statements = ref [] in
  List.iter
    (function
      | `String(s) ->
        if s = "not" then unops := Not :: !unops
        else if s = "shl1" then unops := Shl1 :: !unops
        else if s = "shr1" then unops := Shr1 :: !unops
        else if s = "shr4" then unops := Shr4 :: !unops
        else if s = "shr16" then unops := Shr16 :: !unops
        else if s = "and" then binops := And :: !binops
        else if s = "or" then binops := Or :: !binops
        else if s = "xor" then binops := Xor :: !binops
        else if s = "plus" then binops := Plus :: !binops
        else if s = "if0" then statements := SIf0 :: !statements
        else if s = "fold" then statements := SFold :: !statements
        else if s = "tfold" then statements := STfold :: !statements
        else raise Not_found
      | _ -> raise Not_found
    )
    l;
  !unops, !binops, !statements
;;


let fetch_train size operators =
  let problem_url = api_site ^ "/train?auth=" ^ auth in
  let call =
    new Http_client.post_raw
      problem_url
      (Yojson.Safe.to_string
         (`Assoc([("size", `Int(size));
                  ("operators", `String(operators))]))) in
  let pipeline = new Http_client.pipeline in
  pipeline # add call;
  pipeline # run();
  call # response_body # value
;;

let scan_train_kv_list kv_list =
  let id = ref "" in
  let size = ref 0 in
  let operators = ref ([], [], []) in
  let challenge = ref "" in

  let parse_kv (key, value) =
    if key = "id" then
      match value with
          `String(s) -> id := s
        | _ ->
          print_endline "no";
          raise Not_found
    else if key = "size" then
      match value with
          `Int(i) -> size := i
        | _ ->
          print_endline "no";
          raise Not_found
    else if key = "operators" then
      match value with
          `List(l) -> operators := parse_operator_string_list l
        | _ ->
          print_endline "no";
          raise Not_found
    else if key = "challenge" then
      match value with
          `String(s) -> challenge := s
        | _ ->
          print_endline "c";
          raise Not_found
    else
      raise Not_found in

  List.iter parse_kv kv_list;
  (!id, !size, !operators, !challenge)
;;

let parse_train_string s =
  let train_json = Yojson.Safe.from_string s in
  match train_json with
    | `Assoc(kv_list) ->
      scan_train_kv_list kv_list
    | _ ->
      print_endline "b";
      raise Not_found
;;

let format_train (id, size, (unops, binops, statements), challenge) index =
  Printf.sprintf
    "%4d %s: %d [%s] [%s] [%s] %s"
    index
    id
    size
    (String.concat "," (List.map unop_to_string unops))
    (String.concat "," (List.map binop_to_string binops))
    (String.concat "," (List.map statement_to_string statements))
    challenge
;;

let scan_eval_result_kv_list kv_list =
  let status = ref "" in
  let outputs = ref [] in
  let message = ref "" in

  let parse_kv (key, value) =
    if key = "status" then
      match value with
          `String(s) -> status := s
        | _ ->
          print_endline "no";
          raise Not_found
    else if key = "message" then
      match value with
          `String(s) -> message := s
        | _ ->
          print_endline "no";
          raise Not_found
    else if key = "outputs" then
      match value with
          `List(l) ->
            outputs := List.map
              (fun x -> match x with
                | `String(s) -> Int64.of_string s
                | _ -> raise Not_found)
              l
        | _ ->
          print_endline "no";
          raise Not_found
    else
      raise Not_found in

  List.iter parse_kv kv_list;
  (!status, !outputs, !message)
;;

let parse_eval_result_string s =
  let json = Yojson.Safe.from_string s in
  match json with
    | `Assoc(kv_list) ->
      scan_eval_result_kv_list kv_list
    | _ ->
      print_endline "b";
      raise Not_found
;;

let eval_id id arguments =
  let url = api_site ^ "/eval?auth=" ^ auth in
  let call =
    new Http_client.post_raw
      url
      (Yojson.Safe.to_string
         (`Assoc([("id", `String(id));
                  ("arguments", `List(List.map (fun x -> `String(Printf.sprintf "%Lx" x)) arguments))]))) in
  let pipeline = new Http_client.pipeline in
  pipeline # add call;
  pipeline # run();
  let status = call # response_status_text in
  let result = call # response_body # value in
  print_endline status;
  print_endline result;
  parse_eval_result_string result
;;

let eval_program program_string arguments =
  let url = api_site ^ "/eval?auth=" ^ auth in
  let call =
    new Http_client.post_raw
      url
      (Yojson.Safe.to_string
         (`Assoc([("program", `String(program_string));
                  ("arguments", `List(List.map (fun x -> `String(Printf.sprintf "%Lx" x)) arguments))]))) in
  let pipeline = new Http_client.pipeline in
  pipeline # add call;
  pipeline # run();
  let status = call # response_status_text in
  let result = call # response_body # value in
  print_endline status;
  print_endline result;
  parse_eval_result_string result
;;



let scan_guess_result_kv_list kv_list =
  let status = ref "" in
  let values = ref [] in
  let message = ref "" in
  let lightning = ref false in

  let parse_kv (key, value) =
    if key = "status" then
      match value with
          `String(s) -> status := s
        | _ ->
          print_endline "no";
          raise Not_found
    else if key = "values" then
      match value with
          `List(l) ->
            values := List.map
              (fun x -> match x with
                | `String(s) -> Int64.of_string s
                | _ -> raise Not_found)
              l
        | _ ->
          print_endline "no";
          raise Not_found
    else if key = "message" then
      match value with
          `String(s) -> message := s
        | _ ->
          print_endline "no";
          raise Not_found
    else if key = "lightning" then
      match value with
          `Bool(b) -> lightning := b
        | _ ->
          print_endline "no";
          raise Not_found
    else
      raise Not_found in

  List.iter parse_kv kv_list;
  (!status, !values, !message, !lightning)
;;

let parse_guess_result_string s =
  let json = Yojson.Safe.from_string s in
  match json with
    | `Assoc(kv_list) ->
      scan_guess_result_kv_list kv_list
    | _ ->
      print_endline "b";
      raise Not_found
;;

let guess id program_string =
  let url = api_site ^ "/guess?auth=" ^ auth in
  let call =
    new Http_client.post_raw
      url
      (Yojson.Safe.to_string
         (`Assoc([("id", `String(id));
                  ("program", `String(program_string))]))) in
  let pipeline = new Http_client.pipeline in
  pipeline # add call;
  pipeline # run();
  let status = call # response_status_text in
  let result = call # response_body # value in
  print_endline status;
  print_endline result;
  parse_guess_result_string result
;;
