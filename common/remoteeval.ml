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
          raise Api.Parse_error
    else if key = "message" then
      match value with
          `String(s) -> message := s
        | _ ->
          print_endline "no";
          raise Api.Parse_error
    else if key = "outputs" then
      match value with
          `List(l) ->
            outputs := List.map
              (fun x -> match x with
                | `String(s) -> Int64.of_string s
                | _ -> raise Api.Parse_error)
              l
        | _ ->
          print_endline "no";
          raise Api.Parse_error
    else
      raise Api.Parse_error in

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
      raise Api.Parse_error
;;

let eval_id id arguments =
  let url = Api.api_site ^ "/eval?auth=" ^ Api.auth in
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
  let url = Api.api_site ^ "/eval?auth=" ^ Api.auth in
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

