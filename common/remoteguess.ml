type guess_status = GuessStatusWin | GuessStatusMismatch |
    GuessStatusError | GuessStatusInvalid;;

let parse_guess_status s =
  if s = "win" then
    GuessStatusWin
  else if s = "mismatch" then
    GuessStatusMismatch
  else if s = "error" then
    GuessStatusError
  else
    GuessStatusInvalid

let print_guess_status s =
  if s = GuessStatusWin then
    "win"
  else if s = GuessStatusMismatch then
    "mismatch"
  else if s = GuessStatusError then
    "error"
  else
    "invalid"

let parse_guess_result_string s =
  let scan_guess_result_kv_list kv_list =
    let status = ref GuessStatusInvalid in
    let values = ref [] in
    let message = ref "" in
    let lightning = ref false in

    let parse_kv (key, value) =
      if key = "status" then
        match value with
            `String(s) -> status := parse_guess_status s
          | _ ->
            raise Api.Parse_error
      else if key = "values" then
        match value with
            `List(l) ->
              values := List.map
                (fun x -> match x with
                  | `String(s) -> Int64.of_string s
                  | _ -> raise Api.Parse_error)
                l
          | _ ->
            raise Api.Parse_error
      else if key = "message" then
        match value with
            `String(s) -> message := s
          | _ ->
            raise Api.Parse_error
      else if key = "lightning" then
        match value with
            `Bool(b) -> lightning := b
          | _ ->
            raise Api.Parse_error
      else
        raise Api.Parse_error in

    List.iter parse_kv kv_list;
    (!status, !values, !message, !lightning) in

  let j = Yojson.Safe.from_string s in
  match j with
    | `Assoc(kv_list) ->
      scan_guess_result_kv_list kv_list
    | _ ->
      raise Api.Parse_error

let guess id program_string =
  let url = Api.api_site ^ "/guess?auth=" ^ Api.auth in
  let call =
    new Http_client.post_raw
      url
      (Yojson.Safe.to_string
         (`Assoc(
           [("id", `String(id));
            ("program", `String(program_string))]))) in

  prerr_endline "Running guess RPC";

  let pipeline = new Http_client.pipeline in
  pipeline # add call;
  pipeline # run();

  let status_code = call # response_status_code in
  let status_text = call # response_status_text in
  prerr_endline ("Status: " ^ (string_of_int status_code) ^ " " ^ status_text);

  if status_code <> 200 then
    begin
      print_endline ("Guess failed");
      raise Api.Fetch_error
    end
  else
    let body = call # response_body # value in
    prerr_endline ("Body: " ^ body);
    parse_guess_result_string body
