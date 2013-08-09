open Api

let fetch () =
  let problem_url = api_site ^ "/train?auth=" ^ auth in
  let call =
    new Http_client.post_raw
      problem_url
      (Yojson.Safe.to_string
         (`Assoc([("size", `Int(10));
                  ("operators", `String("fold"))]))) in
  let pipeline = new Http_client.pipeline in
  pipeline # add call;
  pipeline # run();
  call # response_body # value
;;

print_endline (fetch ())
