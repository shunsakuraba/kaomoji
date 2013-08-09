let prefix = "http://icfpc2013.cloudapp.net"
let auth = "AUTHKEYvpsH1H"

let fetch () =
  let problem_url = prefix ^ "/myproblems?auth=" ^ auth in
  Http_user_agent.get problem_url
;;

print_endline (fetch ())
