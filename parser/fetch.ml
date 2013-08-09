open Api

let fetch () =
  let problem_url = api_site ^ "/myproblems?auth=" ^ auth in
  Http_user_agent.get problem_url
;;

print_endline (fetch ())
