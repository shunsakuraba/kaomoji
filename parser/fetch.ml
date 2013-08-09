let prefix = "http://icfpc2013.cloudapp.net"
let auth = "AUTHKEYvpsH1H"

let problem_url = prefix ^ "/myproblems?auth=" ^ auth

(* let fetch () = *)
(*   Http_user_agent.get problem_url;; *)

(* print_endline (fetch ()) *)

let parse = function
  | `Assoc(kv_list) ->
    (let id = ref "" and
     let size = ref "0" and
     let operatos = ref [] and
     let solved = ref false and
     let time_left = ref 0 in

     let (List.find (fun (key, value) -> key = "size") l)
     let parse_kv = function
      with
        _, `Int(i) -> i
      | _ -> (print_endline "c"; raise Not_found)

     List.iter parse_kv kv_list
      )
  | _ ->
    print_endline "b";
    raise Not_found
;;

let problems = Yojson.Safe.from_file "problems.json";;

let parsed_problems = List.map parse js

match js with
    `List(j) ->
      (
        let sorted = List.sort (fun x y -> (extract_size x) - (extract_size y)) j in
        List.iter (fun x -> print_endline (Yojson.Safe.pretty_to_string x)) sorted
      )
  | _ -> raise Not_found;;
