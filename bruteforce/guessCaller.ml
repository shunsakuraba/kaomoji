open Feedback
open ExtList

let filter_by_guess input output expr =
  Eval.eval expr input = output

(*let guess_call eval_results feedback allowed depth candidates =
  let rec consume = function
    | x :: xs ->
      if List.for_all (fun (i, o) -> filter_by_guess i o x) eval_results then
        x, xs
      else
        consume xs
    | _ -> raise Not_found in
  consume candidates*)
  
let guess_call initial feedback allowed depth alllist = 
  (* let () = List.iter (fun x -> print_endline (Print.print x)) alllist in *)
  let initiallist = 
    (* List.fold_left *)
    (*   (fun cur (i, o) -> *)
    (*     List.filter (filter_by_guess i o) cur) *)
    (*   alllist initial *)
    List.filter
      (fun e ->
        List.for_all
          (fun (i, o) -> filter_by_guess i o e)
          initial)
      alllist
  in
  let count = List.length initiallist in
  let () = Printf.eprintf "After filtering %d elements remains\n" count in
  let curlist = ref initiallist in
  try
    while true do
      match !curlist with
	  [] -> failwith "No more candidates"
	| trial :: _ ->
	  match feedback trial count with
	      Success -> 
		raise Exit
	    (* feedback automatically submits answer, thus no more thing to do *)
	    | Fail (i, o) ->
	      curlist := List.filter (filter_by_guess i o) !curlist
    done
  with Exit -> ()
    
