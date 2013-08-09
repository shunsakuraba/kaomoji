open Feedback

let filter_by_guess input output expr =
  Eval.eval expr input = output
  
let guess initial feedback allowed depth = 
  let alllist = Brute.gen allowed depth in
  let initiallist = 
    List.fold_left
      (fun cur (i, o) ->
	List.filter (filter_by_guess i o) cur)
      alllist initial
  in
  let curlist = ref initiallist in
  try
    while true do
      match !curlist with
	  [] -> failwith "No more candidates"
	| trial :: _ ->
	  match feedback trial with
	      Success -> 
		raise Exit
	    (* feedback automatically submits answer, thus no more thing to do *)
	    | Fail (i, o) ->
	      curlist := List.filter (filter_by_guess i o) !curlist
    done
  with Exit -> ()
    
