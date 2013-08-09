open Type

let fetch_train max_depth operator =
  let train_string = Train.fetch_train max_depth operator in
  Train.parse_train_string train_string

let guess id challenge =
  Remoteguess.guess id challenge

let eval_id id arguments =
  Remoteeval.eval_id id arguments

let eval_program program_string arguments =
  Remoteeval.eval_program program_string arguments

let guess id challenge =
  Remoteguess.guess id challenge

let is_match_ops_limit statements ops_limit =
  if ops_limit = "" then
    true
  else if ops_limit = "fold" then
    List.mem SFold statements
  else if ops_limit = "tfold" then
    List.mem STfold statements
  else
    true

let fetch_one_core_problem size_limit ops_limit target =
  if target = "real" then
    let problems_body = Problem.fetch_problems () in
    let problems_json = Yojson.Safe.from_string problems_body in
    let problems = Problem.parse_problems_json problems_json in
    let good_problems = List.find_all
      (fun x -> let (id, size, (unops, binops, statements), solved, time_left) = x in
	       if solved then
		 false
	       else if size <> size_limit then
		 false
	       else 
		 is_match_ops_limit statements ops_limit)
      problems in
    let problem = (List.nth good_problems 0) in
    let (id, size, ops, solved, time_left) = problem in
    (id, size, ops)
  else if target = "train" then
    let train = fetch_train size_limit ops_limit in
    let (id, size, ops, challenge) = train in
    (id, size, ops)
  else
    ("", 0, ([], [], []))

let format_core_problem (id, size, operators) =
  Printf.sprintf
    "%s: %2d %s"
    id
    size
    (Api.format_operator_tuple operators)
