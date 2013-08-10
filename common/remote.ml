open Type
open Problem

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

let fetch_one_core_problem size_limit ops_limit target =
  if target = "real" then
    let good_problems = fetch_good_problems size_limit ops_limit in
    if (List.length good_problems) > 0 then
      let rand_id = Random.int (List.length good_problems) in
      let problem = (List.nth good_problems rand_id) in
      let _ = print_endline (format_problem problem 0) in
      (* let _ = input_line stdin in *)
      let (id, size, ops, solved, time_left) = problem in
      (id, size, ops)
    else
      failwith "No more problem matching the condition."
  else if target = "train" then
    let train = fetch_train size_limit ops_limit in
    let (id, size, ops, challenge) = train in
    (id, size, ops)
  else if target = "train_local" then
    let train = Train.read_train_from_file "trains.json" in
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
