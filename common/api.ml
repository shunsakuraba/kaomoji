open Type

(* let api_site = "http://icfpc2013.cloudapp.net" *)
let api_site = "AMAZONAWS"
let auth = "AUTHKEYvpsH1H"

exception Parse_error
exception Fetch_error

let parse_operator_string_list l =
  let unops = ref [] in
  let binops = ref [] in
  let statements = ref [] in

  List.iter
    (function
      | `String(s) ->
        begin
          match s with
            | "not" ->
              unops := Not :: !unops
            | "shl1" ->
              unops := Shl1 :: !unops
            | "shr1" ->
              unops := Shr1 :: !unops
            | "shr4" ->
              unops := Shr4 :: !unops
            | "shr16" ->
              unops := Shr16 :: !unops
            | "and" ->
              binops := And :: !binops
            | "or" ->
              binops := Or :: !binops
            | "xor" ->
              binops := Xor :: !binops
            | "plus" ->
              binops := Plus :: !binops
            | "if0" ->
              statements := SIf0 :: !statements
            | "fold" ->
              statements := SFold :: !statements
            | "tfold" ->
              statements := STfold :: !statements
            | op ->
              prerr_string ("Unknown op: " ^ op);
              raise Parse_error
        end
      | _ ->
        prerr_string "Malformed operators set";
        raise Parse_error
    )
    l;

  !unops, !binops, !statements

let format_operator_tuple (unops, binops, statements) =
  Printf.sprintf
    "[%s] [%s] [%s]"
    (String.concat "," (List.map unop_to_string unops))
    (String.concat "," (List.map binop_to_string binops))
    (String.concat "," (List.map statement_to_string statements))
