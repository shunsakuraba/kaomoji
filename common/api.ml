open Type

let api_site = "http://icfpc2013.cloudapp.net"
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
        if s = "not" then unops := Not :: !unops
        else if s = "shl1" then unops := Shl1 :: !unops
        else if s = "shr1" then unops := Shr1 :: !unops
        else if s = "shr4" then unops := Shr4 :: !unops
        else if s = "shr16" then unops := Shr16 :: !unops
        else if s = "and" then binops := And :: !binops
        else if s = "or" then binops := Or :: !binops
        else if s = "xor" then binops := Xor :: !binops
        else if s = "plus" then binops := Plus :: !binops
        else if s = "if0" then statements := SIf0 :: !statements
        else if s = "fold" then statements := SFold :: !statements
        else if s = "tfold" then statements := STfold :: !statements
        else raise Parse_error
      | _ -> raise Parse_error
    )
    l;
  !unops, !binops, !statements

let format_operator_tuple (unops, binops, statements) =
  Printf.sprintf
    "[%s] [%s] [%s]"
    (String.concat "," (List.map unop_to_string unops))
    (String.concat "," (List.map binop_to_string binops))
    (String.concat "," (List.map statement_to_string statements))

