open Slr_parser
open Eval

let print_bool b=match b with
  | true->print_string "true"
  | false->print_string "false"

let rec pp_results trees=match trees with
  | []->()
  | h::t->(match (eval h) with
    | IntV n -> print_string "- : int = "; print_int n
    | BoolV b-> print_string "- : bool = "; print_bool b ); 
    print_newline(); pp_results t

let rec cui()=print_string "# "; let line=read_line() in 
  if line="quit;;" then exit 0
  else (if line="" then cui() else let ast=slr_parse line in pp_results ast; cui())
