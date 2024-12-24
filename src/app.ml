open Slr_parser
open Eval

let pp_int n=print_string "- : int = "; print_int n; print_newline();;

let rec pp_results trees=match trees with
  | []->()
  | h::t->pp_int (eval h); pp_results t

let rec cui()=print_string "# "; let line=read_line() in 
  if line="quit;;" then exit 0
  else (if line="" then cui() else let ast=slr_parse line in pp_results ast; cui())
