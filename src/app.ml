open Lexer
open Parser
open Eval

let pp_int n=print_string "- : int = "; print_int n; print_newline();;

(*let rec cui()=print_string "# "; let line=read_line() in 
  if line="quit;;" then exit 0
  else (if line="" then cui() else let ast=parse (tokenize line) in pp_int (eval ast); cui())*)
let cui()=Slr_parser.slr_parse "1+1;;"