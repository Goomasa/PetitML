open Parser
open Eval

let rec print_lit value=match value with
  | Environment.IntV n-> print_int n
  | BoolV true->print_string "true"
  | BoolV false->print_string "false"
  | FunV _ | RecV _->print_string "<fun>"
  | EmptyV->print_string "[]"
  | ListV _->print_string "["; print_list value; print_string "]"
  | _->print_string "??"
and print_list list=match list with
  | ListV(v,EmptyV)->print_lit v
  | ListV(v,next)->print_lit v; print_string "; "; print_list next
  | _->print_string "??"

let rec pp_results trees env=match trees with
  | []->env
  | h::t->
    let (value,new_env)=eval h env in 
    (match value with
      | VarV(id,v)->print_string ("val "^id^" = "); print_lit v
      | FunV _ | RecV _->print_string "- = <fun>"
      | _->print_string " - = "; print_lit value);
    print_newline(); pp_results t new_env

let rec cui env=print_string "# "; let line=read_line() in 
  if line="quit;;" then exit 0
  else (if line="" then cui env else let ast=lalr_parse line in cui (pp_results ast env))
