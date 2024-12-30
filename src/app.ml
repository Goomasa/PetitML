open Parser
open Eval

let print_lit value=match value with
  | Environment.IntV n-> print_int n
  | BoolV true->print_string "true"
  | BoolV false->print_string "false"
  | _->print_string "??"

let rec pp_results trees env=match trees with
  | []->env
  | h::t->
    let (value,new_env)=eval h env in 
    (match value with
      | IntV _|BoolV _->print_string " - = "; print_lit value
      | Var(id,v)->print_string id; print_string " = ";print_lit v
      | FunV(_,_)->print_string "- = <fun>"); 
    print_newline(); pp_results t new_env

let rec cui env=print_string "# "; let line=read_line() in 
  if line="quit;;" then exit 0
  else (if line="" then cui env else let ast=slr_parse line in cui (pp_results ast env))
