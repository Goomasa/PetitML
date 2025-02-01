open Syntax
open Eval
open Typing

let rec print_type ty=match ty with
  | Int->print_string "int"
  | Bool->print_string "bool"
  | String->print_string "string"
  | Fun(t1,t2)->(match t1 with
    | Int|Bool|TyVar _->print_type t1; print_string " -> "; print_type t2
    | _->print_char '('; print_type t1; print_string ") ->"; print_type t2)
  | List t->print_type t; print_string " list"
  | TyVar id->
    print_string "'"; print_char (Char.chr (id mod 26+97));
    if id<26 then () else print_int (id/26)

let rec print_lit value=match value with
  | IntV n-> print_int n
  | BoolV true->print_string "true"
  | BoolV false->print_string "false"
  | StringV s->print_string ("\""^s^"\"")
  | FunV _ | RecV _->print_string "<fun>"
  | EmptyV->print_string "[]"
  | ListV _->print_string "["; print_list value; print_string "]"
  | _->print_string "??"
and print_list list=match list with
  | ListV(v,EmptyV)->print_lit v
  | ListV(v,next)->print_lit v; print_string "; "; print_list next
  | _->print_string "??"

let print_value value ty=(match value with
  | VarV(id,v)->print_string ("val "^id^" : "); print_type ty; print_string " = "; print_lit v
  | FunV _ | RecV _->print_string "- : "; print_type ty; print_string " = <fun>"
  | _->print_string " - : "; print_type ty; print_string " = "; print_lit value);
  print_newline()

let rec print_result trees env tyenv=match trees with
  | []->(env,tyenv)
  | h::t->(match h with
    | LetAnd defs -> 
      let now_env=env and now_tyenv=tyenv in 
      let (new_env,new_tyenv)=List.fold_left (fun (env,tyenv) e->
        let (ty,_,_)=ty_eval now_tyenv e in 
        let (value,_)=eval now_env e in 
        match value with
        | VarV(id,v) -> print_value value ty; ((id,v)::env,(id,ty)::tyenv)
        | _->err "invalid pattern") (env,tyenv) defs in print_result t new_env new_tyenv
    | _->
      let (ty,new_tyenv,_)=ty_eval tyenv h in
      let (value,new_env)=eval env h in print_value value ty; print_result t new_env new_tyenv)

let rec cui env tyenv=print_string "# "; let line=read_line() in 
  if line="quit;;" then exit 0
  else 
    try
      (if line="" then cui env tyenv 
      else let ast=Parser.lalr_parse line in let (new_env,new_tyenv)=print_result (List.rev ast) env tyenv in cui new_env new_tyenv)
    with e->print_string(Printexc.to_string e); print_newline(); cui env tyenv
