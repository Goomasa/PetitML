open Syntax
open Eval
open Typing

let rec print_type ty prev_id=match ty with
  | Int->print_string "int"
  | Bool->print_string "bool"
  | String->print_string "string"
  | Fun(t1,t2)->(match t1 with
    | Int|Bool|TyVar _|String->print_type t1 prev_id; print_string " -> "; print_type t2 prev_id
    | _->print_char '('; print_type t1 prev_id; print_string ") ->"; print_type t2 prev_id)
  | List t->print_type t prev_id; print_string " list"
  | TyVar id->
    let new_id=id-prev_id-1 in
    print_string "'"; print_char (Char.chr (new_id mod 26+97));
    if new_id<26 then () else print_int (new_id/26)

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

let print_value value ty prev_id=(match value with
  | VarV(id,v)->print_string ("val "^id^" : "); print_type ty prev_id; print_string " = "; print_lit v
  | FunV _ | RecV _->print_string "- : "; print_type ty prev_id; print_string " = <fun>"
  | _->print_string " - : "; print_type ty prev_id; print_string " = "; print_lit value);
  print_newline()

let rec print_result trees env tyenv=match trees with
  | []->(env,tyenv)
  | h::t->let prev_id=tyvar_id() in (match h with
    | LetAnd defs -> 
      let now_env=env and now_tyenv=tyenv in 
      let (new_env,new_tyenv)=List.fold_left (fun (env,tyenv) e->
        let (ty,_,_)=ty_eval now_tyenv e in 
        let (value,_)=eval now_env e in 
        match value with
        | VarV(id,v) -> print_value value ty prev_id; ((id,v)::env,(id,ty)::tyenv)
        | _->err "invalid pattern") (env,tyenv) defs in print_result t new_env new_tyenv
    | _->
      let (ty,new_tyenv,_)=ty_eval tyenv h in
      let (value,new_env)=eval env h in print_value value ty prev_id; print_result t new_env new_tyenv)

let rec check_input input n=
  try
    match input.[n] with
    | ' '->check_input input (n-1)
    | ';'->if input.[n-1]=';' then true else false 
    | _->false
  with _->false

let get_code()=let input=ref (read_line()) in 
  let code=ref !input in 
  while not (check_input !input ((String.length !input)-1)) do 
    print_string "  "; input:=read_line(); code:= !code^" "^ !input
  done; !code


let rec cui env tyenv=print_string "# "; let line=get_code() in 
  if line="#quit;;" then exit 0
  else 
    try
      let ast=Parser.lalr_parse line in let (new_env,new_tyenv)=print_result (List.rev ast) env tyenv in cui new_env new_tyenv
    with e->print_string(Printexc.to_string e); print_newline(); cui env tyenv
