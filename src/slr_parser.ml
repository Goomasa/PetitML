open Syntax
open Slr

let err=Lexer.err

let slr_table=create_slr_table Mlr_reader.new_grammer

let head list=match list with
  | []->err "empty"
  | h::_->h

let rec pop list n=if n=0 then list else match list with
  | []->[]
  | _::t->pop t (n-1)

let search_table sym id=let state=List.find (fun x->x.id=id) slr_table in
  let accept={follow=Eps;action=Accept} in 
  if state.items=[accept] then Accept 
  else (List.find (
    fun x->match sym with
    | NT(Num(_)) -> x.follow=NT(Num(0))
    | _->x.follow=sym
  ) state.items).action

let slr_parse code=
  let rec parse stack tokens prev_token trees=match tokens with
  | Lexer.End -> 
    let action=search_table Eps (head stack) in
    (match action with
    | Accept->trees
    | _->err "syntax err")
  | Token(h,t)->
    let action=search_table (NT(h)) (head stack) in 
    match action with
    | Accept->trees@(parse [(head slr_table).id] tokens h [])
    | Shift(n)->parse (n::stack) t h trees
    | Reduce(sym,n,pk)->
      let popped=pop stack n in 
      let next_action=search_table sym (head popped) in 
      match next_action with
      | Shift(n)->parse (n::popped) tokens h (create_ast pk prev_token trees)
      | _->err "parser err"
  in parse [(head slr_table).id] (Lexer.tokenize code) SemiSemi []
