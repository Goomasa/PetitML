open Syntax
open Lalr

let err=Util.err

let table=create_lalr_table Mlr_reader.new_grammer

let head=Util.head

let search_table sym id=
  try
    let state=List.find (fun x->x.id=id) table in
    let accept={follow=Eps;action=Accept} in 
    if state.items=[accept] then Accept 
    else (List.find (
      fun x->match sym with
      | NT(Num _) -> x.follow=NT(Num 0)
      | NT(Ident _)-> x.follow=NT(Ident "")
      | _->x.follow=sym
    ) state.items).action
  with _-> err ("syntax err: "^(string_of_int id))

let lalr_parse code=
  let rec parse stack tokens prev_token trees=match tokens with
  | Lexer.End -> 
    let action=search_table Eps (head stack) in
    (match action with
    | Accept->trees
    | _->err "not accepted")
  | Token(h,t)->
    let action=search_table (NT(h)) (head stack) in 
    match action with
    | Accept->trees@(parse [(head table).id] tokens h [])
    | Shift(n)->parse (n::stack) t h trees
    | Reduce(sym,n,pk)->
      let popped=Util.pop stack n in 
      let next_action=search_table sym (head popped) in 
      match next_action with
      | Shift(n)->parse (n::popped) tokens h (create_ast pk prev_token trees)
      | _->err "parser err"
  in parse [(head table).id] (Lexer.tokenize code) SemiSemi []
