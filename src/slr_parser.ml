open Lexer
open Slr

(*
type symbol=T of string|NT of Lexer.token_kind|Eps
type prod={p_left:symbol;p_right:symbol list}
type grammer=prod list

type item={i_left:symbol;i_right:symbol list;dot:int;next:int option ref}
type 't state={items:'t list;id:int} (* state of DFA and column of slr-table*)

type action=Shift of int|Reduce of symbol*int 
type content={follow:symbol;action:action}
*)

let slr_table=Slr.create_slr_table Mlr_reader.new_grammer

let head list=match list with
  | []->err "empty"
  | h::_->h

let rec pop list n=if n=0 then list 
  else match list with
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
  let rec parse stack tokens=match tokens with
  | End -> 
    let action=search_table Eps (head stack) in
    (match action with
    | Accept->print_string "accepted!"
    | _->err "syntax err")
  | Token(h,t)->
    let action=search_table (NT(h)) (head stack) in 
    match action with
    | Accept->print_string "accepted!"; parse [(head slr_table).id] tokens
    | Shift(n)->parse (n::stack) t
    | Reduce(sym,n)->
      let popped=pop stack n in 
      let next_action=search_table sym (head popped) in 
      match next_action with
      | Shift(n)->parse (n::popped) tokens
      | _->err "parser err"
  in parse [(head slr_table).id] (tokenize code)
