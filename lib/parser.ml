(*
0: S -> A ;;
1: A -> B
2: A -> B + A
3: B -> ( A )
4: B -> Num
*)

open Lexer

type ast_kind=BinTree of token_kind*ast_kind*ast_kind|UniTree of token_kind*ast_kind|ValNode of token_kind|EndNode;;

type slr_state=int;;
type symbol=T of token_kind|S|A|B
type operation=Shift of slr_state|Reduce of int|Trans of slr_state

let print_symbol=function
  |T(_)->print_string "terminal"
  |S->print_char 'S'
  |A->print_char 'A'
  |B->print_char 'B'

let grammer=[(S,2);(A,1);(A,3);(B,3);(B,1)] (* <- int...length of symbol (ex S -> A ;; => (S,2) *)
let slr_table=[[(T(LParen),Shift(3));(T(Num(0)),Shift(8));(A,Trans(1));(B,Trans(2))]
              ;[(T(SemiSemi),Shift(4))]
              ;[(T(Plus),Shift(5));(T(RParen),Reduce(1));(T(SemiSemi),Reduce(1))]
              ;[(T(LParen),Shift(3));(T(Num(0)),Shift(8));(A,Trans(6));(B,Trans(2))]
              ;[]
              ;[(T(LParen),Shift(3));(T(Num(0)),Shift(8));(A,Trans(9));(B,Trans(2))]
              ;[(T(RParen),Shift(7))]
              ;[(T(Plus),Reduce(3));(T(RParen),Reduce(3));(T(SemiSemi),Reduce(3))]
              ;[(T(Plus),Reduce(4));(T(RParen),Reduce(4));(T(SemiSemi),Reduce(4))]
              ;[(T(RParen),Reduce(2));(T(SemiSemi),Reduce(2))]] (*[[state 0];[state 1];...;[state n]]*)

let rec pop stack n=if n=0 then stack 
  else match stack with
  | []->[]
  | _::t->pop t (n-1)

let list_head=function
  | []->err "empty list"
  | h::_->h

let reduce stack id=let (sym,num) = List.nth grammer id in (sym,pop stack num)
let search_table state sym=let rec search list=match list with
  | []->err "internal err : not found in table"
  | h::t->let (s,op)=h in match sym with
    | T(Num(_))->if s=T(Num(0)) then op else search t
    | _->if s=sym then op else search t in search (List.nth slr_table state)

let trans state sym=let op=search_table state sym in match op with
  | Trans(n)->n
  |_->err "internal err : invalid sym"

(*This regards stack as record of state*)
let parse tokens=
  let rec slr_parse tokens stack=let state=list_head stack in if state=4 then "ok" else
    match tokens with
    | End->err "syntax err"
    | Token(head,tail)->let op=search_table state (T(head)) in 
      match op with
      | Shift(s)->slr_parse tail (s::stack)
      | Reduce(id)->let (sym,new_stack)=reduce stack id in 
        let new_state=trans (list_head new_stack) sym in slr_parse tokens (new_state::new_stack)
      | _->err "internal err : impossible Trans"
  in slr_parse tokens [0]