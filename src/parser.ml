(*
0: S -> A ;;
1: A -> B
2: A -> B + A
3: B -> ( A )
4: B -> Num
*)

open Lexer

type bin_op=Add
type exp=ILit of int
  | Bin of bin_op*exp*exp

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

let reduce stack g_id trees =let (sym,num)=List.nth grammer g_id in match g_id with
  | 0->(sym,pop stack num,trees)
  | 1->(sym,pop stack num,trees)
  | 2->let new_trees=match trees with
    | h1::h2::t -> Bin(Add,h2,h1)::t
    | _->err "internal err : invalid trees" in (sym,pop stack num, new_trees)
  | 3->(sym,pop stack num,trees)
  | 4->(sym,pop stack num,trees)
  | _->err "internal err"

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
  let rec slr_parse tokens stack trees=let state=list_head stack in if state=4 then list_head trees else
    match tokens with
    | End->err "syntax err"
    | Token(head,tail)->let op=search_table state (T(head)) in 
      match op with
      | Shift(s)->(match head with
        | Num(n) -> slr_parse tail (s::stack) (ILit(n)::trees)
        | _->slr_parse tail (s::stack) trees)
      | Reduce(id)->let (sym,new_stack,new_trees)=reduce stack id trees in 
        let new_state=trans (list_head new_stack) sym in slr_parse tokens (new_state::new_stack) new_trees
      | _->err "internal err : impossible Trans"
  in slr_parse tokens [0] []