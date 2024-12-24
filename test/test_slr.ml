open Petitml.Lexer
open Petitml.Syntax
open Petitml.Slr
open Petitml.Mlr_reader

let prod0={p_left=T("S");p_right=[T("A");NT(SemiSemi)];kind=PK_null}
and prod1={p_left=T("A");p_right=[T("B")];kind=PK_null}
and prod2={p_left=T("A");p_right=[T("B");NT(Plus);T("A")];kind=PK_add}
and prod3={p_left=T("B");p_right=[NT(LParen);T("A");NT(RParen)];kind=PK_null}
and prod4={p_left=T("B");p_right=[NT(Num(0))];kind=PK_lit}

let newGrammer=[prod0;prod1;prod2;prod3;prod4]

let pp_symbol symbol=match symbol with
  | T(str)->print_string (str^" ")
  | NT(nt)->(match nt with
    | Num(_) -> print_string "num "
    | _->Test_lexer.pp_token nt)
  | Eps->print_string "eps "

let rec pp_symbols symbols=match symbols with
  | []->print_string " "
  | h::t->pp_symbol h; pp_symbols t

let rec pp_items items=match items with
  | []->print_newline()
  | h::t->pp_symbol h.i_left; print_string " -> "; pp_symbols h.i_right; 
    print_string (", dot:"^(string_of_int h.dot)); print_string(" , next:");
    (match !(h.next) with
    | None->print_string("None")
    | Some(n)->print_string(string_of_int n));
    print_newline(); pp_items t

let rec pp_states pp_some states=match states with
  | []->()
  | h::t->print_string ("id:"^(string_of_int h.id)); print_newline(); pp_some h.items; pp_states pp_some t

let rec pp_first_follow firsts=match firsts with
  | []->print_newline()
  | h::t->pp_symbol h.sym; print_string(" : "); pp_symbols !(h.set); print_newline(); pp_first_follow t

let pp_prod_kind kind=match kind with
  | PK_null->print_string "PK_null"
  | PK_add->print_string "PK_add"
  | PK_lit->print_string "PK_lit"

let pp_action action=match action with
  | Shift(id)->print_string (("Shift:(")^(string_of_int id)^(")"))
  | Reduce(sym,num,pk)->print_string ("Reduce:("); pp_symbol sym; print_int num; print_string " "; pp_prod_kind pk;  print_char(')')
  | Accept->print_string("Accept")

let rec pp_contents items=match items with
  | []->print_newline()
  | h::t->print_string("follow:"); pp_symbol h.follow;
    print_string(", action:");pp_action h.action;
    print_newline(); pp_contents t

let ()=pp_states pp_contents (create_slr_table new_grammer)