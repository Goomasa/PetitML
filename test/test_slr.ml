open Petitml.Lexer
open Petitml.Slr

let prod0={p_left=T("S");p_right=[T("A");NT(SemiSemi)]}
and prod1={p_left=T("A");p_right=[T("B")]}
and prod2={p_left=T("A");p_right=[T("B");NT(Plus);T("A")]}
and prod3={p_left=T("B");p_right=[NT(LParen);T("A");NT(RParen)]}
and prod4={p_left=T("B");p_right=[NT(Num(0))]}

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
    print_string (", dot:"^(string_of_int h.dot)); print_newline(); pp_items t

let rec pp_states states=match states with
  | []->print_newline()
  | h::t->print_string ("id:"^(string_of_int h.id)); print_newline(); pp_items h.items; pp_states t

let ()=pp_states (calc_states newGrammer)

let rec pp_first_follow firsts=match firsts with
  | []->print_newline()
  | h::t->pp_symbol h.sym; print_string(" : "); pp_symbols !(h.set); print_newline(); pp_first_follow t

let ()=pp_first_follow (calc_follows newGrammer)