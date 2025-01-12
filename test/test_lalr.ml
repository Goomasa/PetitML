open Petitml.Syntax
open Petitml.Mlr_reader
open Petitml.Lalr

let prod0={p_left=T("S'");p_right=[T("S");NT(SemiSemi)];kind=PK_top}
and prod1={p_left=T("S");p_right=[T("V");NT(Equal);T("E")];kind=PK_eq}
and prod2={p_left=T("S");p_right=[T("E")];kind=PK_null}
and prod3={p_left=T("V");p_right=[NT(Star);T("E")];kind=PK_unary}
and prod4={p_left=T("V");p_right=[NT(Num(0))];kind=PK_lit}
and prod5={p_left=T("E");p_right=[T("V")];kind=PK_null}

let newGrammer2=[prod0;prod1;prod2;prod3;prod4;prod5]

let pp_symbol symbol=match symbol with
  | T(str)->print_string (str^" ")
  | NT(nt)->(match nt with
    | Num(_) -> print_string "num "
    | _->Test_lexer.pp_token nt)
  | Eps->print_string "eps "

let rec pp_symbols symbols=match symbols with
  | []->print_string " "
  | h::t->pp_symbol h; pp_symbols t

let pp_item item=pp_symbol item.i_left; print_string " -> "; pp_symbols item.i_right; 
    print_string (", dot:"^(string_of_int item.dot)); print_string(" , next:");
    (match !(item.next) with
    | None->print_string("None")
    | Some(n)->print_string(string_of_int n));
    print_string " , ahead:"; pp_symbols !(item.ahead)

let rec pp_items items=match items with
  | []->print_newline()
  | h::t->pp_item h; print_newline(); pp_items t

let rec pp_kernel kernel=match kernel with
  | []->print_newline()
  | h::t->print_string "id: "; print_int h.id; print_string " item: "; pp_item h.items; print_newline(); pp_kernel t

let rec pp_kernel_item kernel_item=match kernel_item with
  | []->print_newline()
  | h::t->pp_item h; print_newline(); pp_kernel_item t

let rec pp_spread spread=match spread with
  | []->print_newline()
  | []::t->print_string "nothing"; print_newline(); pp_spread t
  | h::t->pp_kernel_item h; pp_spread t

let pp_action action=match action with
  | Shift(id)->print_string (("Shift:(")^(string_of_int id)^(")"))
  | Reduce(sym,num,_)->print_string ("Reduce:("); pp_symbol sym; print_int num; print_char(')')
  | Accept->print_string("Accept")

let rec pp_states pp_some states=match states with
  | []->()
  | h::t->print_string ("id:"^(string_of_int h.id)); print_newline(); pp_some h.items; pp_states pp_some t

let rec pp_contents items=match items with
  | []->print_newline()
  | h::t->print_string("follow:"); pp_symbol h.follow;
    print_string(", action:");pp_action h.action;
    print_newline(); pp_contents t

(*let ()=pp_states pp_items (calc_lr0_states new_grammer)*)
let ()=pp_states pp_contents (create_lalr_table new_grammer)