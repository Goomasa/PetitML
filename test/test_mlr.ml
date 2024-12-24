open Petitml.Mlr_reader
open Petitml.Syntax

let ()=
  let rec print list=match list with
  | [] -> ()
  | h::t->print_string h; print_newline(); print t
  in print mlr_div

let pp_prod_kind kind=match kind with
  | PK_null->print_string "PK_null"
  | PK_add->print_string "PK_add"
  | PK_lit->print_string "PK_lit"

let rec pp_grammer grammer=match grammer with
  | [] -> ()
  | h::t->Test_slr.pp_symbol h.p_left; print_string " -> "; Test_slr.pp_symbols h.p_right;
  print_string " : "; pp_prod_kind h.kind; print_newline(); pp_grammer t

let ()=pp_grammer new_grammer