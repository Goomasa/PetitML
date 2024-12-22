open Petitml.Mlr_reader
open Test_slr
open Petitml.Slr

let ()=
  let rec print code=match code with
  | [] -> ()
  | h::t->print_string h; print_newline(); print t
  in print mlr_div

let rec pp_grammer grammer=match grammer with
  | [] -> ()
  | h::t->pp_symbol h.p_left; print_string " -> "; pp_symbols h.p_right; print_newline(); pp_grammer t

let ()=pp_grammer new_grammer