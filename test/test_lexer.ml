open Petitml.Lexer

let pp_token=function
  | Num n->print_string (string_of_int n)
  | Ident s->print_string s
  | Plus->print_string "+ "
  | Minus->print_string "- "
  | Star->print_string "* "
  | Slash->print_string "/ "
  | LParen->print_string "( "
  | RParen->print_string ") "
  | SemiSemi->print_string ";; " 
  | AndAnd->print_string "&& "
  | BarBar->print_string "|| "
  | Angles->print_string "<> "
  | LAngle->print_string "< "
  | RAngle->print_string "> "
  | Equal->print_string "= "
  | True->print_string "True "
  | False->print_string "False "

let rec lexer_pp tokens=match tokens with
  |Token(head,tail)->pp_token head; print_newline(); lexer_pp tail
  |End->print_string "-End-"; print_newline() 

(*let ()=lexer_pp (tokenize "true;;")*)
