open Petitml.Lexer

let pp_token=function
  | Num n->print_string (string_of_int n)
  | Ident _->print_string "var "
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
  | Let->print_string "Let "
  | If->print_string "If "
  | Then->print_string "Then "
  | Else->print_string "Else "
  | Not->print_string "Not "
  | End->print_string "$ "

let rec lexer_pp tokens=match tokens with
  |Token(head,tail)->pp_token head; print_newline(); lexer_pp tail
  |End->print_string "-End-"; print_newline() 

(*let ()=lexer_pp (tokenize "let x=1;;")*)
