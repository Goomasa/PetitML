open Petitml.Lexer

let pp_token=function
  |Num(n)->print_string (string_of_int n)
  |Plus->print_string "+ "
  |LParen->print_string "( "
  |RParen->print_string ") "
  |SemiSemi->print_string ";; " 

let rec lexer_pp tokens=match tokens with
  |Token(head,tail)->pp_token head; print_newline(); lexer_pp tail
  |End->print_string "-End-"; print_newline() 

(*let ()=lexer_pp (tokenize "1+ 1;;");;*)
