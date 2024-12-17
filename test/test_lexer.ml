open Petitml.Lexer

let pp_token=function
  |Num(n)->print_string (string_of_int n)
  |Plus->print_char '+'
  |LParen->print_char '('
  |RParen->print_char ')'
  |SemiSemi->print_string ";;" 

let rec lexer_pp tokens=match tokens with
  |Token(head,tail)->pp_token head; print_newline(); lexer_pp tail
  |End->print_string "-End-"; print_newline() 

(* let ()=lexer_pp (tokenize "(10)+100;;");; *)
