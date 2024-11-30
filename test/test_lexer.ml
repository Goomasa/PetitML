open Petitml.Lexer

let rec lexer_pp tokens=match tokens with
  |Token(head,tail)->(match head with
    |Num(n)->print_string (string_of_int n); print_newline(); lexer_pp tail
    |Plus->print_char '+'; print_newline(); lexer_pp tail
    |LParen->print_char '('; print_newline(); lexer_pp tail
    |RParen->print_char ')'; print_newline(); lexer_pp tail
    |SemiSemi->print_string ";;"; print_newline(); lexer_pp tail)
  |End->print_string "-End-"; print_newline()

let ()=lexer_pp (tokenize "(10)+100;;");;
