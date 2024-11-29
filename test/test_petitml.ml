open Petitml.Lexer

let rec lexer_pp tokens=match tokens with
  |Node(node,line)->(match node with
    |Num(n)->print_string (string_of_int n); print_newline(); lexer_pp line
    |Eq->print_char '='; print_newline(); lexer_pp line
    |Plus->print_char '+'; print_newline(); lexer_pp line
    |SemiSemi->print_string ";;"; print_newline(); lexer_pp line)
  |End->print_string "-End-"

let ()=lexer_pp (gen_tokens "1+1;;")