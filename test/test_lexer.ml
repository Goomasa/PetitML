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
  | True->print_string "true "
  | False->print_string "false "
  | Let->print_string "let "
  | If->print_string "if "
  | Then->print_string "then "
  | Else->print_string "else "
  | Not->print_string "not "
  | In->print_string "in "
  | Fun->print_string "fun "
  | Arrow->print_string "->"
  | Rec->print_string "rec "
  | Semi->print_string "; "
  | LBracket->print_string "[ "
  | RBracket->print_string "] "
  | ColCol->print_string ":: "
  | Match->print_string "match "
  | With->print_string "with "
  | Bar->print_string "| "
  | End->print_string "$ "

let rec lexer_pp tokens=match tokens with
  |Token(head,tail)->pp_token head; print_newline(); lexer_pp tail
  |End->print_string "-End-"; print_newline() 

(*let ()=lexer_pp (tokenize "let x=1;;")*)
