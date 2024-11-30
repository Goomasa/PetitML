open Petitml.Lexer
open Petitml.Parser

let tokens=tokenize "((1+((1+1)+1)));;";;
let ()=print_string (parse tokens)