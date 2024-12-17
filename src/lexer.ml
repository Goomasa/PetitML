type token_kind=
  | Num of int
  | Plus
  | LParen
  | RParen
  | SemiSemi

type token_line=Token of token_kind*token_line|End

exception Error of string
let err s = raise (Error s)

let sub_str s n=String.sub s n ((String.length s)-n)
let char_to_int c=(Char.code c)-48
let is_digit c=let i=char_to_int c in i>=0&&i<=9

let rec get_num s n=let i=char_to_int s.[0] in 
  if i>=0 && i<=9 then get_num (sub_str s 1) (10*n+i)
  else (s,n)

let rec tokenize code=if code="" then End else
  match code.[0] with
  | ' '->tokenize (sub_str code 1)
  | '+'->Token(Plus,tokenize (sub_str code 1))
  | '('->Token(LParen,tokenize (sub_str code 1))
  | ')'->Token(RParen,tokenize (sub_str code 1))
  | ';'->if String.length code>=2&&code.[1]=';' then Token(SemiSemi,tokenize (sub_str code 2)) else err "syntax err"
  | c->if is_digit c then let (s,n)=get_num code 0 in Token(Num(n),tokenize s) else err "syntax err"