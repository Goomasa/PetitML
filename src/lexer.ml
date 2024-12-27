type token_kind=
  | Num of int
  | Ident of string
  | Plus
  | Minus
  | Star
  | Slash
  | LParen
  | RParen
  | SemiSemi
  | Equal
  | LAngle
  | RAngle
  | Angles
  | AndAnd
  | BarBar
  | True
  | False

type token_line=Token of token_kind*token_line|End

exception Error of string
let err s = raise (Error s)
let head_str s n=String.sub s 0 n
let sub_str s n=String.sub s n ((String.length s)-n)
let char_to_int c=(Char.code c)-48
let is_digit c=let i=char_to_int c in i>=0&&i<=9
let is_word c=let i=Char.code c in (i>=97&&i<=122)||(i>=65&&i<=90)||(i=95)

let rec get_num s n=
  try
    let i=char_to_int s.[0] in 
    if i>=0 && i<=9 then get_num (sub_str s 1) (10*n+i)
    else (s,n)
  with _->(s,n)

let rec get_word word str=
  try
    let i=Char.code str.[0] in 
    if (i>=97&&i<=122)||(i>=65&&i<=90)||(i=95) then get_word (word^(Char.escaped str.[0])) (sub_str str 1)
    else (word,str)
  with _->(word,str)

let to_token str=if str="" then (None,"") else
   try
    if is_digit str.[0] then let (s,n)=get_num str 0 in (Some (Num(n)),s) else
    let token= match head_str str 2 with
    | ";;"->Some SemiSemi
    | "&&"->Some AndAnd
    | "||"->Some BarBar
    | "<>"->Some Angles
    | _->raise Not_found
   in (token,sub_str str 2)
   with _ ->
    if is_word str.[0] then let (w,s)=get_word "" str in 
      let token=match w with
      | "true"->Some True
      | "false"->Some False
      | _->Some (Ident w)
    in (token,s)
    else let token= match str.[0] with
    | ' '->None
    | '+'->Some Plus
    | '-'->Some Minus
    | '*'->Some Star
    | '/'->Some Slash
    | '('->Some LParen
    | ')'->Some RParen
    | '='->Some Equal
    | '<'->Some LAngle
    | '>'->Some RAngle
    | _-> err "lexer err"
   in (token,sub_str str 1)

let rec tokenize code=let (token,s)=to_token code in 
  match token with
  | None->if s="" then End else tokenize s
  | Some t->Token(t,tokenize s)