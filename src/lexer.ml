type token_kind=
  | Num of int
  | Plus
  | LParen
  | RParen
  | SemiSemi

type token_line=Token of token_kind*token_line|End

exception Error of string
let err s = raise (Error s)
let head_str s n=String.sub s 0 n
let sub_str s n=String.sub s n ((String.length s)-n)
let char_to_int c=(Char.code c)-48
let is_digit c=let i=char_to_int c in i>=0&&i<=9

let rec get_num s n=let i=char_to_int s.[0] in 
  if i>=0 && i<=9 then get_num (sub_str s 1) (10*n+i)
  else (s,n)

let to_token str=if str="" then (None,"") else
   try
    if is_digit str.[0] then let (s,n)=get_num str 0 in (Some (Num(n)),s) else
    let token= match head_str str 2 with
    | ";;"->Some SemiSemi
    | _->raise Not_found
   in (token,sub_str str 2)
   with _ ->
    let token= match str.[0] with
    | ' '->None
    | '+'->Some Plus
    | '('->Some LParen
    | ')'->Some RParen
    | _-> err "syntax err" (* <- add handler for reserved-word *)
   in (token,sub_str str 1)

let rec tokenize code=let (token,s)=to_token code in 
  match token with
  | None->if s="" then End else tokenize s
  | Some t->Token(t,tokenize s)