type token_kind=
  | Int of int
  | Ident of string
  | String of string
  | Float of float
  | Char of char
  | Plus
  | Minus
  | Star
  | Slash
  | PlusDot
  | MinusDot
  | StarDot
  | SlashDot
  | LParen
  | RParen
  | SemiSemi
  | Equal
  | LAngle
  | LAngleEq
  | RAngleEq
  | RAngle
  | Angles
  | AndAnd
  | BarBar
  | True
  | False
  | Let
  | If
  | Then
  | Else
  | Not
  | In
  | Fun
  | Arrow
  | Rec
  | Semi
  | LBracket
  | RBracket
  | ColCol
  | Match
  | With
  | Bar
  | And
  | Tilde
  | At
  | Dot
  | End (* for lalr *)

type token_line=Token of token_kind*token_line|End

exception Error of string
let err s = raise (Error s)
let head_str s n=String.sub s 0 n
let sub_str s n=String.sub s n ((String.length s)-n)
let char_to_int c=(Char.code c)-48
let is_digit c=let i=char_to_int c in i>=0&&i<=9
let is_word c=let i=Char.code c in (i>=97&&i<=122)||(i>=65&&i<=90)||(i=95)

let rec get_num str num=
  try
    let i=char_to_int str.[0] in 
    if i>=0 && i<=9 then get_num (sub_str str 1) (10*num+i)
    else if Char.code str.[0]=46 then let (fnum,str)=get_float (sub_str str 1) 0.0 1.0 in (Some (Float (float_of_int num+.fnum)),str)
    else (Some (Int num),str)
  with _->(Some (Int num),str)
and get_float str num d=
  try 
    let i=char_to_int str.[0] in 
    if i>=0 && i<=9 then get_float (sub_str str 1) (num+.(float_of_int i)/.d/.10.0) (d/.10.0)
    else (num,str)
  with _->(num,str)

let rec get_keyword word str=
  try
    let i=Char.code str.[0] in 
    if (i>=97&&i<=122)||(i>=65&&i<=90)||(i=95)||(i>=48&&i<=57) then get_keyword (word^(Char.escaped str.[0])) (sub_str str 1)
    else (word,str)
  with _->(word,str)

let rec get_string word str=
  try
    let i=Char.code str.[0] in
    if i<>34 then get_string (word^(Char.escaped str.[0])) (sub_str str 1)
    else (Some (String word),sub_str str 1) 
  with _->err "invalid string"

let get_char str=
  try
    let i1=Char.code str.[0] and i2=Char.code str.[1] in 
    if i1<>39&&i2=39 then (Some (Char str.[0]),sub_str str 2)
    else err "invalid char"
  with _->err "invalid char"

let rec skip_comment str begc endc=
  try
    if begc=endc then (None,str)
    else 
      let i1=Char.code str.[0] and i2=Char.code str.[1] in 
      if i1=40&&i2=42 then skip_comment (sub_str str 2) (begc+1) endc
      else if i1=42&&i2=41 then skip_comment (sub_str str 2) begc (endc+1)
      else skip_comment (sub_str str 1) begc endc
  with _->err "no end of comment"

let to_token str=if str="" then (None,"") else
   try
    if head_str str 2="(*" then skip_comment (sub_str str 2) 1 0 
    else if is_digit str.[0] then get_num str 0 else
    let token= match head_str str 2 with
    | ";;"->Some SemiSemi
    | "&&"->Some AndAnd
    | "||"->Some BarBar
    | "<>"->Some Angles
    | "->"->Some Arrow
    | "::"->Some ColCol
    | "<="->Some LAngleEq
    | ">="->Some RAngleEq
    | "+."->Some PlusDot
    | "-."->Some MinusDot
    | "*."->Some StarDot
    | "/."->Some SlashDot
    | _->raise Not_found
   in (token,sub_str str 2)
   with _ ->
    if is_word str.[0] then let (w,s)=get_keyword "" str in 
      let token=match w with
      | "true"->Some True
      | "false"->Some False
      | "let"->Some Let
      | "if"->Some If
      | "then"->Some Then
      | "else"->Some Else
      | "not"->Some Not
      | "in"->Some In
      | "fun"->Some Fun
      | "rec"->Some Rec
      | "match"->Some Match
      | "with"->Some With
      | "and"->Some And
      | _->Some (Ident w)
    in (token,s)
    else if str.[0]='"' then get_string "" (sub_str str 1)
    else if str.[0]='\'' then get_char (sub_str str 1)
    else let token=match str.[0] with
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
    | ';'->Some Semi
    | '['->Some LBracket
    | ']'->Some RBracket
    | '|'->Some Bar
    | '^'->Some Tilde
    | '@'->Some At
    | '.'->Some Dot
    | _-> err "lexer err"
   in (token,sub_str str 1)

let rec tokenize code=let (token,s)=to_token code in 
  match token with
  | None->if s="" then End else tokenize s
  | Some t->Token(t,tokenize s)