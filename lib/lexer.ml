type token_kind=Num of int|Eq|Plus|SemiSemi;;

type token_line=Node of token_kind*token_line|End;;

let sub_str s n=String.sub s n ((String.length s)-n);;
let char_to_int c=(Char.code c)-48;;
let is_digit c=let i=char_to_int c in i>=0&&i<=9;;

let rec get_num s n=let i=char_to_int s.[0] in 
  if i>=0 && i<=9 then get_num (sub_str s 1) (10*n+i)
  else (s,n)

let rec gen_tokens code=if code="" then End else
  match code.[0] with
  | '='->Node(Eq,gen_tokens (sub_str code 1))
  | '+'->Node(Plus,gen_tokens (sub_str code 1))
  | ';'->if code.[1]=';' then Node(SemiSemi,gen_tokens (sub_str code 2)) else exit 0
  | c->if is_digit c then let (s,n)=get_num code 0 in Node(Num(n),gen_tokens s) else exit 0