type prod_kind=
  | PK_null
  | PK_lit
  | PK_add

let to_prod_kind str=match str with
  | "PK_null"->PK_null
  | "PK_lit"->PK_lit
  | "PK_add"->PK_add
  | _->Lexer.err "no such kind"

type bin_op=Add

type exp=ILit of int
  | Bin of bin_op*exp*exp

type symbol=T of string|NT of Lexer.token_kind|Eps
type prod={mutable p_left:symbol;mutable p_right:symbol list;mutable kind:prod_kind}
type grammer=prod list

let rec pop stack n=if n=0 then stack 
  else match stack with
  | []->[]
  | _::t->pop t (n-1)

let create_ast prod_kind token trees=match prod_kind with
  | PK_null->trees
  | PK_add->(match trees with
    | h1::h2::t -> Bin(Add,h2,h1)::t
    | _->Lexer.err "internal err : invalid trees")
  | PK_lit->(match token with
    | Lexer.Num(n)->ILit(n)::trees
    | _->Lexer.err "invalid pattern")
