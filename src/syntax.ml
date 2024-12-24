type prod_kind=
  | PK_null
  | PK_lit
  | PK_add
  | PK_sub
  | PK_mul
  | PK_div

type bin_op=Add|Sub|Mul|Div

type exp=ILit of int
  | Bin of bin_op*exp*exp

type symbol=T of string|NT of Lexer.token_kind|Eps
type prod={mutable p_left:symbol;mutable p_right:symbol list;mutable kind:prod_kind}
type grammer=prod list

let to_prod_kind str=match str with
  | "PK_null"->PK_null
  | "PK_lit"->PK_lit
  | "PK_add"->PK_add
  | "PK_sub"->PK_sub
  | "PK_mul"->PK_mul
  | "PK_div"->PK_div
  | _->Lexer.err "no such kind"

let to_binOp prod_kind=match prod_kind with
  | PK_add->Add
  | PK_sub->Sub
  | PK_mul->Mul
  | _->Div

let create_ast prod_kind token trees=match prod_kind with
  | PK_null->trees
  | PK_add|PK_sub|PK_mul|PK_div->(match trees with
    | h1::h2::t -> Bin(to_binOp prod_kind,h2,h1)::t
    | _->Lexer.err "internal err : invalid trees")
  | PK_lit->(match token with
    | Lexer.Num(n)->ILit(n)::trees
    | _->Lexer.err "invalid pattern")
