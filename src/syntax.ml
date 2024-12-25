type prod_kind=
  | PK_null
  | PK_lit
  | PK_add
  | PK_sub
  | PK_mul
  | PK_div
  | PK_unary
  | PK_eq
  | PK_large
  | PK_small

type bin_op=Add|Sub|Mul|Div|Eq|Large|Small

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
  | "PK_unary"->PK_unary
  | "PK_eq"->PK_eq
  | "PK_large"->PK_large
  | "PK_small"->PK_small
  | _->Lexer.err "no such kind"

let to_binOp prod_kind=match prod_kind with
  | PK_add->Add
  | PK_sub->Sub
  | PK_mul->Mul
  | PK_div->Div
  | PK_eq->Eq
  | PK_large->Large
  | _->Small

let create_ast prod_kind token trees=match prod_kind with
  | PK_null->trees
  | PK_lit->(match token with
    | Lexer.Num(n)->ILit(n)::trees
    | _->Lexer.err "invalid pattern")
  | PK_unary->(match trees with
    | ILit(n)::t -> ILit(-n)::t
    | _->Lexer.err "invalid pattern")
  | _->(match trees with
    | h1::h2::t -> Bin(to_binOp prod_kind,h2,h1)::t
    | _->Lexer.err "internal err : invalid trees")
