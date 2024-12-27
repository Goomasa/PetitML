type prod_kind=
  | PK_top
  | PK_null
  | PK_lit
  | PK_add
  | PK_sub
  | PK_mul
  | PK_div
  | PK_unary
  | PK_eq
  | PK_neq
  | PK_large
  | PK_small
  | PK_and
  | PK_or
  | PK_defv (* definition of variables *)

type bin_op=Add|Sub|Mul|Div|Eq|Neq|Large|Small

type exp=ILit of int
  | BLit of bool
  | Ident of string
  | Var of string*exp
  | Bin of bin_op*exp*exp
  | And of exp*exp
  | Or of exp*exp

type symbol=T of string|NT of Lexer.token_kind|Eps
type prod={mutable p_left:symbol;mutable p_right:symbol list;mutable kind:prod_kind}
type grammer=prod list

let to_prod_kind str=match str with
  | "PK_top"->PK_top
  | "PK_null"->PK_null
  | "PK_lit"->PK_lit
  | "PK_add"->PK_add
  | "PK_sub"->PK_sub
  | "PK_mul"->PK_mul
  | "PK_div"->PK_div
  | "PK_unary"->PK_unary
  | "PK_eq"->PK_eq
  | "PK_neq"->PK_neq
  | "PK_large"->PK_large
  | "PK_small"->PK_small
  | "PK_and"->PK_and
  | "PK_or"->PK_or
  | "PK_defv"->PK_defv
  | _->Lexer.err "no such kind"

let to_binOp prod_kind=match prod_kind with
  | PK_add->Add
  | PK_sub->Sub
  | PK_mul->Mul
  | PK_div->Div
  | PK_eq->Eq
  | PK_neq->Neq
  | PK_large->Large
  | _->Small

let ast_err ()=Lexer.err "invalid pattern"

let create_ast prod_kind token trees=match prod_kind with
  | PK_null->trees
  | PK_lit->(match token with
    | Lexer.Num(n)->ILit(n)::trees
    | True->BLit(true)::trees
    | False->BLit(false)::trees
    | Ident i->Ident(i)::trees
    | _->ast_err())
  | PK_unary->(match trees with
    | ILit(n)::t -> ILit(-n)::t
    | _->ast_err())
  | PK_and->(match trees with
    | h1::h2::t -> And(h2,h1)::t
    | _->ast_err())
  | PK_or->(match trees with
    | h1::h2::t -> Or(h2,h1)::t
    | _->ast_err())
  | PK_defv->(match trees with
    | h::(Ident i)::t->Var(i,h)::t
    | _->ast_err())
  | _->(match trees with
    | h1::h2::t -> Bin(to_binOp prod_kind,h2,h1)::t
    | _->ast_err())
