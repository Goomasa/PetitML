type prod_kind=
  | PK_top
  | PK_null
  | PK_lit
  | PK_add
  | PK_sub
  | PK_mul
  | PK_div
  | PK_unary
  | PK_not
  | PK_eq
  | PK_neq
  | PK_large
  | PK_small
  | PK_and
  | PK_or
  | PK_defv
  | PK_if
  | PK_let
  | PK_fun
  | PK_app
  | PK_args

type bin_op=Add|Sub|Mul|Div|Eq|Neq|Large|Small

type exp=ILit of int
  | BLit of bool
  | Ident of string
  | Var of string*exp
  | Bin of bin_op*exp*exp
  | And of exp*exp
  | Or of exp*exp
  | If of exp*exp*exp
  | Not of exp
  | Unary of exp
  | Let of exp*exp
  | Fun of exp*exp
  | Apply of exp*exp
  | Args of exp*exp
  | Null

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
  | "PK_not"->PK_not
  | "PK_eq"->PK_eq
  | "PK_neq"->PK_neq
  | "PK_large"->PK_large
  | "PK_small"->PK_small
  | "PK_and"->PK_and
  | "PK_or"->PK_or
  | "PK_defv"->PK_defv
  | "PK_if"->PK_if
  | "PK_let"->PK_let
  | "PK_fun"->PK_fun
  | "PK_app"->PK_app
  | "PK_args"->PK_args
  | _->Util.err "no such kind"

let to_binOp prod_kind=match prod_kind with
  | PK_add->Add
  | PK_sub->Sub
  | PK_mul->Mul
  | PK_div->Div
  | PK_eq->Eq
  | PK_neq->Neq
  | PK_large->Large
  | _->Small

let ast_err id=Util.err ("ast: invalid pattern >> "^(string_of_int id))

let create_ast prod_kind token trees=match prod_kind with
  | PK_null->trees
  | PK_lit->(match token with
    | Lexer.Num(n)->ILit(n)::trees
    | True->BLit(true)::trees
    | False->BLit(false)::trees
    | Ident i->Ident(i)::trees
    | _->ast_err 0)
  | PK_unary->(match trees with
    | h::t->Unary h::t
    | _->ast_err 1)
  | PK_not->(match trees with
    | h::t -> Not h::t
    | _->ast_err 2)
  | PK_and->(match trees with
    | h1::h2::t -> And(h2,h1)::t
    | _->ast_err 3)
  | PK_or->(match trees with
    | h1::h2::t -> Or(h2,h1)::t
    | _->ast_err 4)
  | PK_defv->(match trees with
    | h1::h2::(Ident i)::t->Var(i,Fun(h2,h1))::t
    | h::(Ident i)::t->Var(i,h)::t
    | _->ast_err 5)
  | PK_if->(match trees with
    | h1::h2::h3::t->If(h3,h2,h1)::t
    | _->ast_err 6)
  | PK_let->(match trees with
    | h1::h2::h3::(Ident i)::t->Let(Var(i,Fun(h3,h2)),h1)::t
    | h1::h2::(Ident i)::t->Let(Var(i,h2),h1)::t
    | _->ast_err 7)
  | PK_fun->(match trees with
    | h1::h2::t->Fun(h2,h1)::t
    | _->ast_err 8)
  | PK_app->(match trees with
    | h1::h2::t->Apply(h2,h1)::t
    | _->ast_err 9)
  | PK_args->(match trees with
    | h1::Args(a,b)::t->Args(h1,Args(a,b))::t
    | h::t->Args(h,Null)::t
    | _->ast_err 10)
  | _->(match trees with
    | h1::h2::t -> Bin(to_binOp prod_kind,h2,h1)::t
    | _->ast_err 11)
