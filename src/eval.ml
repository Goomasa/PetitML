open Syntax

type value=IntV of int|BoolV of bool

let bin_calc op val1 val2=match (op,val1,val2) with
  | (Add,IntV n1,IntV n2)->IntV (n1+n2)
  | (Sub,IntV n1,IntV n2)->IntV (n1-n2)
  | (Mul,IntV n1,IntV n2)->IntV (n1*n2)
  | (Div,IntV n1,IntV n2)->IntV (n1/n2)
  | (Eq,IntV n1,IntV n2)->BoolV (n1=n2)
  | (Large,IntV n1,IntV n2)->BoolV (n1>n2)
  | (Small,IntV n1,IntV n2)->BoolV (n1<n2)
  | _->Lexer.err "invalid type"

let rec eval=function
  | ILit(n)->IntV n
  | Bin(op,e1,e2)->bin_calc op (eval e1) (eval e2)