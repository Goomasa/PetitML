open Syntax

let rec eval=function
  | ILit(n)->n
  | Bin(op,e1,e2)->(match op with
    | Add->(eval e1)+(eval e2)
    | Sub->(eval e1)-(eval e2)
    | Mul->(eval e1)*(eval e2)
    | Div->(eval e1)/(eval e2))