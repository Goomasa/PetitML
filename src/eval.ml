open Parser

let rec eval=function
  | ILit(n)->n
  | Bin(_,e1,e2)->(eval e1)+(eval e2)