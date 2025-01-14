open Syntax

type value=IntV of int
  | BoolV of bool
  | VarV of string*value
  | FunV of string*exp*(value list)
  | RecV of string*exp*(value list ref)
  | EmptyV
  | ListV of value*value

type env=value list

let rec search_env id env=match env with
  | [] -> Util.err ("undefined variable: "^id)
  | VarV(s,e)::t->if id=s then e else search_env id t
  | _::t->search_env id t