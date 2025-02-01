open Syntax
open Environment

type value=IntV of int
  | BoolV of bool
  | StringV of string
  | VarV of string*value
  | FunV of string*exp*(value env)
  | RecV of string*exp*(value env ref)
  | EmptyV
  | ListV of value*value

let err=Util.err

let rec append l1 l2=match l1 with
  | EmptyV->l2
  | ListV(h,t)->ListV(h,append t l2)
  | _->err "invalid append"

let bin_calc op val1 val2=match (op,val1,val2) with
  | (Add,IntV n1,IntV n2)->IntV (n1+n2)
  | (Sub,IntV n1,IntV n2)->IntV (n1-n2)
  | (Mul,IntV n1,IntV n2)->IntV (n1*n2)
  | (Div,IntV n1,IntV n2)->IntV (n1/n2)
  | (Eq, IntV n1,IntV n2)->BoolV (n1=n2)
  | (Eq, BoolV b1,BoolV b2)->BoolV (b1=b2)
  | (Neq,IntV n1,IntV n2)->BoolV (n1<>n2)
  | (Neq,BoolV b1,BoolV b2)->BoolV (b1<>b2)
  | (Large,IntV n1,IntV n2)->BoolV (n1>n2)
  | (Small,IntV n1,IntV n2)->BoolV (n1<n2)
  | (And,BoolV false,_)->BoolV false
  | (And,BoolV true,b)->b
  | (Or,BoolV true,_)->BoolV true
  | (Or,BoolV false,b)->b
  | (Cons,v1,v2)->ListV(v1,v2)
  | (App,v1,v2)->append v1 v2
  | (Cat,StringV s1,StringV s2)->StringV(s1^s2)
  | _->err "invalid type"

let rec eval env exp=match exp with
  | ILit n->(IntV n,env)
  | BLit b->(BoolV b,env)
  | SLit s->(StringV s,env)
  | Null->(EmptyV,env)
  | LLit(first,next)->
    let (fv,_)=eval env first in 
    let (v,_)=eval env next in (ListV(fv,v),env)
  | Ident id->let v=search_env id env in (v,env)
  | Var(id,e)->let (v,_)=eval env e in let value=VarV(id,v) in (value,(id,v)::env)
  | Bin(op,e1,e2)->
    let (v1,_)=eval env e1 in 
    let (v2,_)=eval env e2 in 
    (bin_calc op v1 v2,env)
  | If(e1,e2,e3)->(match eval env e1 with
    | (BoolV true,_) -> eval env e2
    | (BoolV false,_)->eval env e3
    | _->err "no condition")
  | Unary exp->(match eval env exp with
    | (IntV n,e)->(IntV (-n),e)
    | _->err "not int")
  | Let(e1,e2)->
    let (_,new_env)=eval env e1 in 
    let (v,_)=eval new_env e2 in (v,env)
  | Fun(args,e)->(match args with
    | Args(Ident arg_id,Null)->(FunV(arg_id,e,env),env)
    | Args(Ident arg_id,next)->(FunV(arg_id,Fun(next,e),env),env)
    | _->err "invalid args")
  | Rec(id,args,e)->(match args with
    | Args(Ident arg_id,Null)->(RecV(arg_id,e,ref env),env)
    | Args(Ident arg_id,next)->(RecV(arg_id,Rec(id,next,e),ref env),env)
    | _->err "invalid args")
  | Apply(Ident id,e)->
    let (v,_)=eval env (Ident id) in 
    (match v with
    | FunV(_,_,fenv)->
      let (funv,new_fenv)=eval_app v e fenv env in 
      let (retv,_)=eval new_fenv funv in (retv,env)
    | RecV(arg_id,next,fenv)->
      let (recv,new_fenv)=eval_app v e !fenv env in 
      let rec_env=(id,RecV(arg_id,next,fenv))::new_fenv in 
      let (retv,_)=eval rec_env recv in (retv,env)
    | _->err "not function")
  | Not exp->(match eval env exp with
    | (BoolV b,e)->(BoolV (not b),e)
    | _->err "not int")
  | Match(e1,e2,id1,id2,e3)->
    let (lv,_)=eval env e1 in 
    let (retv,_)=match lv with
    | EmptyV -> eval env e2
    | ListV(a,b)->eval ((id1,a)::(id2,b)::env) e3
    | _->err "invalid match"
    in (retv,env)
  | _->err("not implemented")
and eval_app funv apps fenv env=match apps with
  | h::[]->(match funv with
    | FunV(arg_id,e,_)|RecV(arg_id,e,_)->let (v,_)=eval env h in (e,(arg_id,v)::fenv)
    | _->err "invalid application")
  | h::t->(match funv with
    | FunV(arg_id,next,_)|RecV(arg_id,next,_)->
      let (v,_)=eval env h in
      let (nextfv,_)=eval env next in eval_app nextfv t ((arg_id,v)::fenv) env
    | _->err "invalid application")
  | _->err "invalid pattern"