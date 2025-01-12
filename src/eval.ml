open Syntax
open Environment

let err=Util.err

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
  | _->err "invalid type"

let rec eval exp env=match exp with
  | ILit n->(IntV n,env)
  | BLit b->(BoolV b,env)
  | Ident id->let v=search_env id env in (v,env)
  | Var(id,e)->let (v,_)=eval e env in let value=Var(id,v) in (value,value::env)
  | Bin(op,e1,e2)->
    let (v1,_)=eval e1 env in 
    let (v2,_)=eval e2 env in 
    (bin_calc op v1 v2,env)
  | And(e1,e2)->(match eval e1 env with
    | (BoolV true,_) -> eval e2 env
    | _->(BoolV false,env))
  | Or(e1,e2)->(match eval e1 env with
    | (BoolV true,_) -> (BoolV true,env)
    | _->eval e2 env)
  | If(e1,e2,e3)->(match eval e1 env with
    | (BoolV true,_) -> eval e2 env
    | (BoolV false,_)->eval e3 env
    | _->err "no condition")
  | Unary exp->(match eval exp env with
    | (IntV n,e)->(IntV (-n),e)
    | _->err "not int")
  | Let(e1,e2)->
    let (_,new_env)=eval e1 env in 
    let (v,_)=eval e2 new_env in (v,env)
  | Fun(args,e)->(match args with
    | Args(Ident id,Null)->(FunV(id,e,env),env)
    | Args(Ident id,next)->(FunV(id,Fun(next,e),env),env)
    | _->err "invalid args")
  | Apply(e1,e2)->(match eval e1 env with
    | (FunV(id,e,fenv),_)->
      let (argv,_)=eval e2 env in 
      let (retv,_)=eval e (Var(id,argv)::fenv) in (retv,env)
    | _->err "not function")
  | Not exp->(match eval exp env with
    | (BoolV b,e)->(BoolV (not b),e)
    | _->err "not int")
  | _->err("not implemented")