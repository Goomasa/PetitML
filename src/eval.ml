open Syntax
open Environment

type value=IntV of int
  | BoolV of bool
  | VarV of string*value
  | FunV of string*exp*(value env)
  | RecV of string*exp*(value env ref)
  | EmptyV
  | ListV of value*value

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
  | (And,BoolV false,_)->BoolV false
  | (And,BoolV true,b)->b
  | (Or,BoolV true,_)->BoolV true
  | (Or,BoolV false,b)->b
  | (Cons,v1,v2)->ListV(v1,v2)
  | _->err "invalid type"

let rec eval exp env=match exp with
  | ILit n->(IntV n,env)
  | BLit b->(BoolV b,env)
  | Null->(EmptyV,env)
  | LLit(first,next)->
    let (fv,_)=eval first env in 
    let (v,_)=eval next env in (ListV(fv,v),env)
  | Ident id->let v=search_env id env in (v,env)
  | Var(id,e)->let (v,_)=eval e env in let value=VarV(id,v) in (value,(id,v)::env)
  | Bin(op,e1,e2)->
    let (v1,_)=eval e1 env in 
    let (v2,_)=eval e2 env in 
    (bin_calc op v1 v2,env)
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
    | Args(Ident arg_id,Null)->(FunV(arg_id,e,env),env)
    | Args(Ident arg_id,next)->(FunV(arg_id,Fun(next,e),env),env)
    | _->err "invalid args")
  | Rec(id,args,e)->(match args with
    | Args(Ident arg_id,Null)->(RecV(arg_id,e,ref env),env)
    | Args(Ident arg_id,next)->(RecV(arg_id,Rec(id,next,e),ref env),env)
    | _->err "invalid args")
  | Apply(Ident id,e)->
    let (v,_)=eval (Ident id) env in 
    (match v with
    | FunV(_,_,fenv)->
      let (funv,new_fenv)=eval_app v e fenv env in 
      let (retv,_)=eval funv new_fenv in (retv,env)
    | RecV(arg_id,next,fenv)->
      let (recv,new_fenv)=eval_app v e !fenv env in 
      let rec_env=(id,RecV(arg_id,next,fenv))::new_fenv in 
      let (retv,_)=eval recv rec_env in (retv,env)
    | _->err "not function")
  | Not exp->(match eval exp env with
    | (BoolV b,e)->(BoolV (not b),e)
    | _->err "not int")
  | Match(e1,e2,id1,id2,e3)->
    let (lv,_)=eval e1 env in 
    let (retv,_)=match lv with
    | EmptyV -> eval e2 env
    | ListV(a,b)->eval e3 ((id1,a)::(id2,b)::env)
    | _->err "invalid match"
    in (retv,env)
  | _->err("not implemented")
and eval_app funv apps fenv env=match apps with
  | h::[]->(match funv with
    | FunV(arg_id,e,_)|RecV(arg_id,e,_)->let (v,_)=eval h env in (e,(arg_id,v)::fenv)
    | _->err "invalid application")
  | h::t->(match funv with
    | FunV(arg_id,next,_)|RecV(arg_id,next,_)->
      let (v,_)=eval h env in
      let (nextfv,_)=eval next env in eval_app nextfv t ((arg_id,v)::fenv) env
    | _->err "invalid application")
  | _->err "invalid pattern"