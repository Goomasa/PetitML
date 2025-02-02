open Syntax
open Environment

let err=Util.err

type val_type=
  | Int
  | Float
  | Bool
  | String
  | Char
  | Fun of val_type*val_type   (* type1 -> type2 *)
  | List of val_type
  | TyVar of int

type subst_maps=int*val_type list

let tyvar_id =let id=ref 0 in let count()=let prev= !id in id:=!id+1; prev in count

let subst ty maps=
  let rec subst_one ty map=let (tv_id,img)=map in 
    match ty with
    | TyVar id->if id=tv_id then img else ty  (* TyVar tv_id -> img *)
    | Fun(t1,t2)->Fun(subst_one t1 map,subst_one t2 map)
    | List t->List(subst_one t map)
    | _->ty
  in List.fold_left subst_one ty maps

let rec occur_check tv_id ty=match ty with  (* ok -> true *)
  | TyVar id->if id=tv_id then false else true
  | Fun(t1,t2)->occur_check tv_id t1 || occur_check tv_id t2
  | List t->occur_check tv_id t
  | _->true

let rec unify eqs=match eqs with  (* not tail-call *)
  | []->[]
  | (Fun(t1,t2),Fun(t3,t4))::t->unify ((t1,t3)::(t2,t4)::t)
  | (List t1,List t2)::t->unify ((t1,t2)::t)
  | (TyVar tv_id,ty)::t| (ty,TyVar tv_id)::t->
    if ty=TyVar tv_id then unify t else
    if occur_check tv_id ty then 
      let map=(tv_id,ty) in
      let new_eqs=List.map (fun (x,y)->(subst x [map],subst y [map])) t in map::unify new_eqs
    else err "error in occur-check"
  | (t1,t2)::t->if t1=t2 then unify t else err "invalid type-equation"

let maps_to_eqs maps=
  let rec convert maps=match maps with
  | []->[]
  | (tv_id,ty)::t->(TyVar tv_id,ty)::convert t
  in convert maps

let bin_eqs op ty1 ty2=match op with
  | Add|Sub|Mul|Div->([(ty1,Int);(ty2,Int)],Int)
  | FAdd|FSub|FMul|FDiv->([(ty1,Float);(ty2,Float)],Float)
  | Small|Large|SmallEq|LargeEq->([(ty1,Int);(ty2,Int)],Bool)
  | Eq|Neq->([(ty1,ty2)],Bool)
  | And|Or->([(ty1,Bool);(ty2,Bool)],Bool)
  | Cons->([(ty2,List ty1)],List ty1)
  | App->let ret_ty=List (TyVar (tyvar_id())) in ([(ty1,ty2);(ty1,ret_ty)],ret_ty)
  | Cat->([(ty1,String);(ty2,String)],String)
  | Nth->([(ty1,String);(ty2,Int)],Char)

let rec ty_eval tyenv exp=match exp with
  | ILit _->(Int,tyenv,[])
  | FLit _->(Float,tyenv,[])
  | BLit _->(Bool,tyenv,[])
  | SLit _->(String,tyenv,[])
  | CLit _->(Char,tyenv,[])
  | Null->(List (TyVar (tyvar_id())),tyenv,[])
  | LLit(first,next)->
    let (ty1,_,map1)=ty_eval tyenv first in
    let (ty2,_,map2)=ty_eval tyenv next in 
    let new_eqs=(List ty1,ty2)::(maps_to_eqs map1)@(maps_to_eqs map2) in 
    let new_map=unify new_eqs in (subst ty2 new_map,tyenv,new_map)
  | Ident id->(search_env id tyenv,tyenv,[])
  | Var(id,e)->let (ty,env,_)=ty_eval tyenv e in (ty,(id,ty)::env,[])
  | Bin(op,e1,e2)->
    let (ty1,_,map1)=ty_eval tyenv e1  in 
    let (ty2,_,map2)=ty_eval tyenv e2 in 
    let (eqs,new_ty)=bin_eqs op ty1 ty2 in 
    let new_eqs=(maps_to_eqs map1)@(maps_to_eqs map2)@eqs in 
    let new_map=unify new_eqs in (new_ty,tyenv,new_map)
  | If(e1,e2,e3)->
    let (ty1,_,map1)=ty_eval tyenv e1 in 
    let (ty2,_,map2)=ty_eval tyenv e2 in 
    let (ty3,_,map3)=ty_eval tyenv e3 in 
    let new_eqs=(ty1,Bool)::(ty2,ty3)::(maps_to_eqs map1)@(maps_to_eqs map2)@(maps_to_eqs map3) in 
    let new_map=unify new_eqs in (subst ty2 new_map,tyenv,new_map)
  | Unary e->
    let (ty,_,map)=ty_eval tyenv e in 
    let new_eqs=(ty,Int)::(maps_to_eqs map) in 
    let new_map=unify new_eqs in (subst ty new_map,tyenv,new_map)
  | Not e->
    let (ty,_,map)=ty_eval tyenv e in 
    let new_eqs=(ty,Bool)::(maps_to_eqs map) in 
    let new_map=unify new_eqs in (subst ty new_map,tyenv,new_map)
  | Let(e1,e2)->
    let (_,env,map1)=ty_eval tyenv e1 in 
    let (ty,_,map2)=ty_eval env e2 in 
    let new_eqs=(maps_to_eqs map1)@(maps_to_eqs map2) in
    let new_map=unify new_eqs in (subst ty new_map,tyenv,new_map)
  | Fun(args,e)->
    let rec eval_arg args env=let domty=TyVar (tyvar_id()) in 
      match args with
      | Args(Ident id,Null)->let (ty,_,map)=ty_eval ((id,domty)::env) e in (Fun(subst domty map,ty),map)
      | Args(Ident id,next)->let (ty,map)=eval_arg next ((id,domty)::env) in (Fun(subst domty map,ty),map)
      | _->err "invalid args"
    in let (ty,new_map)=eval_arg args tyenv in (ty,tyenv,new_map)
  | Rec(id,args,e)->
    let ret_ty=TyVar (tyvar_id()) in 
    let rec eval_arg args env=let domty=TyVar (tyvar_id()) in 
      match args with
      | Args(Ident arg_id,Null)->(Fun(domty,ret_ty),(arg_id,domty)::env)
      | Args(Ident arg_id,next)->let (ty,new_env)=eval_arg next ((arg_id,domty)::env) in (Fun(domty,ty),new_env)
      | _->err "invalid args"
    in let (rec_ty,rec_env)=eval_arg args tyenv in 
    let (ty,_,map)=ty_eval ((id,rec_ty)::rec_env) e in 
    let new_eqs=(ret_ty,ty)::(maps_to_eqs map) in 
    let new_map=unify new_eqs in (subst rec_ty new_map,tyenv,map)
  | Apply(e1,e2)->
    let (fty,_,fmap)=ty_eval tyenv e1 in 
    let (ty,map)=tyeval_app fty e2 (maps_to_eqs fmap) tyenv in (ty,tyenv,map)
  | Match(e1,e2,id1,id2,e3)->
    let (ty1,_,map1)=ty_eval tyenv e1 in 
    let (ty2,_,map2)=ty_eval tyenv e2 in 
    let domty1=TyVar(tyvar_id()) and domty2=TyVar(tyvar_id()) in 
    let (ty3,_,map3)=ty_eval ((id1,domty1)::(id2,domty2)::tyenv) e3 in 
    let new_eqs=(ty1,List domty1)::(ty1,domty2)::(ty2,ty3)::(maps_to_eqs map1)@(maps_to_eqs map2)@(maps_to_eqs map3) in 
    let new_map=unify new_eqs in (subst ty2 new_map,tyenv,new_map)
  | _->err "not implemented"
and tyeval_app funty app eqs tyenv=match app with
  | []->let new_map=unify eqs in (subst funty new_map,new_map)
  | h::t->(match funty with
    | Fun(ty1,ty2)-> 
      let (ty,_,map)=ty_eval tyenv h in tyeval_app ty2 t ((ty1,ty)::(maps_to_eqs map)@eqs) tyenv
    | _->err "invalid application")
