
type symbol=T of string|NT of Lexer.token_kind|Eps
type prod={p_left:symbol;p_right:symbol list}
type grammer=prod list

type item={i_left:symbol;i_right:symbol list;dot:int}
type 't state={items:'t list;id:int} (* state of DFA and column of slr-table*)

type action=Shift of int|Reduce of int (* shift->next_state_id , reduce->grammer index *)
type content={follow:symbol;action:action}

let head=function
  | []->None
  | h::_->Some h

let states_list states=List.rev_map (fun x->x.items) states

let state_id=let id=ref 0 in let count()= id:=!id+1;!id in count (* return 1,2,... *)

let closure base_items grammer=
  let rec extend items tmp=match tmp with
    | []->items
    | item::tail->match (List.nth_opt item.i_right item.dot) with
      | Some(T t)->let prods_from_t=List.filter (fun x->x.p_left=T(t)) grammer in
        let items_from_t=List.rev_map (fun x->{i_left=x.p_left;i_right=x.p_right;dot=0}) prods_from_t in 
        let new_items=List.filter (fun x->not(List.mem x items)) items_from_t in
        extend (items@new_items) (tail@new_items)
      | _->extend items tail
  in extend base_items base_items

let next_state_items items symbol grammer=
  let read_items=List.filter (fun x->List.nth_opt x.i_right x.dot=Some symbol) items in
  let new_items=List.rev_map (fun x->{i_left=x.i_left;i_right=x.i_right;dot=x.dot+1}) read_items in
  closure new_items grammer

let base_states grammer=
  let items=match grammer with
  | []->Lexer.err "empty grammer"
  | h::_->closure [{i_left=h.p_left;i_right=h.p_right;dot=0}] grammer
  in [{items=items;id=0}]

let calc_states grammer=
  let rec add states added_states=match added_states with
    | []->states
    | st_h::st_t->let symbols_opt=List.rev_map (fun x->List.nth_opt x.i_right x.dot) st_h.items in
      let symbols=List.filter (fun x->not(x=None)) symbols_opt in
      let rec add_one_state symbols states added_states =
        match symbols with
        | []->add states added_states
        | sym_h::sym_t->
          let next_items=next_state_items st_h.items (Option.get sym_h) grammer in
          if not(List.mem next_items (states_list states)) then 
            let new_state={items=next_items;id=state_id()} in add_one_state sym_t (new_state::states) (new_state::added_states)
          else add_one_state sym_t states added_states
      in add_one_state symbols states st_t
  in let base=base_states grammer in add base base

let rec conflict content table_col=match table_col with
  | []->false
  | h::t->if content.follow=h.follow&&content.action<>h.action then true
    else conflict content t

let rec uni_list list=match list with
  | []->[]
  | h::t->if List.mem h t then uni_list t else h::(uni_list t)

let uni_list_one a list=if List.mem a list then list else a::list

let rec uni_app l1 l2=match l1 with
  | [] -> l2
  | h::t->if List.mem h l2 then uni_app t l2 else uni_app t (h::l2)

let rec is_null l nulls=match l with
  | []->true
  | h::t->if List.mem h nulls then is_null t nulls else false

let rec calc_nulls grammer nulls=
  let tmp=uni_list (List.filter (fun x->x.p_right=[Eps]||is_null x.p_right nulls) grammer) in 
  let new_nulls=List.rev_map (fun x->x.p_left) tmp in 
  if List.length nulls=List.length new_nulls then nulls
  else calc_nulls grammer new_nulls

type constr_type=Follow of symbol|First of symbol|Sym of symbol
type 't constr_set={sym:symbol;set:'t list ref}

let rec first_constr_one constr prod_r nulls=match prod_r with
  | []->()
  | h::t->(match h with
    | T(_) -> (if List.mem h !(constr.set) then () else constr.set:=h:: !(constr.set));
      if List.mem h nulls then first_constr_one constr t nulls else ()
    | NT(_)->if List.mem h !(constr.set) then () else constr.set:=h:: !(constr.set)
    | Eps->())

let first_constraint grammer nulls=
  let terminals=uni_list (List.rev_map (fun x->x.p_left) grammer) in 
  let constraints=List.rev_map (fun x->{sym=x;set=ref []}) terminals in 
  let rec calc rest_g=(match rest_g with
  | []->constraints
  | h::t->let sym_constr=List.find (fun x->x.sym=h.p_left) constraints in 
    first_constr_one sym_constr h.p_right nulls; calc t)
  in calc grammer

let rec first_one sym set constraints=match set with
  | []->[]
  | h::t->(match h with
    | T(_) -> if sym=h then first_one sym t constraints else
      let sym_constr=List.find (fun x->x.sym=h) constraints in
      uni_app !(sym_constr.set) (first_one sym t constraints)
    | NT(_)->h::(first_one sym t constraints)
    | Eps->Lexer.err "internal err : invalid pattern")

let calc_firsts grammer nulls=let constraints=first_constraint grammer nulls in 
  let rec one_loop is_change constr=match constr with
  | []->is_change
  | h::t->let prev= !(h.set) in
    let new_set=first_one h.sym !(h.set) constraints in h.set:=new_set;
    if prev=new_set then one_loop is_change t else one_loop true t
  in let is_change=ref true in 
  while !is_change=true do 
    is_change:=one_loop false constraints
  done; constraints

let rec fol_constr_aux constr prod_rests nulls=match prod_rests with
  | []->()
  | h::t->(match h with
    | T(_)->constr.set:=uni_list_one (First(h)) !(constr.set);
      if List.mem h nulls then fol_constr_aux constr t nulls else ()
    | NT(_)->constr.set:=uni_list_one (Sym(h)) !(constr.set)
    | Eps->fol_constr_aux constr t nulls)

let rec fol_constr_fir constraints prod_r nulls=match prod_r with
  | []->()
  | h::t->(match h with
    | T(_) -> let constr=List.find (fun x->x.sym=h) constraints in 
      fol_constr_aux constr t nulls; fol_constr_fir constraints t nulls
    | _->fol_constr_fir constraints t nulls)

let fol_constr_fol constraints prod nulls=
  let rec aux rev_prod_r=match rev_prod_r with
  | [] -> ()
  | h::t->(match h with
    | T(_) -> let constr=List.find (fun x->x.sym=h) constraints in 
      constr.set:=uni_list_one (Follow(prod.p_left)) !(constr.set);
      if List.mem h nulls then aux t else ()
    | _->())
  in aux (List.rev prod.p_right)

let follow_constraint grammer nulls=
  let terminals=uni_list (List.rev_map (fun x->x.p_left) grammer) in 
  let constraints=List.rev_map (fun x->{sym=x;set=ref []}) terminals in 
  let rec calc rest_g=(match rest_g with
  | []->constraints
  | h::t->fol_constr_fir constraints h.p_right nulls;
    fol_constr_fol constraints h nulls; calc t)
  in calc grammer

let rec follow_one sym set constraints firsts=match set with
  | []->[]
  | h::t->(match h with
    | First(s)->let first=List.find (fun x->x.sym=s) firsts in 
      uni_app (List.rev_map (fun x->Sym(x)) !(first.set)) (follow_one sym t constraints firsts)
    | Follow(s)->if sym=s then follow_one sym t constraints firsts else
      let constr=List.find (fun x->x.sym=s) constraints in 
      uni_app !(constr.set) (follow_one sym t constraints firsts)
    | Sym(_)->h::(follow_one sym t constraints firsts))

let calc_follows grammer=
  let nulls=calc_nulls grammer [] in 
  let constraints=follow_constraint grammer nulls in 
  let firsts=calc_firsts grammer nulls in 
  let rec one_loop is_change constr=match constr with
  | [] -> is_change
  | h::t->let prev= !(h.set) in 
    let new_set=follow_one h.sym !(h.set) constraints firsts in h.set:=new_set;
    if prev=new_set then one_loop is_change t else one_loop true t 
  in let is_change=ref true in 
  while !is_change=true do
    is_change:=one_loop false constraints
  done;
  let to_syms=List.rev_map (fun x->match x with
    | Sym(s) -> s
    | _->Lexer.err "invalid content")
  in List.map (fun x->{sym=x.sym;set=ref (to_syms !(x.set))}) constraints