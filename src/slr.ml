
type symbol=T of string|NT of Lexer.token_kind|Eps
type prod={p_left:symbol;p_right:symbol list}
type grammer=prod list

type item={i_left:symbol;i_right:symbol list;dot:int}
type state={items:item list;s_id:int}

type action=Shift of int|Reduce of int (* shift->next_state_id , reduce->grammer index *)
type content={follow:symbol;action:action}
type table_col={contents:content list;t_id:int}

let head=function
  | []->None
  | h::_->Some h

let states_list states=List.map (fun x->x.items) states

let state_id=let id=ref 0 in let count()= id:=!id+1;!id in count (* return 1,2,... *)

let closure base_items grammer=
  let rec extend items tmp=match tmp with
    | []->items
    | item::tail->match (List.nth_opt item.i_right item.dot) with
      | Some(T t)->let prods_from_t=List.filter (fun x->x.p_left=T(t)) grammer in
        let items_from_t=List.map (fun x->{i_left=x.p_left;i_right=x.p_right;dot=0}) prods_from_t in 
        let new_items=List.filter (fun x->not(List.mem x items)) items_from_t in
        extend (items@new_items) (tail@new_items)
      | _->extend items tail
  in extend base_items base_items

let next_state_items items symbol grammer=
  let read_items=List.filter (fun x->List.nth_opt x.i_right x.dot=Some symbol) items in
  let new_items=List.map (fun x->{i_left=x.i_left;i_right=x.i_right;dot=x.dot+1}) read_items in
  closure new_items grammer

let base_states grammer=
  let items=match grammer with
  | []->Lexer.err "empty grammer"
  | h::_->closure [{i_left=h.p_left;i_right=h.p_right;dot=0}] grammer
  in [{items=items;s_id=0}]

let calc_states grammer=
  let rec add states added_states=match added_states with
    | []->states
    | st_h::st_t->let symbols_opt=List.map (fun x->List.nth_opt x.i_right x.dot) st_h.items in
      let symbols=List.filter (fun x->not(x=None)) symbols_opt in
      let rec add_one_state symbols states added_states =
        match symbols with
        | []->add states added_states
        | sym_h::sym_t->
          let next_items=next_state_items st_h.items (Option.get sym_h) grammer in
          if not(List.mem next_items (states_list states)) then 
            let new_state={items=next_items;s_id=state_id()} in add_one_state sym_t (new_state::states) (new_state::added_states)
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

let rec is_null l nulls=match l with
  | []->true
  | h::t->if List.mem h nulls then is_null t nulls else false

let rec calc_nulls grammer nulls=
  let tmp=uni_list (List.filter (fun x->x.p_right=[Eps]||is_null x.p_right nulls) grammer) in 
  let new_nulls=List.map (fun x->x.p_left) tmp in 
  if List.length nulls=List.length new_nulls then nulls
  else calc_nulls grammer new_nulls

type first={symbol:symbol;first_set:symbol list ref}

let rec first_constr_one constr prod nulls=match prod.p_right with
  | []->()
  | h::t->(match h with
    | T(_) -> (if List.mem h !(constr.first_set) then () else constr.first_set:=h:: !(constr.first_set));
      if List.mem h nulls then first_constr_one constr {p_left=prod.p_left;p_right=t} nulls else ()
    | NT(_)->if List.mem h !(constr.first_set) then () else constr.first_set:=h:: !(constr.first_set)
    | Eps->())

let first_constraint grammer nulls=
  let terminals=uni_list (List.map (fun x->x.p_left) grammer) in 
  let constraints=List.map (fun x->{symbol=x;first_set=ref []}) terminals in 
  let rec calc rest_g=(match rest_g with
  | []->constraints
  | h::t->let sym_constr=List.filter (fun x->x.symbol=h.p_left) constraints in 
    first_constr_one (Option.get (head sym_constr)) h nulls; calc t)
  in calc grammer

let rec first_one set constraints=match set with
  | []->set
  | h::t->(match h with
    | T(_) -> let sym_constr=List.filter (fun x->x.symbol=h) constraints in
      !((Option.get (head sym_constr)).first_set)@(first_one t constraints)
    | NT(_)->first_one t constraints
    | Eps->Lexer.err "internal err : invalid pattern")

let calc_firsts grammer nulls=let constraints=first_constraint grammer nulls in 
  let rec one_loop is_change constr=match constr with
  | []->is_change
  | h::t->let prev= !(h.first_set) in
    let new_set=first_one !(h.first_set) constraints in h.first_set:=new_set;
    if List.length prev=List.length new_set then one_loop is_change t else one_loop false t
  in let is_change=ref false in 
  while !is_change=false do 
    is_change:=one_loop true constraints
  done; constraints