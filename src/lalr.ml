open Syntax

type lalr_item={i_left:symbol;i_right:symbol list;dot:int;next:int option ref;prod_kind:prod_kind;ahead:symbol list ref}
type 't state={items:'t;id:int} (* state of DFA and column of lalr-table*)

type action=Shift of int|Reduce of symbol*int*prod_kind|Accept (* shift->next_state_id , reduce->(i_left, num of i_right) *)
type content={follow:symbol;action:action}

let state_id=let id=ref 0 in let count()= id:=!id+1;!id in count

let union=Slr.union

let lr0_closure base_items grammer=
  let rec extend items tmp=match tmp with
    | []->items
    | item::tail->
      match (List.nth_opt item.i_right item.dot) with
      | Some(T t)->
        let prods_from_t=List.filter (fun x->x.p_left=T(t)) grammer in
        let items_from_t=List.rev_map (fun x->{
          i_left=x.p_left; i_right=x.p_right; dot=0; next=ref None; prod_kind=x.kind; ahead=ref []
          }) prods_from_t in 
        let new_items=List.filter (fun x->not(List.mem x items)) items_from_t in
        extend (List.rev_append items new_items) (List.rev_append tail new_items)
      | _->extend items tail
  in extend base_items base_items

let next_state_items items symbol grammer=
  let read_items=List.filter (fun x->List.nth_opt x.i_right x.dot=Some symbol) items in
  let new_items=List.rev_map (fun x->{
    i_left=x.i_left; i_right=x.i_right; dot=x.dot+1; next=ref None; prod_kind=x.prod_kind; ahead=ref []
    }) read_items in
  lr0_closure new_items grammer

let base_states grammer=
  let rec aux rest_g items=match rest_g with
  | []->items
  | h::t->
    if h.kind=PK_top then aux t ({i_left=h.p_left; i_right=h.p_right; dot=0; next=ref None; prod_kind=h.kind; ahead=ref []}::items) 
    else aux t items 
  in let items= lr0_closure (aux grammer []) grammer in [{items=items;id=0}]

let update_next symbol state id=
  let state_items=List.filter (fun x->List.nth_opt x.i_right x.dot=Some symbol) state.items in 
  List.iter (fun x->x.next:=id) state_items

let next_id items states=
  let tmp_items=List.rev_map (fun x->(x.i_left,x.i_right,x.dot)) items in 
  let rec aux rests=match rests with
  | []->None
  | h::t->if tmp_items=(List.rev_map (fun x->(x.i_left,x.i_right,x.dot)) h.items) then Some h.id else aux t
  in aux states

let calc_lr0_states grammer=
  let rec add states added_states=match added_states with
    | []->states
    | st_h::st_t->
      let symbols_opt=List.rev_map (fun x->List.nth_opt x.i_right x.dot) st_h.items in
      let symbols=List.filter (fun x->not(x=None)) symbols_opt in
      let rec add_one_state symbols states added_states =
        match symbols with
        | []->add states added_states
        | sym_h::sym_t->
          let some_sym=Option.get sym_h in 
          let next_items=next_state_items st_h.items some_sym grammer in
          let next_id=next_id next_items states in 
          if next_id=None then 
            let id=state_id() in update_next some_sym st_h (Some id);
            let new_state={items=next_items;id=id} in 
            add_one_state sym_t (new_state::states) (new_state::added_states)
          else (update_next some_sym st_h next_id; add_one_state sym_t states added_states)
      in add_one_state symbols states st_t
  in let base=base_states grammer in add base base

let rec pop list n=if n=0 then list else match list with
  | []->[]
  | _::t->pop t (n-1)

let calc_first syms grammer=
  let nulls=Slr.calc_nulls grammer [] in 
  let firsts=Slr.calc_firsts grammer nulls in 
  let rec calc syms first=match syms with
  | []->first
  | NT nts::_->NT nts::first
  | T ts::t->
    let set=(List.find (fun x->x.Slr.sym=T(ts)) firsts).set in 
    if List.mem (T ts) nulls then calc t (union (!set) first)
    else union (!set) first
  | Eps::t->calc t first
  in ref (calc syms [])

let rec app_ahead i items=match items with
  | []->(Some i,None)
  | h::t->
    if i.i_left=h.i_left&&i.i_right=h.i_right&&i.dot=h.dot
      then (h.ahead:=union !(i.ahead) !(h.ahead); (None,Some h))
    else app_ahead i t

let rec app_aheads new_items now_items rests updates=match new_items with
  | []->(rests,updates) (* return new_items and updated items *)
  | h::t->
    let (rest,updated)=app_ahead h now_items in 
    match rest with
    | None->app_aheads t now_items rests (Option.get updated::updates)
    | Some r->app_aheads t now_items (r::rests) updates

let lr1_closure bases grammer=
  let rec extend items tmp=match tmp with
  | []->items
  | h::t->
    match List.nth_opt h.i_right h.dot with
    | Some(T ts)->
      let prods_from_t=List.filter (fun x->x.p_left=T ts) grammer in 
      let items_from_t=List.rev_map (fun x->{
        i_left=x.p_left; i_right=x.p_right; dot=0; next=ref None; prod_kind=x.kind;
        ahead=calc_first ((pop h.i_right (h.dot+1))@ !(h.ahead)) grammer (* <- slow? *)
      }) prods_from_t in 
      let (new_items,updates)=app_aheads items_from_t items [] [] in 
      extend (List.rev_append new_items items) (List.rev_append (new_items@updates) t)
    | _->extend items t 
  in extend bases bases

let kernel_from_items state=
  let rec calc items kernel_items=match items with
  | []->kernel_items
  | h::t->
    if h.prod_kind=PK_top||h.dot>0 then calc t ({items=h;id=state.id}::kernel_items)
    else calc t kernel_items
  in calc state.items []

let calc_kernel lr0_states=
  let rec calc states kernel=match states with
  | []->kernel
  | h::t->calc t (List.rev_append (kernel_from_items h) kernel)
  in calc lr0_states []

let shift item=
  {i_left=item.i_left; i_right=item.i_right; dot=item.dot+1; next=item.next; prod_kind=item.prod_kind; ahead=item.ahead}

let same_list l1 l2=List.for_all (fun x->List.mem x l2) l1

let spread_dest kernel grammer=
  let rec aux ker results=match ker with
  | []->results
  | {items=i;id=_}::t->
    match List.nth_opt i.i_right i.dot with
    | None->aux t ([]::results)
    | Some(T _)->
      let closured=lr1_closure [i] grammer in 
      let shifted=List.rev_map (fun x->shift x) closured in 
      aux t (shifted::results)
    | Some _->let shifted=shift i in aux t ([shifted]::results)
  in List.rev (aux kernel [])

let rec spread_one_state lr0_states kernel is_change state dest=match dest with
  | []->is_change
  | h::t->
    let id_state=List.find (fun x->x.id=state.id) lr0_states in 
    let id=let item=List.find (
      fun x->let sym=(List.nth_opt x.i_right x.dot) in sym<>None && sym=Some(List.nth h.i_right (h.dot-1))
    ) id_state.items in Option.get !(item.next) in 
    let ker=List.filter (fun x->x.id=id) kernel in 
    let ahead=(List.find (
      fun x->let i=x.items in (i.i_left,i.i_right,i.dot)=(h.i_left,h.i_right,h.dot)
      ) ker).items.ahead in 
    let prev= !ahead in 
    ahead:=union !(ahead) !(state.items.ahead);
    if same_list prev !ahead then spread_one_state lr0_states kernel is_change state t
    else spread_one_state lr0_states kernel true state t

let spread lr0_states kernel grammer=
  let dest=spread_dest kernel grammer in 
  let rec init dest_fst=match dest_fst with
  | []->()
  | h::t->(List.find (
    fun x->let i=x.items in (i.i_left,i.i_right,i.dot)=(h.i_left,h.i_right,h.dot)
    ) kernel).items.ahead:= !(h.ahead);init t
  in init (List.hd dest);
  let is_change=ref true in 
  while !is_change do
    is_change:=List.fold_left2 (spread_one_state lr0_states kernel) false kernel dest;
  done

let rec is_conflict table_col content=match table_col with
  | []->()
  | h::t->
    if content.follow=h.follow&&content.action<>h.action then Lexer.err "conflicted!"
    else is_conflict t content

let table_one_col kernel state=
  let state_items=state.items in
  let rec one_col st_items col_items=match st_items with
  | []->col_items
  | h::t->(match !(h.next) with
    | None -> 
      let ker=List.filter (fun x->x.id=state.id) kernel in
      let ahead=(List.find (
        fun x->let i=x.items in (i.i_left,i.i_right,i.dot)=(h.i_left,h.i_right,h.dot)
        ) ker).items.ahead in
      let contents=List.rev_map (fun x->{follow=x;action=Reduce(h.i_left,List.length h.i_right,h.prod_kind)}) !ahead in
      List.iter (is_conflict col_items) contents; one_col t (union contents col_items)
    | Some(id)->
      let content={follow=List.nth h.i_right h.dot;action=Shift(id)} in 
      is_conflict col_items content; one_col t (Slr.uni_one content col_items))
  in {items=one_col state_items [];id=state.id}

let create_lalr_table grammer=
  let lr0_states=calc_lr0_states grammer in
  let kernel=calc_kernel lr0_states in spread lr0_states kernel grammer;
  let rec create st_rests table=match st_rests with
    | [] -> table
    | h::t->
      let column=table_one_col kernel h in 
      let new_col= if column.items=[] then {items=[{follow=Eps;action=Accept}];id=column.id} else column in 
      create t (new_col::table)
  in create lr0_states []