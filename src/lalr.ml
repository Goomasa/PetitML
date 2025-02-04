open Syntax

type lalr_item={i_left:symbol;i_right:symbol list;dot:int;next:int option ref;prod_kind:prod_kind;ahead:symbol list ref}
type 't state={items:'t;id:int} (* state of DFA and column of lalr-table*)

type action=Shift of int|Reduce of symbol*int*prod_kind|Accept (* shift->next_state_id , reduce->(i_left, num of i_right) *)
type content={follow:symbol;action:action}

let state_id=let id=ref 0 in let count()= id:=!id+1;!id in count

let union=Util.union

let same_list=Util.same_list

let lr0_closure base_items grammer=
  let rec extend items tmp=match tmp with
    | []->items
    | item::tail->
      match (List.nth_opt item.i_right item.dot) with
      | Some(T t)->
        let prods_from_t=List.filter (fun x->x.p_left=T(t)) grammer in
        let items_from_t=List.rev_map (fun x->{
          i_left=x.p_left; i_right=x.p_right; dot=0; next=ref None; prod_kind=x.kind; ahead=ref [NT End]
          }) prods_from_t in 
        let new_items=List.filter (fun x->not(List.mem x items)) items_from_t in
        extend (List.rev_append items new_items) (List.rev_append tail new_items)
      | _->extend items tail
  in extend base_items base_items

let next_state_items items symbol grammer=
  let read_items=List.filter (fun x->List.nth_opt x.i_right x.dot=Some symbol) items in
  let new_items=List.rev_map (fun x->{
    i_left=x.i_left; i_right=x.i_right; dot=x.dot+1; next=ref None; prod_kind=x.prod_kind; ahead=ref [NT End]
    }) read_items in
  lr0_closure new_items grammer

let base_states grammer=
  let rec aux rest_g items=match rest_g with
  | []->items
  | h::t->
    if h.kind=PK_top then aux t ({i_left=h.p_left; i_right=h.p_right; dot=0; next=ref None; prod_kind=h.kind; ahead=ref [NT End]}::items) 
    else aux t items 
  in let items= lr0_closure (aux grammer []) grammer in [{items=items;id=0}]

let update_next symbol state id=
  let state_items=List.filter (fun x->List.nth_opt x.i_right x.dot=Some symbol) state.items in 
  List.iter (fun x->x.next:=id) state_items

let next_id items states=
  let tmp_items=List.rev_map (fun x->(x.i_left,x.i_right,x.dot)) items in 
  let rec aux rests=match rests with
  | []->None
  | h::t->if same_list tmp_items (List.rev_map (fun x->(x.i_left,x.i_right,x.dot)) h.items) then Some h.id else aux t
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

let calc_first nulls firsts syms=
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
      then (if Util.include_list !(i.ahead) !(h.ahead) 
        then (None,None)
        else (h.ahead:=union !(i.ahead) !(h.ahead); (None,Some h)))
    else app_ahead i t

let rec app_aheads new_items now_items rests updates=match new_items with
  | []->(rests,updates) (* return new_items and updated items *)
  | h::t->
    let (rest,updated)=app_ahead h now_items in 
    match rest with
    | None->(match updated with
      | None -> app_aheads t now_items rests updates
      | Some i->app_aheads t now_items rests (i::updates))
    | Some r->app_aheads t now_items (r::rests) updates

let lr1_closure nulls firsts bases grammer=
  let rec extend items tmp=match tmp with
  | []->items
  | h::t->  
    match List.nth_opt h.i_right h.dot with
    | Some(T ts)->
      let prods_from_t=List.filter (fun x->x.p_left=T ts) grammer in 
      let items_from_t=List.rev_map (fun x->{
        i_left=x.p_left; i_right=x.p_right; dot=0; next=ref None; prod_kind=x.kind;
        ahead=calc_first nulls firsts ((Util.pop h.i_right (h.dot+1))@ !(h.ahead)) (* <- slow? *)
      }) prods_from_t in 
      let (new_items,updates)=app_aheads items_from_t items [] [] in 
      extend (List.rev_append new_items items) (List.rev_append (List.rev_append new_items updates) t)
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

let shift id item lr0_states=
  let id_state=List.find (fun x->x.id=id) lr0_states in 
  let next=(List.find (
    fun x->(x.i_left,x.i_right,x.dot)=(item.i_left,item.i_right,item.dot)
  ) id_state.items).next in 
  {i_left=item.i_left; i_right=item.i_right; dot=item.dot+1; next=next; prod_kind=item.prod_kind; ahead=item.ahead}

let rec extract_dest result dest=match dest with
  | []->result
  | h::t->
    if List.mem (NT End) !(h.ahead) then extract_dest (h::result) t
    else extract_dest result t

let spread_dests lr0_states kernel grammer=
  let nulls=Slr.calc_nulls grammer [] in 
  let firsts=Slr.calc_firsts grammer nulls in 
  let rec aux ker dests=match ker with
  | []->dests
  | {items=i;id=id}::t->
    match List.nth_opt i.i_right i.dot with
    | None->aux t ([]::dests)
    | Some(T _)->
      let closured=lr1_closure nulls firsts [i] grammer in 
      let shifted=List.rev_map (fun x->shift id x lr0_states) closured in 
      aux t (shifted::dests)
    | Some _->let shifted=shift id i lr0_states in aux t ([shifted]::dests)
  in List.rev (aux kernel [])

let rec spread_one_state lr0_states kernel is_change state dest=match dest with
  | []->is_change
  | h::t->
    let ker=List.filter (fun x->x.id=Option.get !(h.next)) kernel in 
    let ahead=(List.find (
      fun x->let i=x.items in (i.i_left,i.i_right,i.dot)=(h.i_left,h.i_right,h.dot)
      ) ker).items.ahead in 
    let prev= !ahead in 
    ahead:=union !(state.items.ahead) !(ahead);
    if same_list prev !ahead then spread_one_state lr0_states kernel is_change state t
    else spread_one_state lr0_states kernel true state t

let spread lr0_states kernel grammer=
  let dests=spread_dests lr0_states kernel grammer in 
  let rec init state dest=match dest with
  | []->()
  | h::t->
    let ker=List.filter (fun x->x.id=Option.get !(h.next)) kernel in 
    let ahead= (List.find (
      fun x->let i=x.items in (i.i_left,i.i_right,i.dot)=(h.i_left,h.i_right,h.dot)
    ) ker).items.ahead in 
    ahead:=union !(ahead) !(h.ahead); init state t
  in List.iter2 init kernel dests;
  let new_dests=List.map (extract_dest []) dests in
  let is_change=ref true in 
  while !is_change do
    is_change:=List.fold_left2 (spread_one_state lr0_states kernel) false kernel new_dests;
  done

let rec is_conflict id table_col content=match table_col with
  | []->()
  | h::t->
    if content.follow=h.follow&&content.action<>h.action 
      then (print_string ("conflicted at "^(string_of_int id)); print_newline())
    else is_conflict id t content

let rec remove_end ahead result=match ahead with
  | []->result
  | h::t->
    if h=NT End then List.rev_append t result
    else remove_end t (h::result)

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
      let contents=List.rev_map (
        fun x->{follow=x;action=Reduce(h.i_left,List.length h.i_right,h.prod_kind)}
        ) (remove_end !ahead []) in
      List.iter (is_conflict state.id col_items) contents; one_col t (union contents col_items)
    | Some(id)->
      let content={follow=List.nth h.i_right h.dot;action=Shift(id)} in 
      is_conflict state.id col_items content; one_col t (Slr.uni_one content col_items))
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