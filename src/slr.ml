open Lexer

type symbol=T of string|NT of token_kind
type prod={p_left:string;p_right:symbol list}
type grammer=prod list

type item={i_left:string;i_right:symbol list;dot:int;next_id:int option}
type state={items:item list;id:int}

let head=function
  | []->None
  | h::_->Some h

let states_list states=List.map (fun x->x.items) states

let state_id=let id=ref 0 in let count()= id:=!id+1;!id in count (* return 1,2,... *)

let closure base_items grammer=
  let rec extend items tmp=match tmp with
    | []->items
    | item::tail->match (List.nth_opt item.i_right item.dot) with
      | Some(T t)->let prods_from_t=List.filter (fun x->x.p_left=t) grammer in
        let items_from_t=List.map (fun x->{i_left=x.p_left;i_right=x.p_right;dot=0;next_id=None}) prods_from_t in 
        let new_items=List.filter (fun x->not(List.mem x items)) items_from_t in
        extend (items@new_items) (tail@new_items)
      | _->extend items tail
  in extend base_items base_items

let next_state_items items symbol grammer=
  let read_items=List.filter (fun x->List.nth_opt x.i_right x.dot=Some symbol) items in
  let new_items=List.map (fun x->{i_left=x.i_left;i_right=x.i_right;dot=x.dot+1;next_id=None}) read_items in
  closure new_items grammer

let calc_states base_states grammer=
  let rec add states added_states=match added_states with
    | []->states
    | st_h::st_t->let symbols_opt=List.map (fun x->List.nth_opt x.i_right x.dot) st_h.items in
      let symbols=List.filter (fun x->not(x=None)) symbols_opt in
      let rec add_one_state symbols states added_states =
        match symbols with
        | []->add states st_t
        | sym_h::sym_t->let next_items=next_state_items st_h.items (Option.get sym_h) grammer in
          if not(List.mem next_items (states_list states)) then 
            let new_state={items=next_items;id=state_id()} in add_one_state sym_t (new_state::states) (new_state::st_t)
          else add_one_state sym_t states added_states
      in add_one_state symbols states added_states
  in add base_states base_states
  