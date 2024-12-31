let same_list l1 l2=List.for_all (fun x->List.mem x l2) l1

let rec uni_list list=match list with
  | []->[]
  | h::t->if List.mem h t then uni_list t else h::(uni_list t)

let uni_one a list=if List.mem a list then list else a::list

let rec union l1 l2=match l1 with
  | [] -> l2
  | h::t->if List.mem h l2 then union t l2 else union t (h::l2)

exception Error of string
let err s = raise (Error s)

let head list=match list with
  | []->err "empty"
  | h::_->h

let rec pop list n=if n=0 then list else match list with
  | []->[]
  | _::t->pop t (n-1)