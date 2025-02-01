open Syntax

let sub_str=Lexer.sub_str

let mlr_code=
  let file=try open_in "src/grammer.mlr" with _->open_in "../../../src/grammer.mlr" in (* test-> ../../../src/gramer.mlr *)
  let rec reader code=
    try
      let line=input_line file in 
      reader (code^line)
    with End_of_file->
      close_in_noerr file; code
  in reader ""

let mlr_div=
  let rec concat code tmp=if code="" then [] else
  match code.[0] with
  | ' '->if tmp<>"" then tmp::concat (sub_str code 1) "" else concat (sub_str code 1) ""
  | ','->if tmp<>"" then tmp::","::concat (sub_str code 1) "" else ","::concat (sub_str code 1) ""
  | '"'->if tmp<>"" then tmp::"$$"::concat (sub_str code 1) "" else "$$"::concat (sub_str code 1) ""
  | '{'->if tmp<>"" then tmp::"{"::concat (sub_str code 1) "" else "{"::concat (sub_str code 1) ""
  | '}'->if tmp<>"" then tmp::"}"::concat (sub_str code 1) "" else "}"::concat (sub_str code 1) ""
  | c->concat (sub_str code 1) (tmp^(Char.escaped c))
  in concat mlr_code ""

let new_grammer=
  let rec read prod grammer mlr_list=match mlr_list with
    | []->prod::grammer
    | h::[]->(match h with
      | ","|"}" ->prod.p_right<-List.rev prod.p_right; prod::grammer
      | _->Util.err "mlr err")
    | h1::h2::t->(match h1 with
      | ","->prod.p_right<-List.rev prod.p_right; read {p_left=T(h2);p_right=[];kind=PK_null} (prod::grammer) t
      | ":="->read prod grammer (h2::t)
      | "|"->prod.p_right<-List.rev prod.p_right; read {p_left=prod.p_left;p_right=[];kind=PK_null} (prod::grammer) (h2::t)
      | "{"->prod.kind<-to_prod_kind h2; read prod grammer t
      | "}"->read prod grammer (h2::t)
      | "$$"->
        let token=let (tk,_)=Lexer.to_token h2 in NT (Option.get tk) in ( 
        match t with
        | "$$"::t2 -> prod.p_right<-token::prod.p_right; read prod grammer t2
        | _->Util.err "mlr err")
      | _->let token= (match h1 with
        | "Int" -> NT (Num 0)
        | "String"->NT (String "")
        | "Eps"->Eps
        | "Ident"->NT (Ident "")
        | _->T h1)
       in prod.p_right<-token::prod.p_right; read prod grammer (h2::t))
  in let g= match mlr_div with
  | []->Util.err "empty mlr"
  | h::t->read {p_left=T h;p_right=[];kind=PK_null} [] t
  in List.rev g