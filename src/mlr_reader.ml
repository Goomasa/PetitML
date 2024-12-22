let mlr_code=
  let file=open_in "../../../src/grammer.mlr" in
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
  | ' '->if tmp<>"" then tmp::concat (Lexer.sub_str code 1) "" else concat (Lexer.sub_str code 1) ""
  | ','->if tmp<>"" then tmp::","::(concat (Lexer.sub_str code 1)) "" else ","::(concat (Lexer.sub_str code 1)) ""
  | '"'->concat (Lexer.sub_str code 1) tmp
  | c->concat (Lexer.sub_str code 1) (tmp^(Char.escaped c))
  in concat mlr_code ""

let new_grammer=
  let rec aux prod grammer mlr_list=match mlr_list with
    | []->prod::grammer
    | h::[]->(match h with
      | "," -> {Slr.p_left=prod.Slr.p_left;Slr.p_right=List.rev prod.p_right}::grammer
      | ":="|"|"->Lexer.err "mlr err"
      | _->
      try 
        let (token,_)=Lexer.to_token h in 
        {Slr.p_left=prod.p_left;Slr.p_right=(NT (Option.get token))::prod.p_right}::grammer
      with _-> 
        let token=match h with
        | "Eps"->Slr.Eps
        | "Int" -> Slr.NT (Lexer.Num 0)
        | _->Slr.T h
        in {Slr.p_left=prod.p_left;Slr.p_right=token::prod.p_right}::grammer)
    | h1::h2::t->(match h1 with
      | ","->aux {Slr.p_left=T(h2);Slr.p_right=[]} ({Slr.p_left=prod.p_left;Slr.p_right=List.rev prod.p_right}::grammer) t
      | ":="->aux prod grammer (h2::t)
      | "|"->aux {Slr.p_left=prod.p_left;Slr.p_right=[]} ({Slr.p_left=prod.p_left;Slr.p_right=List.rev prod.p_right}::grammer) (h2::t)
      | _->
      try 
        let (token,_)=Lexer.to_token h1 in 
        aux {Slr.p_left=prod.p_left;Slr.p_right=(NT (Option.get token))::prod.p_right} grammer (h2::t)
      with _-> 
        let token=match h1 with
        | "Eps"->Slr.Eps
        | "Int" -> Slr.NT (Lexer.Num 0)
        | _->Slr.T h1
        in aux {Slr.p_left=prod.p_left;Slr.p_right=token::prod.p_right} grammer (h2::t)) 
  in let g= match mlr_div with
  | []->Lexer.err "empty mlr"
  | h::t->aux {Slr.p_left=T h;Slr.p_right=[]} [] t
  in List.rev g