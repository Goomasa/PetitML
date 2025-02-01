type 't env=(string*'t) list

let rec search_env id env=match env with
  | [] -> Util.err ("undefined variable: "^id)
  | (s,v)::t->if id=s then v else search_env id t