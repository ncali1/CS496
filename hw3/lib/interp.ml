open Parser_plaf.Ast
open Parser_plaf.Parser
open Ds

(* Nicholas Cali

I pledge my honor that I have abided by the Stevens Honor System.
*)
let rec eval_expr : expr -> exp_val ea_result = fun e ->
  match e with
  | Int(n) ->
    return @@ NumVal n
  | Var(id) ->
    apply_env id
  | Add(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1+n2)
  | Sub(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1-n2)
  | Mul(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1*n2)
  | Div(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    if n2==0
    then error "Division by zero"
    else return @@ NumVal (n1/n2)
  | Let(id,def,body) ->
    eval_expr def >>=
    extend_env id >>+
    eval_expr body
  | ITE(e1,e2,e3) ->
    eval_expr e1 >>=
    bool_of_boolVal >>= fun b ->
    if b
    then eval_expr e2
    else eval_expr e3
  | IsZero(e) ->
    eval_expr e >>=
    int_of_numVal >>= fun n ->
    return @@ BoolVal (n = 0)
  | Proc(id,_,e)  ->
    lookup_env >>= fun en ->
    return (ProcVal(id,e,en))
  | App(e1,e2)  ->
    eval_expr e1 >>= 
    clos_of_procVal >>= fun (id,e,en) ->
    eval_expr e2 >>= fun ev ->
    return en >>+
    extend_env id ev >>+
    eval_expr e
  | Abs(e1)      ->
    eval_expr e1  >>=
    int_of_numVal >>= fun n ->
    return @@ NumVal (abs n)
  | Cons(e1, e2) -> 
    eval_expr e1 >>= fun e ->
    eval_expr e2 >>= 
    list_of_listVal >>= fun l ->
    return @@ (ListVal (List.cons e l))
  | Hd(e1) ->  
    eval_expr e1 >>=
    list_of_listVal >>= fun l ->
    if (List.length l) = 0
    then error "empty list"
    else return @@ (List.hd l)
  | Tl(e1) ->  
    eval_expr e1 >>=
    list_of_listVal >>= fun l ->
    if (List.length l) == 0
    then error "empty list"
    else return @@ (ListVal (List.tl l))   
  | Record(fs) -> 
    failwith"implement me"
  | Proj(e,id) ->  
    failwith"implement me"
  | IsEmpty(e1) ->  
    eval_expr e1 >>= tree_of_treeVal >>= fun n1 ->
    match n1 with 
    | _-> return @@ BoolVal(false)
    | Empty -> return @@ BoolVal(true)
  | EmptyList ->  return @@ (ListVal [])
  | EmptyTree ->  
    return @@ (TreeVal Empty)
  | Node(e1,lte,rte) -> 
    eval_expr e1 >>= fun n ->
    eval_expr lte >>=
    tree_of_treeVal >>= fun lt ->
    eval_expr rte >>=
    tree_of_treeVal >>= fun rt ->
    return @@ (TreeVal (Node(n, lt, rt)))
  | CaseT(target,emptycase,id1,id2,id3,nodecase) -> 
    eval_expr target >>=
    tree_of_treeVal >>= fun t ->
    begin
    match t with
  | Empty -> eval_expr emptycase >>= fun e ->
      return e 
  | Node(d,l,r)-> 
    extend_env id1 d >>+
    extend_env id2 (TreeVal (l)) >>+
    extend_env id3 (TreeVal (r)) >>+
    eval_expr nodecase >>= fun n ->
    return n
    end
  | Tuple(es) -> failwith "implement me"
  | Untuple(ids,e1,e2) -> failwith "implement me" 
  | _ -> failwith "not implemented" 

(** [eval_prog e] evaluates program [e] *)
let eval_prog (AProg(_,e)) =
  eval_expr e

(** [interp s] parses [s] and then evaluates it *)
let interp (e:string) : exp_val result =
  let c = e |> parse |> eval_prog
  in run c

