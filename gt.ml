(* 

   Stub for HW2 
   Please
   1. Rename to gt.ml
   2. Place your name here:

    Name: Nicholas Cali
    Pledge: I pledge my honor that I have abided by the Stevens Honors System. 
*)



type 'a gt = Node of 'a*('a gt) list

let mk_leaf (n:'a) : 'a gt =
  Node(n,[])
    
let t : int gt =
 Node (33,
       [Node (12,[]);
        Node (77, 
              [Node (37, 
                     [Node (14, [])]); 
               Node (48, []); 
               Node (103, [])])
       ])


let rec mapp (f: 'a->'b) (l: 'a list) : 'b list =
  match l with
  | [] -> []
  | h::t -> f h :: mapp f t

let rec foldr (f: 'a->'b->'b) (a: 'b) (l: 'a list) : 'b = 
  match l with
  |[] -> a 
  |h::t -> f h (foldr f a t)


let rec sum a b = 
  a + b

let append a l =
  a @ l


let rec flip l  = 
  match l with 
  | [] -> [] 
  | h::t -> flip t @ [h]


(*'a gt -> int *)
let rec height t = 
  match t with
  | Node(d, l) -> 1 + foldr max 0 (mapp height l)
    
(*'a gt -> int *)    
let rec size t =
  match t with
  | Node(d, l) -> 1 + foldr sum 0 (mapp size l)

(*'a gt -> 'a list list *)
let paths_to_leaves t =
  let rec paths_to_leaves_helper t path =
    match t with
    | Node(d, []) -> [d :: path]
    | Node(d, l) ->
        List.flatten (List.map (fun x -> paths_to_leaves_helper x (d :: path)) l)
  in paths_to_leaves_helper t []


(*'a gt -> bool*)
let rec is_leaf_perfect t =
  match t with
  | Node(_, []) -> true
  | Node(_, l) ->
    let height_of_children = mapp height l in
    let is_all_same_height = List.for_all (fun x -> x = List.hd height_of_children) height_of_children in
    let is_all_leaves = List.for_all (fun x -> size x = 1) l in 
    is_all_same_height && is_all_leaves

(*'a gt -> 'a list*)
let rec preorder (Node(d,ch)) =
    match ch with
    | [] -> [d]
    | _ -> d::foldr append [] (mapp preorder ch)

(*'a gt -> 'a gt*)                        
let rec mirror (Node(d,ch)) =
  Node(d, flip (mapp mirror ch))

(*('a -> 'b) -> 'a gt -> 'b gt*)  
let rec map f (Node(d,ch)) =
  let rec map_helper x =
    map f x
  in match ch with
  | [] -> Node(f d, [])
  | _ -> Node(f d, (mapp map_helper ch))

  
let rec fold : ('a -> 'b list -> 'b) -> 'a gt -> 'b =
  fun f (Node(d,ch)) ->
    let rec fold_helper x = 
      fold f x
    in match ch with
      | [] -> f d []
      | _ -> f d (mapp fold_helper ch)

(* int gt -> int *)
let sum t =
  fold (fun i rs -> i + List.fold_left (fun i j -> i+j) 0 rs) t

(*'a gt -> 'a -> bool*)
let mem t e =
  fold (fun i rs -> i=e || List.exists (fun i -> i) rs) t

(*'a gt -> 'a gt*)
let mirror' t = 
  fold (fun i rs -> Node(i, flip(rs))) t

(*'a gt -> int*)
let degree t =
  match t with
  | Node(_, l) -> 1 + List.length l