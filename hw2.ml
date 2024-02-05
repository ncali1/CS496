(*Nicholas Cali 
  I pledge my honor that I have abided by the Stevens Honors system *)
type dtree = Leaf of int | Node of (char * dtree * dtree)

let tLeft = Node('w', Node('x', Leaf(2), Leaf(5)), Leaf(8))

let tRight = Node('w', Node('x', Leaf(2), Leaf(5)), Node('y', Leaf(7), Leaf(5)))

let rec dTree_height tree = 
match tree with
  | Leaf(x) -> 0
  | Node(x, y, z) ->
    let yy = dTree_height y in
    let zz = dTree_height z in
    if(yy > zz) then 1 + yy
    else 1 + zz

let rec dTree_size tree = 
  match tree with
  | Leaf(x) -> 1
  | Node(x, y, z) -> (dTree_size y) + (dTree_size z) + 1

let dTree_path_helper_first t  =
  0::t

let dTree_path_helper_second  t  =
  1::t
let rec dTree_paths tree = 
  match tree with
  | Leaf(x) -> [[]]
  | Node(x, y, z) -> List.map dTree_path_helper_first ( (dTree_paths y)) @ List.map dTree_path_helper_second ( (dTree_paths z))

let rec dTree_is_perfect tree =
  match tree with
  | Leaf(x) -> true
  | Node(mid,lchild,rchild) -> let lheight = dTree_height lchild in let rheight = dTree_height rchild in
  if lheight != rheight then false
  else dTree_is_perfect lchild && dTree_is_perfect rchild

let rec dTree_map f g t =
  match t with 
  | Leaf(x) -> Leaf(g x)
  | Node(x,y,z) -> Node((f x), dTree_map f g y, dTree_map f g z)

let rec list_to_tree l =
  match l with 
  | [] -> Leaf(0)
  | h::t -> 
  match h with
  | l -> Node(h, list_to_tree t, list_to_tree t)

let rec replace_leaf_at_help t n x = 
  match n with
  | [] -> Leaf(x)
  | h::tl -> 
  match t with
  | _ -> failwith "Error"
  | Node(mid, lchild, rchild) -> if h = 0 then Node(mid, replace_leaf_at_help lchild tl x, rchild)
  else Node(mid, lchild, replace_leaf_at_help rchild tl x)

let rec replace_leaf_at t f = 
  match f with
  | [] -> t
  | (a,b)::tl -> replace_leaf_at (replace_leaf_at_help t a b) tl

let rec bf_to_dTree (x,y) =
  replace_leaf_at (list_to_tree x) y