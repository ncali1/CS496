(* Quiz 1 - 1 Feb 2023 

   Student name 1:  Nicholas Cali
   Student name 2: Adam Ray
   Pledge: I pledge my honor that i have abided by the stevvens honors systen

*)

(* Sample Directed Graph *)

let ex = [(1, 2); (2, 3); (3, 1); (3, 4)]


(*
  1 <------ 3
  |      //||
  |     /   | 
  |    /    | 
 \/  /     \/
  2        4
*)
       
(* 
Eg. outgoing ex 3 => [1,4] 
*)
let rec outgoing_nodes g n =
  match g with 
  | [] -> []
  | h::t -> 
    if fst h = n
    then snd h :: (outgoing_nodes t n)
    else (outgoing_nodes t n)

(* 
   The list of nodes of the tree without duplicates. The order of the  nodes in the list is irrelevant.
   eg. nodes ex => [1,2,3,4] 
*)
let rec no_dups l =
  match l with
  | [] -> []
  | h ::t ->
    if List.mem h t
    then no_dups t
    else h :: no_dups t 


let rec nodes g =
  (**no_dups @@ List.flatten @@ List.map (fun (src, tgt) -> [src;tgt]) g **)
  failwith "yo"

(** [degree g]  returns the degree of [g]. The degree of a graph is 
    the maximum number of outgoing edges that any node has. 
*)

let rec maxl l =
  match l with
  | [] -> failwith "empty list"
  | [x] -> x
  | h::t -> max h (maxl t) 

let rec degree g =
  maxl @@ List.map (fun n ->  List.length @@ outgoing_nodes g n) (nodes g)

(* 
   Remove a node from the graph
   Eg. remove ex 2 =>  [(3, 1); (3, 4)] 
*)
let rec remove g n =
  match g with
  | [] -> []
  |h::t ->
  if fst h = n || snd h = n
  then remove t n
  else h:: remove t n

  
(* Reachable nodes from a source node. (Extra-credit)
   Eg. reachale ex 3 => [1,4,2,3] 
   *)

let reachable g n =
  failwith "implement"

