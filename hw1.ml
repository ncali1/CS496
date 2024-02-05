(* ******************************************** *)
(** Basic functions on finite automata *)
(* ******************************************** *)
(**
   Code stub for assignment 1
   
   Nicholas Cali
   I pledge my honor that I have abided by the Stevens Honors System.
*)
type symbol = char

type input = char list

type state = string

(* transition function *)
type tf = (state * symbol * state) list
(* initial state * transition function * end state *)
type fa = { states: state list; start:state; tf: tf; final: state list}

(* ******************************************** *)
(* Examples of automata *)
(* ******************************************** *)

let a = {states = ["q0";"q1";"q2"];
         start = "q0";
         tf = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")];
         final = ["q2"]}

let a2 = {states = ["q0";"q1";"q2";"q3";"q4"];
          start = "q0";
          tf = [("q0",'a',"q1"); ("q1",'b',"q1")
               ; ("q1",'c',"q2");  ("q3",'a',"q4")];
          final= ["q2"]
         }
let tf_of_a = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")]



(* ******************************************** *)
(* Helper functions *)
(* ******************************************** *)

let input_of_string s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

    
let rec map f l =
  match l with 
  | [] -> [] 
  | h::t -> f h :: map f t

let rec rem_state state s3 = 
  match state with
  | [] -> []
  | x :: y -> 
    if List.mem x s3
    then x::rem_state y s3
    else rem_state y s3

let rec rem_transition_function tf s1 = 
  match tf with
  | [] -> []
  | (q, w, q')::tf' ->
    if List.mem q s1
    then (q, w, q') :: rem_transition_function tf' s1
    else rem_transition_function tf' s1

let rec combine l1 l2 =
  match l1 with
  | [] -> l2
  | x::y -> if List.mem x l2
    then combine y l2
    else x::combine y l2


(* ******************************************** *)
(* Simulating automata *)
(* ******************************************** *)

    
let rec apply_transition_function tf state sym =
  match tf with
  | [] -> None
  | (q, w, q') :: tf' when q = state && w = sym -> Some q' 
  | _ :: tf' -> apply_transition_function tf' state sym 
                  
let rec accept fa input = 
  let rec accept_helper state input = 
    match input with
    |[] -> state
    |sym::symbols->
        match state with 
        |Some q -> accept_helper (apply_transition_function fa.tf q sym) symbols 
        |None -> None
  in match accept_helper(Some fa.start) input with
  |None -> false
  |Some state -> List.mem state fa.final
    
let rec next tf state symbol =
  match tf with 
  | [] -> []
  | (q, w, q')::tf' ->
      if q = state && w = symbol 
      then q'::next tf' state symbol
      else next tf' state symbol
  
          
let rec deterministic fa = 
  let rec checker = function
    | [] -> true
    | (q,w,q') :: tf -> 
        match next tf q w with
        | [] -> checker tf
        | _ -> false 
  in checker fa.tf
    
    
let rec valid fa = 
  let found state = List.mem state fa.states
  in found fa.start && List.for_all found fa.final && deterministic fa 
  
       
let rec reachable_helper tf start states = 
    match tf with
    | [] -> []
    | (q, w, q'):: tf' ->
      if q = start 
      then 
        if List.mem q' states
        then reachable_helper tf' start states
        else q' :: reachable_helper tf' start (q'::states)
      else reachable_helper tf' start states


let rec reachable fa =
  (* prints out q1, q2, q0 for some reason, dont know what to do to fix it *)
  let rec holder state s1 =
      match state with
      | [] -> [fa.start]
      | x::y -> if List.mem x s1
        then holder y s1
        else x::holder(combine(reachable_helper fa.tf x(List.append state s1)) y ) (List.append [x] s1)
  in holder (reachable_helper fa.tf fa.start []) [fa.start] 



let rec non_empty fa =
  failwith"not yet"
  
  

    
let rec remove_dead_states fa = 
  let s1 = reachable fa in 
  {
    states = s1;
    tf = rem_transition_function fa.tf s1;
    start = fa.start;
    final = rem_state fa.final s1;
  }
  
  

    