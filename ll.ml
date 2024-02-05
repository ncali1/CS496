(* Mutable Fields in Records 
   An example - Linked Lists
   22 March 2023
*)


type 'a node = { mutable data: 'a;
                 mutable next: 'a node option}

type 'a ll = { mutable head:'a node option;
               mutable size:int}

let ex1: int ll = { head = None;
                    size = 0}

let ex2: int ll =
  { head = Some { data=1; next=None};
    size = 1 }

let add_first : 'a ll -> 'a -> unit =
  fun ll e ->
  ll.head <- Some {data=e; next= ll.head};
  ll.size <- ll.size+1

let rec map =
  fun f ll ->
  let rec helper no = 
    match no with 
    | None -> ()
    | Some n -> 
      n.data <- f(n.data);
      helper (n.next)
    in helper ll.head

let rec nth =
  fun ll n ->
  failwith "implement"

let rec last =
  fun ll ->
  failwith "implement"

let rec append =
  fun ll1 ll2 ->
  let rec helper aa = 
    match aa with
    | None -> ()
    | Some n -> 
      if n.next = None 
      then n.next <- ll2.head
      else helper n.next
  in 
  begin 
    if ll1.size = 0
    then ll1.head <- ll2.head
    else helper ll1.head;
  ll1.size <- ll1.size+ll2.size
  end





let rec string_of_ll =
  fun ll to_string ->
  failwith "implement"
    
