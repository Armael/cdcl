(* Module that takes a type (representing an associative structure
   like an array/hashtable), 3 operations on it :
     - set : assigns a value to a key 
     - get : returns the value associated to a key
     - replace : assigns a value to a key, *and* returns the value that
     has been overwritten
   and returns a new type that is a backtrackable version of the structure.

   The following functions can be applied to it : get, set, push_state
   (creates a breakpoint), pop_state (backtracks to the last
   breakpoint)

   Example of utilisation (creates the backtrackable version of an
   array of size 5) :

   let a = Bt.mk_backtrackable
     ~set:(fun a i x -> a.(i) <- x)
     ~get:(fun a i -> a.(i))
     (Array.make 5 'a')
*)

type ('a, 'b) action =
| Stop
| Set of 'a * 'b

type ('a,'b,'c) t = {
  container : 'c;
  set : 'c -> 'a -> 'b -> unit;
  get : 'c -> 'a -> 'b;
  replace : 'c -> 'a -> 'b -> 'b;
  stack : ('a, 'b) action Stack.t;
  mutable depth : int;
  mutable set_past : ('a * (unit -> 'b) * int) list
}

let mk_backtrackable ~set ~get container = {
  container;
  get;
  set;
  replace = (fun a i v -> let x = get a i in set a i v; x);
  stack = Stack.create ();
  depth = 0;
  set_past = [];
}

let get bt x = bt.get bt.container x

let set bt x y =
  let y' = bt.replace bt.container x y in
  Stack.push (Set (x,y')) bt.stack

let set_past bt x f =
  bt.set_past <- (x,f,bt.depth)::bt.set_past

let push_state bt =
  bt.depth <- bt.depth + 1;
  Stack.push Stop bt.stack

let rec pop_state bt =
  match Stack.pop bt.stack with
  | Stop ->
    bt.depth <- bt.depth - 1;
    bt.set_past <-
      (List.map (fun c ->
        let (x, f, d) = c in
        if bt.depth < d then (
          bt.set bt.container x (f ());
          (x, f, bt.depth)
        ) else c
      ) bt.set_past)
  | Set (x,y) ->
    bt.set bt.container x y;
    pop_state bt

let push_n_state bt n =
  for i = 1 to n do
    push_state bt
  done

let pop_n_state bt n =
  for i = 1 to n do
    pop_state bt
  done

let forget bt =
  bt.depth <- 0;
  Stack.clear bt.stack
