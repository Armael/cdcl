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

(* Utility functions : convenience wrappers, that just use
   mk_backtrackable with particular set and get functions *)

(* Wrapper to mk_backtrackable that turns an array into a
   backtrackable one *)
let mk_backtrackable_array arr =
  mk_backtrackable
    ~set:Array.set
    ~get:Array.get
    arr

let mk_backtrackable_dynarray arr =
  mk_backtrackable
    ~set:DynArray.set
    ~get:DynArray.get
    arr

(* Makes a functionnal value backtrackable, by storing it in a
   ref. The type of the keys is unit, so if v is of type 'a, the
   resulting type is (unit, 'a, 'a ref) Bt.backtrackable *)
let mk_backtrackable_value v =
  let r = ref v in
  mk_backtrackable
    ~set:(fun r _ y -> r := y)
    ~get:(fun r _ -> !r)
    r

(* Makes an array of size 2*n backtrackable, but with keys between -n
   and n (excluding 0). That is useful to have arrays indexed by
   literals *)
let mk_backtrackable_var_array a =
  mk_backtrackable
    ~set:(fun a x v -> a.(Prelude.aid x) <- v)
    ~get:(fun a x -> a.(Prelude.aid x))
    a
