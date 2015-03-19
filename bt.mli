type ('a, 'b) action
type ('a, 'b, 'c) t = {
  container : 'c;
  set : 'c -> 'a -> 'b -> unit;
  get : 'c -> 'a -> 'b;
  replace : 'c -> 'a -> 'b -> 'b;
  stack : ('a, 'b) action Stack.t;
  mutable depth : int;
  mutable set_past : ('a * (unit -> 'b) * int) list;
}

val mk_backtrackable :
  set:('a -> 'b -> 'c -> unit) ->
  get:('a -> 'b -> 'c) -> 'a -> ('b, 'c, 'a) t

val get : ('a, 'b, 'c) t -> 'a -> 'b
val set : ('a, 'b, 'c) t -> 'a -> 'b -> unit
val set_past : ('a, 'b, 'c) t -> 'a -> (unit -> 'b) -> unit
val push_state : ('a, 'b, 'c) t -> unit
val pop_state : ('a, 'b, 'c) t -> unit
val push_n_state : ('a, 'b, 'c) t -> int -> unit
val pop_n_state : ('a, 'b, 'c) t -> int -> unit

val mk_backtrackable_array : 'a array -> (int, 'a, 'a array) t
val mk_backtrackable_dynarray : 'a DynArray.t -> (int, 'a, 'a DynArray.t) t
val mk_backtrackable_value : 'a -> ('b, 'a, 'a ref) t
val mk_backtrackable_var_array : 'a array -> (int, 'a, 'a array) t
