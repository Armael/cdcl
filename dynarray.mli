(* Dynamic arrays *)

type 'a t
val make : int -> 'a -> 'a t
val init : int -> (int -> 'a) -> 'a t
val length : 'a t -> int
val get : 'a t -> int -> 'a
val set : 'a t -> int -> 'a -> unit
val push_back : 'a t -> 'a -> int
val shrink : 'a t -> int -> unit
