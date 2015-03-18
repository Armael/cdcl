type 'a t
val make : int -> 'a -> 'a t
val fmake : int -> (unit -> 'a) -> 'a t
val get : 'a t -> int -> 'a
val set : 'a t -> int -> 'a -> unit
val map : ('a -> 'a) -> 'a t -> unit
