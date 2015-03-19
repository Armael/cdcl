type t = int * int * int list list

val parse_clause : string -> int list
val parse : Buffer.t -> t
val buffer_out : t -> Buffer.t -> unit
