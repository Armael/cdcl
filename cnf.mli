(* Type representing a raw .cnf file :
   - the first value is the number of vars,
   - the second the number of clauses,
   - the third is the list of the clauses,
   each clause being a list of its literals
*)
type t = int * int * int list list

val parse : Buffer.t -> t
val buffer_out : t -> Buffer.t -> unit
