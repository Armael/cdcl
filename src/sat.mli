type state
type outcome = UnSat | Sat of bool Prelude.Array.t

val init_state : Cnf.t -> state
val cdcl : state -> outcome
