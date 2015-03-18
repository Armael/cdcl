open Prelude

type 'a t = 'a Array.t

let make n v =
  Array.make (2*n) v

let fmake n f =
  Array.init (2*n) (fun _ -> f ())

let get a i =
  a.(Prelude.aid i)

let set a i x =
  a.(Prelude.aid i) <- x

let map f a =
  Array.iteri (fun i x ->
    a.(i) <- f a.(i)
  ) a
