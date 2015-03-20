(** Implements autoexpanding arrays *)


type 'a t = {
  mutable contents : 'a array;
  mutable length : int;
}

let make size x =
  if size < 0 then raise (Invalid_argument "Dynarray.make");
  { contents = Array.make (size + 1) x;
    length = size;
  }

let init size f =
  let a = make size (Obj.magic ()) in
  for i = 0 to size-1 do
    a.contents.(i) <- f i
  done;
  a

let length a = a.length

let expand a =
  let len = Array.length a.contents in
  let new_contents = Array.make (len * 2) (Obj.magic 0) in
  Array.blit a.contents 0 new_contents 0 len;
  a.contents <- new_contents

let get a i =
  if i >= a.length then raise (Invalid_argument "Dynarray.get");
  a.contents.(i)

let set a i x =
  if i >= a.length then raise (Invalid_argument "Dynarray.set");
  a.contents.(i) <- x

let push_back a x =
  let i = length a in
  if i >= Array.length a.contents then
    expand a;

  a.length <- a.length + 1;
  set a i x;
  i

let shrink a i =
  if i >= Array.length a.contents then raise (Invalid_argument "Dynarray.shrink");
  a.length <- i

let map f a =
  for i = 0 to a.length - 1 do
    a.contents.(i) <- f a.contents.(i)
  done
