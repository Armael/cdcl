(* Useful operators *)
let ( |> ) x f = f x
let ( @@ ) f x = f x
let ( % ) f g = fun x -> f (g x)

(* The identity function *)
let identity = fun x -> x

(* Give a function that returns the value x *)
let const x = fun _ -> x

(* Iterates a function n times *)
let rec do_n n f = if n > 0 then (f (); do_n (n-1) f)

(* Takes a literal between -n and n (for an unknown n), and returns an
   index between 0 and 2*n. aid is bijective, so it's useful to index
   array by literals *)
let aid var = 2 * (abs var - 1) + (if var > 0 then 0 else 1)

(* Cowardly exits the program *)
let die msg =
  Printf.printf "%s\n%!" msg;
  exit 1

(* Returns the position in the string s of the first non blank
   character, bundled in an option type : if the string is entirely
   blank, None is returned *)
let strip_begin s =
  let i = ref 0 in
  while !i < String.length s && s.[!i] = ' ' do
    incr i
  done;
  (if !i < String.length s then Some !i else None)

(* Returns a list describing the content of the queue q *)
let queue_content q =
  List.rev (Queue.fold (fun acc x -> x::acc) [] q)

let hash_revmap h =
  let rev_h = Hashtbl.create (Hashtbl.length h) in
  Hashtbl.iter (fun k v -> Hashtbl.replace rev_h v k) h;
  rev_h

(*
 * Utility functions on list (iterators,…)
 *)

(* Returns the list [a; a+1; ...; b] *)
let rec seq a b =
  if a > b then []
  else a::(seq (a+1) b)

let aseq a b =
  Array.init (b - a + 1) ((+) a)

let rec uniq = function
  | [] -> []
  | [x] -> [x]
  | x::y::xs -> if x = y then uniq (y::xs)
    else x::(uniq (y::xs))

(***************************
  Some specialized iterators
****************************)

(* Just like List.fold_left, but also pass to the function f the list
   of the elements after the current one.

   It is useful in particular to make imbricated iterations in
   n(n-1)/2 on a list *)
let rec fold_left_tail f acc = function
  | [] -> acc
  | x::xs -> fold_left_tail f (f acc xs x) xs

(* Just like List.iter, but also pass to f the list of the elemets
   after the current one *)
let rec iter_tail f = function
  | [] -> ()
  | x::xs -> f x xs; iter_tail f xs

(* Just like List.fold_left, but the function f returns a couple
   (continue?, new_acc). If continue? = false, we return new_acc,
   otherwise we continue the iteration (like a normal fold)
*)
let rec fold_stop f acc = function
  | [] -> acc
  | x::xs -> let (continue, new_acc) = f acc x in
             if continue then
               fold_stop f new_acc xs
             else
               new_acc

(* Just like List.map, but f returns a option type. For a given, if
   the result is None, the element is ignored, otherwise the content of
   the Some _ is mapped *)
let rec map_some f = function
  | [] -> []
  | x::xs -> match f x with
    | None -> map_some f xs
    | Some y -> y::(map_some f xs)

(* Just like List.map, but removes items for which f returns []. *)
let rec map_skip f = function
  | [] -> []
  | x::xs -> match f x with
    | [] -> map_skip f xs
    | y -> y::(map_skip f xs)

(* Same.
   Tail-rec version *)
let map_skip_tr f l =
  let rec aux acc = function
    | [] -> List.rev acc
    | x::xs -> match f x with
      | [] -> aux acc xs
      | y -> aux (y::acc) xs in
  aux [] l

(* Remove duplicates in a sorted list.
   Tail-rec version *)
let remove_dup l =
  let rec aux acc = function
    | [] -> acc
    | [x] -> x::acc
    | x::y::xs -> if x = y then aux acc (x::xs)
    else aux (x::acc) (y::xs) in
  List.rev (aux [] l)

(* Merge two sorted lists in a sorted list *)
let rec merge_sorted compare l1 l2 = match (l1, l2) with
  | ([], _) -> l2
  | (_, []) -> l1
  | (x::xs, y::ys) -> if compare x y < 0 then
      x::(merge_sorted compare xs (y::ys)) else
      y::(merge_sorted compare (x::xs) ys)

let merge_sorted_tr compare l1 l2 =
  let rec aux acc l1 l2 = match (l1, l2) with
    | ([], _) -> List.rev_append acc l2
    | (_, []) -> List.rev_append acc l1
    | (x::xs, y::ys) -> if compare x y < 0 then
        aux (x::acc) xs (y::ys) else
        aux (y::acc) (x::xs) ys in
  aux [] l1 l2


let dump_string filename s =
  let chan = open_out filename in
  output_string chan s;
  close_out chan

let dump_chan chan =
  let b = Buffer.create 217 in
  try
    while true do
      Buffer.add_string b (input_line chan);
      Buffer.add_char b '\n'
    done;
    assert false
  with End_of_file -> b

(*
 * Utility functions on tuples
 *)

let put_fst (a, b) f = (f a, b)
let put_snd (a, b) f = (a, f b)
let map2 f (a, b) = (f a, f b)
let fst3 (a, _, _) = a


(* Overloading Buffer to add a get_lines function :
   val get_lines : Buffer.t -> (unit -> string)

   get_lines returns a function that returns, at each call, a new
   line, or End_of_file if there is no new *)
module Buffer = struct
  include Buffer

  let get_lines buf =
    let pos = ref 0 in
    fun () ->
      if !pos >= Buffer.length buf then raise End_of_file;
      let length = ref 0 in
      while (!pos + !length) < Buffer.length buf &&
            Buffer.nth buf (!pos + !length) <> '\n' do
        incr length
      done;
      let line = Buffer.sub buf !pos !length in
      pos := !pos + !length + 1;
      line
end

module IntSet = Set.Make (struct
  type t = int
  let compare = compare
end)

module Option = struct
  let iter f = function
    | None -> ()
    | Some x -> f x
end

module Array = struct
  type 'a t = 'a array
  include Array

  let for_all p a =
    let b = ref true in
    let i = ref 0 in
    while !b && !i < Array.length a do
      b := !b && (p a.(!i));
      incr i
    done;
    !b
end
