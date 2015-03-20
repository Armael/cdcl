(* Takes a literal between -n and n (for an unknown n), and returns an
   index between 0 and 2*n. aid is bijective, so it's useful to index
   array by literals *)
let aid var = 2 * (abs var - 1) + (if var > 0 then 0 else 1)

let lit_iter nvars f =
  for i = 1 to nvars do
    f i; f (-i)
  done

(* Cowardly exits the program *)
let die msg =
  Printf.printf "%s\n%!" msg;
  exit 1

(* Returns the list [a; a+1; ...; b] *)
let rec seq a b =
  if a > b then []
  else a::(seq (a+1) b)

let aseq a b =
  Array.init (b - a + 1) ((+) a)
    
let rec fold_stop f acc = function
  | [] -> acc
  | x::xs -> let (continue, new_acc) = f acc x in
    if continue then
      fold_stop f new_acc xs
    else
      new_acc

(* Returns the position in the string s of the first non blank
   character, bundled in an option type : if the string is entirely
   blank, None is returned *)
let strip_begin s =
  let i = ref 0 in
  while !i < String.length s && s.[!i] = ' ' do
    incr i
  done;
  (if !i < String.length s then Some !i else None)

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

let string_dump chan =
  let buf = Buffer.create 200 in
  try
    while true do
      let l = input_line chan in
      Buffer.add_string buf (l ^ "\n")
    done; assert false
  with End_of_file -> Buffer.contents buf

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
