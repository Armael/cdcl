val ( |> ) : 'a -> ('a -> 'b) -> 'b
val ( @@ ) : ('a -> 'b) -> 'a -> 'b
val ( % ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val identity : 'a -> 'a
val const : 'a -> 'b -> 'a
val do_n : int -> (unit -> 'a) -> unit
val aid : int -> int
val die : string -> 'a
val strip_begin : string -> int option
val queue_content : 'a Queue.t -> 'a list
val hash_revmap : ('a, 'b) Hashtbl.t -> ('b, 'a) Hashtbl.t
val seq : int -> int -> int list
val aseq : int -> int -> int array
val uniq : 'a list -> 'a list
val fold_left_tail : ('a -> 'b list -> 'b -> 'a) -> 'a -> 'b list -> 'a
val iter_tail : ('a -> 'a list -> 'b) -> 'a list -> unit
val fold_stop : ('a -> 'b -> bool * 'a) -> 'a -> 'b list -> 'a
val map_some : ('a -> 'b option) -> 'a list -> 'b list
val map_skip : ('a -> 'b list) -> 'a list -> 'b list list
val map_skip_tr : ('a -> 'b list) -> 'a list -> 'b list list
val remove_dup : 'a list -> 'a list
val merge_sorted : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
val merge_sorted_tr : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
val dump_string : string -> string -> unit
val dump_chan : in_channel -> Buffer.t
val put_fst : 'a * 'b -> ('a -> 'c) -> 'c * 'b
val put_snd : 'a * 'b -> ('b -> 'c) -> 'a * 'c
val map2 : ('a -> 'b) -> 'a * 'a -> 'b * 'b
val fst3 : 'a * 'b * 'c -> 'a

module Buffer :
  sig
    type t = Buffer.t
    val create : int -> t
    val contents : t -> string
    val to_bytes : t -> bytes
    val sub : t -> int -> int -> string
    val blit : t -> int -> bytes -> int -> int -> unit
    val nth : t -> int -> char
    val length : t -> int
    val clear : t -> unit
    val reset : t -> unit
    val add_char : t -> char -> unit
    val add_string : t -> string -> unit
    val add_bytes : t -> bytes -> unit
    val add_substring : t -> string -> int -> int -> unit
    val add_subbytes : t -> bytes -> int -> int -> unit
    val add_substitute : t -> (string -> string) -> string -> unit
    val add_buffer : t -> t -> unit
    val add_channel : t -> in_channel -> int -> unit
    val output_buffer : out_channel -> t -> unit
    val get_lines : Buffer.t -> unit -> string
  end

module IntSet :
  sig
    type elt = int
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
    val of_list : elt list -> t
  end

module Option : sig val iter : ('a -> unit) -> 'a option -> unit end

module Array :
  sig
    type 'a t = 'a array
    external length : 'a array -> int = "%array_length"
    external get : 'a array -> int -> 'a = "%array_safe_get"
    external set : 'a array -> int -> 'a -> unit = "%array_safe_set"
    external make : int -> 'a -> 'a array = "caml_make_vect"
    external create : int -> 'a -> 'a array = "caml_make_vect"
    val init : int -> (int -> 'a) -> 'a array
    val make_matrix : int -> int -> 'a -> 'a array array
    val create_matrix : int -> int -> 'a -> 'a array array
    val append : 'a array -> 'a array -> 'a array
    val concat : 'a array list -> 'a array
    val sub : 'a array -> int -> int -> 'a array
    val copy : 'a array -> 'a array
    val fill : 'a array -> int -> int -> 'a -> unit
    val blit : 'a array -> int -> 'a array -> int -> int -> unit
    val to_list : 'a array -> 'a list
    val of_list : 'a list -> 'a array
    val iter : ('a -> unit) -> 'a array -> unit
    val map : ('a -> 'b) -> 'a array -> 'b array
    val iteri : (int -> 'a -> unit) -> 'a array -> unit
    val mapi : (int -> 'a -> 'b) -> 'a array -> 'b array
    val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a
    val fold_right : ('b -> 'a -> 'a) -> 'b array -> 'a -> 'a
    external make_float : int -> float array = "caml_make_float_vect"
    val sort : ('a -> 'a -> int) -> 'a array -> unit
    val stable_sort : ('a -> 'a -> int) -> 'a array -> unit
    val fast_sort : ('a -> 'a -> int) -> 'a array -> unit
    external unsafe_get : 'a array -> int -> 'a = "%array_unsafe_get"
    external unsafe_set : 'a array -> int -> 'a -> unit = "%array_unsafe_set"
    val for_all : ('a -> bool) -> 'a array -> bool
  end
