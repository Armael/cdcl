open Prelude

(* Type representing a raw .cnf file :
   - the first value is the number of vars,
   - the second the number of clauses,
   - the third is the list of the clauses,
   each clause being a list of its literals
*)
type t = int * int * int list list

(* The parser of .cnf files
   
   The parse function takes an already opened file, read its content,
   and using a int list list type, represents a list of the clauses,
   where a clause is itself a list of literals (ints)
*)

let parse_clause s =
  let i = ref 0 in
  let vars = ref [] in
  try
    while true do
      Scanf.bscanf 
        (Scanf.Scanning.from_function
           (fun () ->
             if !i < String.length s then (
               let c = s.[!i] in
               incr i; c
             ) else raise End_of_file
           ))
        " %d"
        (fun x -> vars := x::!vars)
    done; assert false (* never reached *)
  with End_of_file -> List.tl !vars (* remove the final '0' *)

let parse buffer : t =
  let clauses = ref [] in
  let nb_vars = ref 0
  and nb_clauses = ref 0 in

  let input_line = Buffer.get_lines buffer in
  
  (try
     while true do
       let line = input_line () in
       let i = strip_begin line in
       Option.iter (fun i -> match line.[i] with
       | 'p' -> let (v, c) = Scanf.sscanf line "p cnf %d %d" (fun x y -> (x,y)) in
                nb_vars := v
       (* We ignore the given number of clauses, since it is sometimes
          incorrect *)
       (* nb_clauses := c *)
       | 'c' -> ()
       | _ -> (* So it's a clause *)
         incr nb_clauses;
         clauses := (parse_clause line)::!clauses) i
     done
   with End_of_file -> ());
  (!nb_vars, !nb_clauses, !clauses)


let buffer_out (cnf : t) buf =
  let (nvars, nclauses, clauses) = cnf in
  let open Printf in
  
  bprintf buf "p cnf %d %d\n" nvars nclauses;
  List.iter (fun cl ->
    List.iter (fun lit -> bprintf buf "%d " lit) cl;
    bprintf buf "0\n"
  ) clauses
