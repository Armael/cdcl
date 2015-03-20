open Prelude

let verif (nb_vars, nb_clauses, clauses) values =
  let tbl = Hashtbl.create nb_vars in

  (* Initialize a hashtable with the values of the variables *)
  Array.iteri (fun i b -> Hashtbl.add tbl (i+1) b) values;

  let verif_clause clause =
    List.fold_left (fun acc v ->
      (if v > 0 then (Hashtbl.find tbl v)
       else not (Hashtbl.find tbl (abs v))) || acc) false clause in

  fold_stop (fun acc c ->
    let nacc = acc && (verif_clause c) in
    (nacc, nacc)) true clauses
