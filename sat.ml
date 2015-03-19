open Prelude
open Debug

type state = {
  nvars: int;
  assign: (int, int, int array) Bt.t;
  clauses: int Array.t DynArray.t;
  mutable learnt_clauses_begin: int;

  (* Propagation *)
  wl_of_clause: (int * int) DynArray.t;
  clauses_of_wl: int DynArray.t LitArray.t;
  queue: (int (* lit *) *
          int (* cause clause *)) Queue.t;

  (* Clause learning *)
  propagation_log: (int * (* propagated literal *)
                    int (* cause clause *)) DynArray.t;
      (* Si "cause clause" est -1, alors il s'agit
         d'une décision, non d'une conséquence *)
  propagation_bt: int DynArray.t;
  mutable decision_level: int;
  mutable conflicting_clause: int;

  (* Heuristique *)
  lit_activity: float LitArray.t;
  lit_activity_increment: float;
  lit_activity_decay: float;
  (* clause_activity *)
  (* increment *)
  (* decay *)
  (* restart limit *)
  (* multiplication du restart limite *)
  (* limite du nombre total de clauses apprises *)
  (* multiplieur pour la limite du nb de clauses *)
}

let print_assign st =
  p1 "Assign:";
  for i = 1 to st.nvars do
    match Bt.get st.assign i with
    | 0 -> p1 " -"
    | 1 -> p1 " %d" i 
    | _ -> p1 " %d" (-i)
  done;
  p1 "\n"

let discriminate st cl =
  let rec aux (idk, y, n) i =
    if i = Array.length cl then (idk, y, n)
    else
      aux
        (match Bt.get st.assign cl.(i) with
         | 0 -> (cl.(i)::idk, y, n)
         | 1 -> (idk, cl.(i)::y, n)
         | _ (* -1 *) -> (idk, y, cl.(i)::n))
        (i+1)
  in
  aux ([], [], []) 0

let most_recent st l =
  let rec aux i =
    if i < 0 then (p0 "dafuq??\n"; List.hd l)
    else (try List.find (fun x -> abs x = abs (fst (DynArray.get st.propagation_log i))) l
          with Not_found ->
            aux (i-1)) in
  aux (DynArray.length st.propagation_log - 1)

let pick_two_wl st cl =
  match discriminate st cl with
  | (x1::x2::_, _, _) -> (x1, x2), 0
  | ([x], [], []) -> (x, 0), x
  | ([x], [], l) -> (x, most_recent st l), x
  | ([x], l, _) -> (x, most_recent st l), 0
  | ([], [], _) -> (* clause fausse *) assert false
  | ([], [x], []) -> (x, 0), 0
  | ([], [x], l) ->
    p1 "hummmmm???\n";
    (x, most_recent st l), 0
  | ([], l, l') ->
    (* n'est pas censé arriver je pense; on fait un truc à la noix *)
    p1 "hummmmm????\n";
    (most_recent st l, most_recent st l'), 0

let watch_new_clause st cl_id =
  let (u, v), propg = pick_two_wl st (DynArray.get st.clauses cl_id) in
  p1 "(%d) watched: (%d, %d); propagated: %d\n%!" cl_id u v propg;
  if propg <> 0 then Queue.add (propg, cl_id) st.queue;
  DynArray.set st.wl_of_clause cl_id (u, v);
  if u <> 0 then
    DynArray.push_back (LitArray.get st.clauses_of_wl u) cl_id |> ignore;
  if v <> 0 then
    DynArray.push_back (LitArray.get st.clauses_of_wl v) cl_id |> ignore

let add_new_clause st cl =
  let new_id = DynArray.push_back st.clauses cl in
  DynArray.push_back st.wl_of_clause (0,0) |> ignore;
  new_id

let check_watched st =
  for i = 1 to st.nvars do
    List.iter (fun i ->
      let cls = LitArray.get st.clauses_of_wl i in
      for j = 0 to DynArray.length cls - 1 do
        let (u, v) = DynArray.get st.wl_of_clause (DynArray.get cls j) in
        f1 (fun () -> assert (u = i || v = i))
      done
    ) [i; -i]
  done

let init_state ((nvars, nclauses, clauses): Cnf.t): state =
  let assign = Bt.mk_backtrackable
      ~get:(fun a i -> if i > 0 then a.(i-1) else - a.((abs i) - 1))
      ~set:(fun a i x -> if i > 0 then a.(i-1) <- x else a.((abs i) - 1) <- - x)
      (Array.make nvars 0) in
  let cls = ref clauses in
  let clauses = DynArray.init nclauses (fun _ ->
    let arr = Array.of_list (List.hd !cls) in (* todo: éviter ça au parsing (cnf.ml) *)
    cls := List.tl !cls;
    arr) in

  let wl_of_clause = DynArray.make nclauses (0,0) in
  let clauses_of_wl = LitArray.fmake nvars (fun () -> DynArray.make 0 0) in

  let propagation_log = DynArray.make 0 (0, -1) in
  let propagation_bt = DynArray.make 0 0 in
  
  let lit_activity = LitArray.make nvars 0. in
  for i = 0 to nclauses - 1 do
    Array.iter (fun lit ->
      LitArray.set lit_activity lit ((LitArray.get lit_activity lit) +. 1.)
    ) (DynArray.get clauses i)
  done;

  let st =
    { nvars;
      assign;
      clauses;
      learnt_clauses_begin = nclauses;
      
      wl_of_clause;
      clauses_of_wl;
      queue = Queue.create ();

      propagation_log;
      propagation_bt;
      decision_level = 0;
      conflicting_clause = -1;
    
      lit_activity;
      lit_activity_increment = 1.;
      lit_activity_decay = 1. /. 0.95;
    }
  in

  p1 "0/\n";
  
  (* On initialise les watched literals *)
  for cl_id = 0 to nclauses - 1 do
    watch_new_clause st cl_id
  done;
  st

(* retourne [true] si on peut continuer, [false] s'il y a eu un conflit.
   Dans le cas où [false] est retourné, [st.conflicting_clause] doit indiquer
   l'id de la cause fausse qui a signalé le conflit.
*)
let rec propagate (st: state): bool =
  if Queue.is_empty st.queue then true
  else begin
    let (v, cl_cause) = Queue.take st.queue in

    let v = -v in (* on propage v = FALSE *)
    p1 "PROPAGATING %d = false\n\n%!" (v);
    
    match Bt.get st.assign v with
    | -1 -> true
    | 1 -> st.conflicting_clause <- cl_cause; false
    | _ (* 0 *) ->
      if cl_cause = -1 then
        DynArray.push_back st.propagation_bt (DynArray.length st.propagation_log)
        |> ignore;
      DynArray.push_back st.propagation_log (-v, cl_cause) |> ignore;
      Bt.set st.assign v (-1);
      
      let clauses_of_v = DynArray.make 0 0 in
      
      (* retourne [false] en cas de conflit *)
      let choose_new_lit cl_id =
        let (v1, v2) = DynArray.get st.wl_of_clause cl_id in
        f1 (fun () -> assert (v1 = v || v2 = v));
        let v' = if v1 = v then v2 else v1 in

        (* Printf.printf "Currently watching (%d,%d) for clause %d; choose new lit\n%!" v1 v2 cl_id; *)

        if v' = 0 then (
          (* cl_id correspond à la clause [v], et v est faux.
             Il faut backtracker *)
          st.conflicting_clause <- cl_id;
          false
        ) else if Bt.get st.assign v' = 1 then (
          DynArray.push_back clauses_of_v cl_id |> ignore;
          true
        ) else (
          print_assign st;
          (* Printf.printf "cl_id: %d; v: %d; v': %d\n%!" cl_id v v'; *)

          match discriminate st (DynArray.get st.clauses cl_id) with
          | ([], [], _) -> (* conflit *)
            st.conflicting_clause <- cl_id;
            false
          | ([], _, _) -> assert false
          | ([x], [], _) ->
            f1 (fun () -> assert (x = v'));
            (* Printf.printf "Cant watch someone else, propagating %d = true\n%!" v'; *)
            Queue.add (v', cl_id) st.queue;
            (* on continue de regarder v; pas de meilleur candidat, et normalement
               v' sera assigné à vrai par la propagation *)
            DynArray.push_back clauses_of_v cl_id |> ignore;
            true
          | ([x], l, _) ->
            f1 (fun () -> assert (x = v'));
            let new_v = most_recent st l in
            (* Printf.printf "new_v: %d\n%!" new_v; *)
            DynArray.set st.wl_of_clause cl_id (new_v, v');
            DynArray.push_back (LitArray.get st.clauses_of_wl new_v) cl_id |> ignore;
            true
          | (l, _, _) ->
            let new_v = List.find ((<>) v') l in
            (* Printf.printf "new_v: %d\n%!" new_v; *)
            DynArray.set st.wl_of_clause cl_id (new_v, v');
            DynArray.push_back (LitArray.get st.clauses_of_wl new_v) cl_id |> ignore;
            true
        )
      in

      let cls_of_v = LitArray.get st.clauses_of_wl v in

      check_watched st;

      match
        for i = 0 to DynArray.length cls_of_v - 1 do
          if not (choose_new_lit (DynArray.get cls_of_v i)) then (
            for j = i to DynArray.length cls_of_v - 1 do
              DynArray.push_back clauses_of_v (DynArray.get cls_of_v j) |> ignore
            done;
            LitArray.set st.clauses_of_wl v clauses_of_v;
            raise Exit
          )
        done;
        LitArray.set st.clauses_of_wl v clauses_of_v
      with
      | () ->
        check_watched st;
        if not (Queue.is_empty st.queue) then propagate st else true
      | exception Exit ->
        check_watched st;
        false
  end
    
let incr_lit_activity st x =
  LitArray.set st.lit_activity x
    ((LitArray.get st.lit_activity x) +. st.lit_activity_increment)

let decay_lit_activity st =
  LitArray.map (fun a -> a *. st.lit_activity_decay) st.lit_activity

let print_propagation_log st =
  p1 "Propagation log:";
  for i = 0 to DynArray.length st.propagation_log - 1 do
    let (lit, cause_cl) = DynArray.get st.propagation_log i in
    if cause_cl = (-1) then p1 " |";
    p1 " (%d, %d)" lit cause_cl
  done; p1 "\n"

let conflict_analysis (st: state): int * int (* id de la nouvelle clause apprise, 
                                                nombre de backtracks *) =
  let set_of_cl cl = Array.to_list cl |> IntSet.of_list in
  let cl_of_set s = IntSet.elements s |> Array.of_list in
    
  let false_clause = DynArray.get st.clauses st.conflicting_clause in
  p1 "false clause %d:" st.conflicting_clause;
  Array.iter (fun i -> p1 " %d" i) false_clause;
  p1 "\n";

  print_propagation_log st;

  let temp_clause = ref (set_of_cl false_clause) in

  let temp_is_unit () = IntSet.min_elt !temp_clause = IntSet.max_elt !temp_clause in

  let temp_is_uip () =
    p1 "<temp_is_uip> ";
    p1 "temp clause :" ;
    IntSet.iter (fun i -> p1 " %d" i) !temp_clause;
    p1 "\n";

    if temp_is_unit () then
      IntSet.min_elt !temp_clause
    else begin
      let n = ref 0 and i = ref (DynArray.length st.propagation_log - 1) in
      let uip = ref 0 in
      let continue = ref true in
      while !n <= 1 && !continue do
        let (lit, cause_cl) = DynArray.get st.propagation_log !i in
        if cause_cl = (-1) then continue := false;
        if IntSet.mem (-lit) !temp_clause then (
          uip := (-lit);
          incr n
        );
        decr i
      done;
      f1 (fun () -> assert (!n >= 1));
      if !n = 1 then !uip else 0
    end
  in

  let find_resolving_lit_and_cl () =
    let resolution = ref (0, -1) in
    let i = ref (DynArray.length st.propagation_log - 1) in
    let l = DynArray.get st.propagation_bt (DynArray.length st.propagation_bt - 1) in
    (* on ne veut pas regarder le littéral décidé, seulement ceux déduits *)
    while !resolution = (0, -1) && (!i > l) do
      let (lit, cause_cl) = DynArray.get st.propagation_log !i in
      if IntSet.mem (-lit) !temp_clause then
        resolution := (lit, cause_cl);
      decr i
    done;
    !resolution
  in

  let resolve cl1 cl2 lit =
    IntSet.remove lit (IntSet.remove (-lit) (IntSet.union cl1 cl2))
  in

  let temp_is_false () =
    IntSet.for_all (fun lit -> Bt.get st.assign lit = -1) !temp_clause
  in

  let rec loop () =
    f1 (fun () -> assert (temp_is_false ()));
    (* on s'arrête au premier UIP *)
    let uip = temp_is_uip () in
    if uip <> 0 then uip
    else match find_resolving_lit_and_cl () with
      | (0, -1) -> assert false
      | (lit, cause_cl_id) ->
        incr_lit_activity st lit;
        temp_clause := resolve
            !temp_clause
            (DynArray.get st.clauses cause_cl_id |> set_of_cl)
            lit;
        f1 (fun () -> assert (temp_is_false ()));
        loop ()
  in
  let uip = loop () in

  f1 (fun () -> assert (temp_is_false ()));

  IntSet.iter (incr_lit_activity st) !temp_clause;

  let bt_steps =
    if temp_is_unit () then
      1 (* le seul littéral de la clause est forcément l'uip, qui a été
           décidé au niveau courant *)
    else begin
      let steps = ref 0 in
      (try
         for i = DynArray.length st.propagation_log - 1 downto 0 do
           let (lit, cause_cl) = DynArray.get st.propagation_log i in
           if cause_cl = (-1) then incr steps;
           if (-lit) <> uip && IntSet.mem (-lit) !temp_clause then
             raise Exit
         done
       with Exit -> ());
      !steps
    end
  in

  (* On apprend !temp_clause *)
  let new_id = add_new_clause st (cl_of_set !temp_clause) in
  p1 "learning clause %d:" new_id;
  IntSet.iter (fun i -> p1 " %d" i) !temp_clause;
  p1 "\n";
  
  (new_id, bt_steps)

let pick_branching_variable (st: state): int =
  let x = ref (0, -1.) in
  for i = 1 to st.nvars do
    List.iter (fun i ->
      let a = LitArray.get st.lit_activity i in
      if a > snd !x && Bt.get st.assign i = 0 then
        x := (i, a)
    ) [i; -i]
  done;
  fst !x

type outcome =
  | UnSat
  | Sat of bool Array.t

let total_assign st =
  Array.for_all ((<>) 0) st.assign.Bt.container

let cdcl (st: state): outcome =
  check_watched st;
  
  match propagate st with
  | false (* conflict *) -> UnSat
  | true ->
    p1 "2/";
    let unsat = ref false in
    while not !unsat && not (total_assign st) do
      check_watched st;
      
      if Queue.is_empty st.queue then (
        let x = pick_branching_variable st in
        p1 "DECIDING %d = true\n%!" x;
      
        Bt.push_state st.assign;
        st.decision_level <- st.decision_level + 1;
        Queue.add (x, -1) st.queue;
      );

      match propagate st with
      | true -> p1 "propagation ok\n"; ()
      | false ->
        p1 "conflict\n";
        Queue.clear st.queue;
        
        let new_clause_id, bt_steps = conflict_analysis st in
        p1 "Backtracking of %d steps\n%!" bt_steps;
          
        if bt_steps > st.decision_level then
          unsat := true
        else (
          (* Decay des activités à chaque conflit *)
          decay_lit_activity st;
          
          (* on backtrack *)
          (* nettoyage *)
          st.conflicting_clause <- (-1);
          
          let i = ref (DynArray.get st.propagation_bt (DynArray.length st.propagation_bt - bt_steps)) in
          DynArray.shrink st.propagation_bt (DynArray.length st.propagation_bt - bt_steps);
          DynArray.shrink st.propagation_log (!i + 1);
          st.decision_level <- st.decision_level - bt_steps;
          Bt.pop_n_state st.assign bt_steps;

          print_propagation_log st;

          watch_new_clause st new_clause_id
        )
    done;

    if !unsat then UnSat
    else
      let a = Array.make st.nvars false in
      for i = 1 to st.nvars do
        a.(i-1) <- (Bt.get st.assign i > 0)
      done;
      Sat a
