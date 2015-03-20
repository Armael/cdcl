open Prelude

(* Mutable global state; shared by all functions *)
type state = {
  nvars: int;
  assign: (int, int, int array) Bt.t;
  clauses: int Array.t DynArray.t;
  mutable learnt_clauses_begin: int;

  (*** Propagation: structures for watched literals + propagation queue *)
  wl_of_clause: (int * int) DynArray.t;
  clauses_of_wl: int DynArray.t LitArray.t;
  queue: (int (* lit *) *
          int (* cause clause *)) Queue.t;
  mutable is_unsat: bool;

  (**** Clause learning structures *)
  (* If "cause clause" is -1, then this literal has been
     propagated due to a decision, not deduced *)
  propagation_log: (int * (* propagated literal *)
                    int (* cause clause *)) DynArray.t;
  (* List of the index of the decided literals. Useful
     for backtracking propagation_log *)
  propagation_bt: int DynArray.t;
  (* Current decision level *)
  mutable decision_level: int;
  (* After a conflict, set to the false clause that has
     been detected *)
  mutable conflicting_clause: int;

  (**** Heuristic-related structures *)
  (* Activity (VSIDS) *)
  lit_activity: float LitArray.t;
  lit_activity_increment: float;
  lit_activity_decay: float;
  mutable restart_limit: int;
  restart_mult: float;
  mutable conflicts_counter: int;
}

type outcome =
  | UnSat
  | Sat of bool Array.t

(* Debug related functions ****************************************************)

let print_assign st =
  print_string "Assign:";
  for i = 1 to st.nvars do
    match Bt.get st.assign i with
    | 0 -> Printf.printf " -"
    | 1 -> Printf.printf " %d" i
    | _ -> Printf.printf " %d" (-i)
  done; print_endline ""

(* check that the two wl-related arrays are coherent *)
let check_watched st =
  lit_iter st.nvars (fun i ->
    let cls = LitArray.get st.clauses_of_wl i in
    for j = 0 to DynArray.length cls - 1 do
      let (u, v) = DynArray.get st.wl_of_clause (DynArray.get cls j) in
      assert (u = i || v = i)
    done
  )

let print_propagation_log st =
  print_string "Propagation log:";
  for i = 0 to DynArray.length st.propagation_log - 1 do
    let (lit, cause_cl) = DynArray.get st.propagation_log i in
    if cause_cl = (-1) then print_string " |";
    Printf.printf " (%d, %d)" lit cause_cl
  done; print_endline "";
  for i = 0 to DynArray.length st.propagation_bt - 1 do
    Printf.printf " %d" (DynArray.get st.propagation_bt i);
  done;
  print_endline "<<"

(******************************************************************************)

(* Propagation (Watched-literals) utility functions ***************************)

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
    if i < 0 then (Debug.p 2 "most_recent: weird stuff happening\n"; List.hd l)
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
  | ([], [], _) -> (* false clause *)
    st.is_unsat <- true;
    (0, 0), 0 (* dummy *)
  | ([], [x], []) -> (x, 0), 0
  | ([], [x], l) ->
    Debug.p 1 "pick_two_wl: weird stuff happening\n";
    (x, most_recent st l), 0
  | ([], l, l') ->
    (* n'est pas censé arriver je pense; on fait un truc à la noix *)
    Debug.p 1 "pick_two_wl: weird stuff happening\n";
    (most_recent st l, most_recent st l'), 0


let add_new_clause st cl =
  let new_id = DynArray.push_back st.clauses cl in
  DynArray.push_back st.wl_of_clause (0,0) |> ignore;
  new_id

let watch_new_clause st cl_id =
  let (u, v), propg = pick_two_wl st (DynArray.get st.clauses cl_id) in
  Debug.p 2 "(new clause %d) watched: (%d, %d); propagated: %d\n%!" cl_id u v propg;
  if propg <> 0 then Queue.add (propg, cl_id) st.queue;
  DynArray.set st.wl_of_clause cl_id (u, v);
  if u <> 0 then
    DynArray.push_back (LitArray.get st.clauses_of_wl u) cl_id |> ignore;
  if v <> 0 then
    DynArray.push_back (LitArray.get st.clauses_of_wl v) cl_id |> ignore

(******************************************************************************)

(* Heuristic utility functions ************************************************)

let incr_lit_activity st x =
  LitArray.set st.lit_activity x
    ((LitArray.get st.lit_activity x) +. st.lit_activity_increment)

let decay_lit_activity st =
  LitArray.map (fun a -> a *. st.lit_activity_decay) st.lit_activity

let total_assign st =
  Array.for_all ((<>) 0) st.assign.Bt.container

(******************************************************************************)

(* State-handling functions: initialize/reset the state ***********************)

(* Create a state from a cnf formula *)
let init_state ((nvars, nclauses, clauses): Cnf.t): state =
  let assign = Bt.mk_backtrackable
      ~get:(fun a i -> if i > 0 then a.(i-1) else - a.((abs i) - 1))
      ~set:(fun a i x -> if i > 0 then a.(i-1) <- x else a.((abs i) - 1) <- -x)
      (Array.make nvars 0) in
  let cls = ref clauses in
  let clauses = DynArray.init nclauses (fun _ ->
    let arr = Array.of_list (List.hd !cls |> List.sort_uniq compare) in
    cls := List.tl !cls;
    arr
  ) in

  let wl_of_clause = DynArray.make nclauses (0,0) in
  let clauses_of_wl = LitArray.fmake nvars (fun () -> DynArray.make 0 0) in

  let propagation_log = DynArray.make 0 (0, -1) in
  let propagation_bt = DynArray.make 0 0 in

  let lit_activity = LitArray.make nvars 0. in

  let st =
    { nvars;
      assign;
      clauses;
      learnt_clauses_begin = nclauses;

      wl_of_clause;
      clauses_of_wl;
      queue = Queue.create ();
      is_unsat = false;

      propagation_log;
      propagation_bt;
      decision_level = 0;
      conflicting_clause = -1;

      lit_activity;
      lit_activity_increment = 1.;
      lit_activity_decay = 1. /. 0.95;
      restart_limit = 100;
      restart_mult = 1.5;
      conflicts_counter = 0;
    }
  in

  (* Initialize the watched literals *)
  for cl_id = 0 to nclauses - 1 do
    watch_new_clause st cl_id
  done;
  st

(* Reset the state. Used for restarts *)
let reset_state st =
  for i = 1 to st.nvars do
    (* clear assignations *)
    Bt.set st.assign i 0
  done;
  (* clear the backtrack stack *)
  Bt.forget st.assign;

  Queue.clear st.queue;
  DynArray.shrink st.propagation_log 0;
  DynArray.shrink st.propagation_bt 0;
  st.decision_level <- 0;
  st.conflicting_clause <- -1;

  (* reset literals activities *)
  lit_iter st.nvars (fun i ->
    LitArray.set st.lit_activity i 0.
  );

  (* we reset watching literals because
     choosing watching literals for a clause
     may trigger propagation steps we don't want
     to miss *)
  DynArray.map (fun _ -> (0, 0)) st.wl_of_clause;
  lit_iter st.nvars (fun i ->
    DynArray.shrink (LitArray.get st.clauses_of_wl i) 0
  )

(* Main functions: propagation, decision, conflict analysis, main loop ********)

(* The propagation function

   Returns [true] if we can continue, [false] if a conflict happened.
   If [false] is returned, [st.conflicting_clause] must contain the id
   of the false clause that triggered the conflict.
*)
let rec propagate (st: state): bool =
  if st.is_unsat then false (* doesn't matter actually *)
  else if Queue.is_empty st.queue then true
  else begin
    let (v, cl_cause) = Queue.take st.queue in

    let v = -v in (* we propagate v = FALSE *)
    Debug.p 2 "PROPAGATING %d = true\n\n%!" (-v);

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

      (* Returns [false] if there was a conflict *)
      let choose_new_lit cl_id =
        let (v1, v2) = DynArray.get st.wl_of_clause cl_id in
        Debug.f 1 (fun _ -> assert (v1 = v || v2 = v));
        let v' = if v1 = v then v2 else v1 in

        if v' = 0 then (
          (* cl_id corresponds to clause [v], and v is false.
             We must backtrack *)
          st.conflicting_clause <- cl_id;
          false
        ) else if Bt.get st.assign v' = 1 then (
          DynArray.push_back clauses_of_v cl_id |> ignore;
          true
        ) else (
          Debug.f 2 (fun _ -> print_assign st);
          match discriminate st (DynArray.get st.clauses cl_id) with
          | ([], [], _) -> (* conflit *)
            st.conflicting_clause <- cl_id;
            false
          | ([], _, _) -> assert false
          | ([x], [], _) ->
            Queue.add (v', cl_id) st.queue;
            (* We continue to watch v; there is no better candidate, and v'
               should be set to true by the incoming propagation *)
            DynArray.push_back clauses_of_v cl_id |> ignore;
            true
          | ([x], l, _) ->
            let new_v = most_recent st l in
            DynArray.set st.wl_of_clause cl_id (new_v, v');
            DynArray.push_back (LitArray.get st.clauses_of_wl new_v) cl_id |> ignore;
            true
          | (l, _, _) ->
            let new_v = List.find ((<>) v') l in
            DynArray.set st.wl_of_clause cl_id (new_v, v');
            DynArray.push_back (LitArray.get st.clauses_of_wl new_v) cl_id |> ignore;
            true
        )
      in

      let cls_of_v = LitArray.get st.clauses_of_wl v in

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
        Debug.f 1 (fun _ -> check_watched st);
        if not (Queue.is_empty st.queue) then propagate st else true
      | exception Exit ->
        Debug.f 1 (fun _ -> check_watched st);
        false
  end

(* The decision function.
   Picks a new unassigned literal to be propagated,
   according to the activities
*)
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

(* The Conflict analysis / clause learning function.
   Called everytime there is a conflict
*)
let conflict_analysis (st: state): int * int (* id of the new clause learnt,
                                                nb of backtracks *) =
  let set_of_cl cl = Array.to_list cl |> IntSet.of_list in
  let cl_of_set s = IntSet.elements s |> Array.of_list in

  let false_clause = DynArray.get st.clauses st.conflicting_clause in
  Debug.f 2 (fun _ ->
    Printf.printf "False clause (%d):" st.conflicting_clause;
    Array.iter (Printf.printf " %d") false_clause;
    print_endline "";

    print_propagation_log st
  );

  let temp_clause = ref (set_of_cl false_clause) in

  let temp_is_unit () = IntSet.min_elt !temp_clause = IntSet.max_elt !temp_clause in

  let temp_is_uip () =
    Debug.f 2 (fun _ ->
      Printf.printf "temp_is_uip: ";
      Printf.printf "temp clause:" ;
      IntSet.iter (Printf.printf " %d") !temp_clause;
      print_endline ""
    );

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
      Debug.f 1 (fun () -> assert (!n >= 1));
      if !n = 1 then !uip else 0
    end
  in

  let find_resolving_lit_and_cl () =
    let resolution = ref (0, -1) in
    let i = ref (DynArray.length st.propagation_log - 1) in
    (* We do not want to look at the decided literal, only the deduced ones *)
    let lit = ref 0 and cause_cl = ref (-1) in
    while !resolution = (0, -1) &&
          (let (l, c) = DynArray.get st.propagation_log !i in
           lit := l; cause_cl := c;
           c <> -1)
    do
      if IntSet.mem (- !lit) !temp_clause then
        resolution := (!lit, !cause_cl);
      decr i
    done;
    !resolution
  in

  let resolve cl1 cl2 lit =
    IntSet.remove lit (IntSet.remove (-lit) (IntSet.union cl1 cl2))
  in

  let rec loop () =
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
        loop ()
  in
  let uip = loop () in

  (* Bump activity of the literals in the conflicting clause *)
  IntSet.iter (incr_lit_activity st) !temp_clause;

  let bt_steps =
    if temp_is_unit () then
      1 (* The only literal of the clause can only be the uip,
           which has been decided at the current decision level *)
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

  (* Learn !temp_clause *)
  let new_id = add_new_clause st (cl_of_set !temp_clause) in
  Debug.f 2 (fun _ ->
    Printf.printf "Learning clause %d:" new_id;
    IntSet.iter (Printf.printf " %d") !temp_clause;
    print_endline ""
  );

  (new_id, bt_steps)


let cdcl (st: state): outcome =
  match propagate st with
  | false (* conflict *) -> UnSat
  | true ->
    while not st.is_unsat && not (total_assign st) do
      if Queue.is_empty st.queue then (
        let x = pick_branching_variable st in
        Debug.p 2 "DECIDING %d = true\n%!" x;

        Bt.push_state st.assign;
        st.decision_level <- st.decision_level + 1;
        Queue.add (x, -1) st.queue;
      );

      begin match propagate st with
        | true -> Debug.p 2 "Propagation ok\n%!"; ()
        | false ->
          Debug.p 2 "Conflict\n%!";
          Queue.clear st.queue;

          st.conflicts_counter <- st.conflicts_counter + 1;
          let new_clause_id, bt_steps = conflict_analysis st in
          Debug.p 2 "Backtracking of %d steps\n%!" bt_steps;

          if bt_steps > st.decision_level then
            st.is_unsat <- true
          else (
            (* Decay activities everytime there is a conflict *)
            decay_lit_activity st;

            (* Backtracking *)
            st.conflicting_clause <- (-1);

            let new_dl = DynArray.length st.propagation_bt - bt_steps in
            let i = DynArray.get st.propagation_bt new_dl in
            DynArray.shrink st.propagation_bt new_dl;
            DynArray.shrink st.propagation_log i;
            st.decision_level <- st.decision_level - bt_steps;
            Bt.pop_n_state st.assign bt_steps;

            Debug.f 2 (fun _ -> print_propagation_log st);

            (* We compute the watched literals for the new clause
               *after* backtracking, since this choice depends on the
               current assignation *)
            watch_new_clause st new_clause_id
          )
      end;

      (* Do we restart? *)
      if st.conflicts_counter >= st.restart_limit then (
        Debug.p 2 "Restarting\n%!";

        st.conflicts_counter <- 0;
        st.restart_limit <- (float_of_int st.restart_limit) *. st.restart_mult
                            |> int_of_float;
        (* learnt clauses are now considered part of the initial instance.
           This info is not used at the moment though (no clause activity/decay) *)
        st.learnt_clauses_begin <- DynArray.length st.clauses;
        reset_state st;
        for cl_id = 0 to DynArray.length st.clauses - 1 do
          watch_new_clause st cl_id
        done
      );
    done;

    if st.is_unsat then UnSat
    else
      let a = Array.make st.nvars false in
      for i = 1 to st.nvars do
        a.(i-1) <- (Bt.get st.assign i > 0)
      done;
      Sat a
