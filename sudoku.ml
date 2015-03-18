open Prelude

let p i j d =
  (* 0 <= i, j, d <= 8 *)
  i + 9 * j + 81 * d + 1

let sudoku_encoding grid =
  let clauses = ref [] in
  let nclauses = ref 0 in
  let addc c = clauses := c::!clauses; incr nclauses in

  for i = 0 to 8 do
    for j = 0 to 8 do
      match grid.(i).(j) with
      | 0 ->
        seq 0 8 |> List.map (p i j) |> addc;
        for d = 0 to 8 do
          for d' = d+1 to 8 do
            addc [- (p i j d); - (p i j d')]
          done
        done
      | x ->
        addc [p i j (x-1)];
    done
  done;

  let valid xi (* x1 x2 x3 x4 x5 x6 x7 x8 x9 *) =
    for i = 0 to 8 do
      for j = i + 1 to 8 do
        for d = 0 to 8 do
          addc [- (p (fst xi.(i)) (snd xi.(i)) d); - (p (fst xi.(j)) (snd xi.(j)) d)];
        done
      done
    done
  in

  for i = 0 to 8 do
    aseq 0 8 |> Array.map (fun x -> (i, x)) |> valid;
  done;
  for j = 0 to 8 do
    aseq 0 8 |> Array.map (fun x -> (x, j)) |> valid;
  done;

  List.iter (fun i ->
    List.iter (fun j ->
      valid [|
        (i, j);
        (i, j+1);
        (i, j+2);
        (i+1, j);
        (i+1, j+1);
        (i+1, j+2);
        (i+2, j);
        (i+2, j+1);
        (i+2, j+2);
      |]
    ) [0;3;6]
  ) [0;3;6];

  (729, !nclauses, !clauses)

let sudoku_decode get_value =
  for i = 0 to 8 do
    if i mod 3 = 0 then print_endline "-------------";
    for j = 0 to 8 do
      if j mod 3 = 0 then print_char '|';
      try for d = 0 to 8 do
          if get_value (p i j d) = Some true then (
            print_int (d+1);
            raise Exit
          )
        done; print_int 0 with Exit -> ();
    done;
    print_char '|';
    print_newline ();
  done

let _ =
  let input_buf =  dump_chan stdin in
  close_in stdin;

  let m = Array.make_matrix 9 9 0 in
  for i = 0 to 8 do
    for j = 0 to 8 do
      m.(i).(j) <- (int_of_char @@ Buffer.nth input_buf (i * 9 + j)) - (int_of_char '0')
    done
  done;
  let cnf = sudoku_encoding m in

  print_endline "parsed";

  let st = Sat.init_state cnf in
  match Sat.cdcl st with
  | Sat.UnSat -> print_endline "UnSat"
  | Sat.Sat a ->
    if not (Verif.verif cnf a) then
      print_endline ">> BUG <<";
    sudoku_decode (fun i -> Some a.(i))
