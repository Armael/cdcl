open Prelude

let _ =
  Arg.parse ["-v", Arg.Unit (fun () -> Debug.verbosity := 1), "verbose"] (fun _ -> ()) "";
  let input_buf = dump_chan stdin in
  close_in stdin;
  let cnf = Cnf.parse input_buf in

  print_endline "parsed";
  
  let st = Sat.init_state cnf in
  match Sat.cdcl st with
  | Sat.UnSat -> print_endline "UnSat"
  | Sat.Sat a ->
    if not (Verif.verif cnf a) then
      print_endline ">> BUG <<";
    
    print_string "S ";
    Array.iteri (fun i b ->
      print_int (if b then i+1 else -i-1);
      print_string " "
    ) a;
    print_endline ""
