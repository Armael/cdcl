open Prelude

let _ =
  Arg.parse [
    "-v", Arg.Unit (fun () -> Debug.set_verbosity 1),
      "Execute runtime checks";
    "-vv", Arg.Unit (fun () -> Debug.set_verbosity 2),
      "Execute runtime checks and print debug messages";
  ] (fun _ -> ())
    "Usage: ./main.native < input.cnf";
  
  let input_buf = dump_chan stdin in
  close_in stdin;
  let cnf = Cnf.parse input_buf in

  let st = Sat.init_state cnf in
  match Sat.cdcl st with
  | Sat.UnSat -> print_endline "UNSATISFIABLE"
  | Sat.Sat a ->
    Debug.f 1 (fun _ -> assert (Verif.verif cnf a));
    
    print_endline "SATISFIABLE";
    Array.iteri (fun i b ->
      print_int (if b then i+1 else -i-1);
      print_string " "
    ) a;
    print_endline ""
