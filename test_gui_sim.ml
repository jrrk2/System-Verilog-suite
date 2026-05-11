(* Headless smoke test for [Gui_sim] — load a small SV file via Verible,
   simulate the first module for a few cycles, dump the trace.  Lets us
   sanity-check the simulator without spinning up the GTK window.       *)

let () =
  if Array.length Sys.argv < 2 then begin
    prerr_endline "usage: test_gui_sim <file.sv> [cycles]"; exit 1
  end;
  let path = Sys.argv.(1) in
  let cycles =
    if Array.length Sys.argv >= 3 then int_of_string Sys.argv.(2) else 16 in
  let p = Verible_to_behavioral.convert_files_all [path] in
  Printf.printf "Loaded %d modules from %s:\n"
    (List.length p.modules) path;
  List.iter (fun (m : Behavioral_ir.bmodule) ->
    Printf.printf "  %-30s  signals=%d  processes=%d  mems=%d\n"
      m.name (List.length m.signals) (List.length m.processes)
      (List.length m.mems)) p.modules;
  let m = List.find
    (fun (m : Behavioral_ir.bmodule) -> m.processes <> []) p.modules in
  Printf.printf "\nSimulating %s for %d cycles…\n%!" m.name cycles;
  let sr = Gui_sim.run ~n_cycles:cycles m in
  let print_trace tag traces =
    List.iter (fun (ts : Gui_sim.trace_signal) ->
      Printf.printf "  %s %-20s [%2d]:" tag ts.ts_name ts.ts_width;
      for c = 0 to sr.sr_cycles - 1 do
        Printf.printf " %s" (Gui_sim.format_value ts c)
      done;
      print_newline ()
    ) traces in
  Printf.printf "Inputs (%d):\n"  (List.length sr.sr_inputs);
  print_trace "in " sr.sr_inputs;
  Printf.printf "Outputs (%d):\n" (List.length sr.sr_outputs);
  print_trace "out" sr.sr_outputs
