(* Plumb the Surelog UHDM frontend into the existing Hardcaml stage.
 *
 * For each module in a UHDM dump, run
 *   Surelog dump → token tree
 *               → Behavioral_ir.bmodule (via Surelog_to_behavioral)
 *               → Hardcaml circuit (via Behavioral_to_hardcaml)
 *               → Verilog text
 * and report what worked / what threw. The point is to find out what
 * the hardcaml layer is unhappy about given our current converter
 * coverage (no processes, no widths beyond 1 yet) so we know which
 * surelog→BIR gaps to close next. *)

open Behavioral_ir

let dump_path =
  if Array.length Sys.argv > 1 then Sys.argv.(1)
  else "/home/jonathan/System-Verilog-suite/test/surelog/apb_uart.dump"

let try_module (m : bmodule) =
  Printf.printf "\n─── %s ───\n" m.name;
  Printf.printf "  signals: %d (in=%d out=%d int=%d)\n"
    (List.length m.signals)
    (List.length (List.filter (fun (s : bsignal) -> s.direction = `Input) m.signals))
    (List.length (List.filter (fun (s : bsignal) -> s.direction = `Output) m.signals))
    (List.length (List.filter (fun (s : bsignal) -> s.direction = `Internal) m.signals));
  Printf.printf "  processes: %d  instances: %d  mems: %d\n"
    (List.length m.processes)
    (List.length m.instances)
    (List.length m.mems);
  let single_prog =
    { modules = [m]; library_cells = [] }
  in
  match
    try Ok (Behavioral_to_hardcaml.convert_to_hardcaml single_prog)
    with e -> Error (Printexc.to_string e)
  with
  | Ok None ->
      Printf.printf "  hardcaml: convert_to_hardcaml returned None\n"
  | Ok (Some (m', ins, outs)) ->
      Printf.printf "  hardcaml: ✓ port-shell built (name=%s, %d inputs, %d outputs)\n"
        m'.name (List.length ins) (List.length outs);
      (* Now drive the real circuit builder. `module_to_create` walks
       * processes; with our processless BIR this should at minimum
       * not crash, and outputs come back as the wire defaults. *)
      (try
        let in_signals =
          List.map (fun (n, w) -> (n, Hardcaml.Signal.input n w)) ins
        in
        let outputs_built = Behavioral_to_hardcaml.module_to_create m' in_signals in
        Printf.printf "  hardcaml: ✓ module_to_create returned %d outputs\n"
          (List.length outputs_built);
        let outputs_named =
          List.map (fun (n, s) -> Hardcaml.Signal.output n s) outputs_built
        in
        (try
          let _circuit =
            Hardcaml.Circuit.create_exn ~name:m'.name outputs_named
          in
          Printf.printf "  hardcaml: ✓ Circuit.create_exn succeeded\n"
        with e ->
          Printf.printf "  hardcaml: ✗ Circuit.create_exn: %s\n"
            (Printexc.to_string e |> String.split_on_char '\n' |> List.hd))
      with e ->
        Printf.printf "  hardcaml: ✗ module_to_create: %s\n"
          (Printexc.to_string e |> String.split_on_char '\n' |> List.hd))
  | Error msg ->
      let first_line = msg |> String.split_on_char '\n' |> List.hd in
      Printf.printf "  hardcaml: ✗ %s\n" first_line

let () =
  Printf.printf "═══════════════════════════════════════════════════════\n";
  Printf.printf "  Surelog → BIR → Hardcaml smoke test\n";
  Printf.printf "  dump: %s\n" dump_path;
  Printf.printf "═══════════════════════════════════════════════════════\n";
  let prog = Surelog_to_behavioral.convert_dump_file dump_path in
  Printf.printf "Loaded %d modules from dump.\n" (List.length prog.modules);
  List.iter try_module prog.modules;
  Printf.printf "\n═══════════════════════════════════════════════════════\n";
  let total = List.length prog.modules in
  Printf.printf "  %d modules processed.\n" total
