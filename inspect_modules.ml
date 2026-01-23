(* Simple script to inspect ALL modules in Verilator JSON conversion *)

open Behavioral_ir

let inspect_all_modules json_file =
  Printf.printf "Inspecting: %s\n\n" json_file;

  match Verilator_to_behavioral.convert_verilator_json_to_behavioral json_file with
  | None -> Printf.printf "Conversion failed\n"
  | Some bprog ->
      Printf.printf "Total modules: %d\n\n" (List.length bprog.modules);

      (* Print summary for each module *)
      List.iteri (fun i (m : bmodule) ->
        Printf.printf "[%2d] %-30s  signals:%3d  processes:%2d  instances:%2d\n"
          (i+1)
          m.name
          (List.length m.signals)
          (List.length m.processes)
          (List.length m.instances)
      ) bprog.modules;

      Printf.printf "\n";

      (* Find std_icache *)
      let std_icache_opt = List.find_opt (fun m -> m.name = "std_icache") bprog.modules in
      match std_icache_opt with
      | Some icache ->
          Printf.printf "std_icache details:\n";
          Printf.printf "  Signals: %d\n" (List.length icache.signals);
          Printf.printf "  Processes: %d\n" (List.length icache.processes);

          if List.length icache.signals > 0 then begin
            Printf.printf "\n  Signals:\n";
            List.iter (fun s ->
              Printf.printf "    - %s\n" s.name
            ) (List.filteri (fun i _ -> i < 20) icache.signals)
          end;

          if List.length icache.processes > 0 then begin
            Printf.printf "\n  Processes:\n";
            List.iter (fun proc ->
              match proc with
              | BSequential { name; clock; clock_edge; body; _ } ->
                  Printf.printf "    - Sequential: %s (clock=%s, %s, %d stmts)\n"
                    name clock
                    (match clock_edge with `Pos -> "posedge" | `Neg -> "negedge")
                    (List.length body)
              | BCombinational { name; body; _ } ->
                  Printf.printf "    - Combinational: %s (%d stmts)\n"
                    name (List.length body)
            ) icache.processes
          end

      | None ->
          Printf.printf "std_icache NOT FOUND in converted modules\n"

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: %s <json_file>\n" Sys.argv.(0);
    exit 1
  end;
  inspect_all_modules Sys.argv.(1)
