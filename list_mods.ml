let () =
  let _ = match Slang_to_behavioral.convert_files ~top:"cva6" 
                  ["test/cva6_ram/cva6_flat.sv"] with
    | Some p ->
        Printf.printf "=== SLANG (%d) ===\n" (List.length p.modules);
        List.iter (fun (m : Behavioral_ir.bmodule) ->
          Printf.printf "  %s\n" m.name) p.modules
    | None -> () in
  let v = Verible_to_behavioral.convert_files ~top:"cva6"
            ["test/cva6_ram/cva6_flat.sv"] in
  Printf.printf "=== VERIBLE (%d) ===\n" (List.length v.modules);
  List.iter (fun (m : Behavioral_ir.bmodule) ->
    Printf.printf "  %s\n" m.name) v.modules
