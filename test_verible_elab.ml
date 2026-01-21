(* Test Verible parser with elaboration *)

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: %s <verilog_file>\n" Sys.argv.(0);
    exit 1
  end;

  let filename = Sys.argv.(1) in
  Printf.printf "=== Testing Verible Parser + Elaboration ===\n";
  Printf.printf "File: %s\n\n" filename;

  (* Parse with Verible *)
  Printf.printf "Step 1: Parsing with Verible...\n";
  match Sv_verible_to_ir.parse_verible_file filename with
  | None ->
      Printf.eprintf "✗ Failed to parse file\n";
      exit 1
  | Some ast ->
      Printf.printf "✓ Parsed successfully\n\n";

      (* Elaborate *)
      Printf.printf "Step 2: Elaborating...\n";
      let elab_ctx = Sv_elaborate.elaborate ast in
      Printf.printf "✓ Elaboration complete\n\n";

      (* Print elaboration results *)
      Printf.printf "Step 3: Elaboration Results\n";
      Sv_elaborate.print_context elab_ctx;
      Printf.printf "\n";

      (* Convert to IR (stub) *)
      Printf.printf "Step 4: Converting to IR...\n";
      let module_name = Filename.chop_extension (Filename.basename filename) in
      let _ir = Sv_verible_to_ir.verible_to_ir ast module_name in
      Printf.printf "✓ IR created (stub)\n"
