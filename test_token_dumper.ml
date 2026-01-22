(* Test program for the token dumper *)
(* Uses elaboration to extract and show statement ordering *)

let test_file filename =
  Printf.printf "\n";
  Printf.printf "╔════════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║  SystemVerilog Statement Ordering Analyzer                    ║\n";
  Printf.printf "║  Shows how Verible orders statements in parse tree            ║\n";
  Printf.printf "╚════════════════════════════════════════════════════════════════╝\n";
  Printf.printf "\n";
  Printf.printf "File: %s\n\n" filename;

  (* Show original source *)
  Printf.printf "════════════════════════════════════════════════════════════\n";
  Printf.printf "ORIGINAL SOURCE (program order)\n";
  Printf.printf "════════════════════════════════════════════════════════════\n\n";

  let ic = open_in filename in
  let line_num = ref 0 in
  (try
    while true do
      let line = input_line ic in
      line_num := !line_num + 1;
      Printf.printf "%3d: %s\n" !line_num line
    done
  with End_of_file -> close_in ic);

  Printf.printf "\n";

  (* Parse and elaborate *)
  Printf.printf "════════════════════════════════════════════════════════════\n";
  Printf.printf "PARSED STATEMENT ORDER (from Verible parse tree)\n";
  Printf.printf "════════════════════════════════════════════════════════════\n\n";

  match Sv_verible_to_ir.parse_verible_file filename with
  | None ->
      Printf.printf "ERROR: Failed to parse file\n"
  | Some verible_ast ->
      let elab_ctx = Sv_elaborate.elaborate verible_ast in
      let module_name = match elab_ctx.Sv_elaborate.module_name with
        | Some name -> name
        | None -> failwith "No module name found"
      in
      let module_data = match Sv_elaborate.get_module_data elab_ctx module_name with
        | Some data -> data
        | None -> failwith "No module data found"
      in
      Printf.printf "Module: %s\n\n" module_name;

          (* Show each always block *)
          List.iter (fun always_blk ->
            match always_blk.Sv_elaborate.always_type with
            | Sv_elaborate.AlwaysFF { clock; edge; async_reset } ->
                Printf.printf "always @(";
                (match edge with `Posedge -> Printf.printf "posedge" | `Negedge -> Printf.printf "negedge");
                Printf.printf " %s" clock;
                (match async_reset with
                 | Some reset_info ->
                     Printf.printf " or ";
                     (match reset_info.Sv_elaborate.reset_edge with `Posedge -> Printf.printf "posedge" | `Negedge -> Printf.printf "negedge");
                     Printf.printf " %s" reset_info.Sv_elaborate.reset_signal
                 | None -> ());
                Printf.printf ")\n";

                Printf.printf "  Statements extracted in this order:\n";
                List.iteri (fun i assign ->
                  Printf.printf "    [%d] %s <= <expr>  (condition: %s)\n"
                    i
                    assign.Sv_elaborate.assign_lhs
                    (match assign.Sv_elaborate.assign_condition with
                     | None -> "NONE - unconditional"
                     | Some _ -> "SOME - conditional")
                ) always_blk.Sv_elaborate.always_stmts;
                Printf.printf "\n"
            | _ -> ()
          ) module_data.Sv_elaborate.mod_always_blocks;

          Printf.printf "════════════════════════════════════════════════════════════\n";
          Printf.printf "ANALYSIS\n";
          Printf.printf "════════════════════════════════════════════════════════════\n\n";
          Printf.printf "The [N] numbers show the order Verible extracted statements.\n";
          Printf.printf "Compare with the line numbers in ORIGINAL SOURCE above.\n\n";
          Printf.printf "Key observations:\n";
          Printf.printf "- If unconditional assignments come AFTER conditionals,\n";
          Printf.printf "  that's a reversal bug\n";
          Printf.printf "- Chronological order matters: later assignments override earlier\n";
          Printf.printf "- The List.rev fix removed one reversal, but order may still\n";
          Printf.printf "  be wrong for some patterns\n\n"

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.printf "Usage: %s <systemverilog_file>\n" Sys.argv.(0);
    Printf.printf "\nExample test files:\n";
    Printf.printf "  /tmp/test_unconditional_then_conditional.sv\n";
    Printf.printf "  /tmp/slib_clock_div.sv\n";
    Printf.printf "  /tmp/test_sequential_ifs.sv\n";
    exit 1
  end;

  let filename = Sys.argv.(1) in
  if not (Sys.file_exists filename) then begin
    Printf.eprintf "Error: File not found: %s\n" filename;
    exit 1
  end;

  test_file filename
