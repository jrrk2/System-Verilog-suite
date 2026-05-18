(* dump BIR processes for slib_counter from both frontends *)
let dump label (p : Behavioral_ir.bprogram) =
  Printf.printf "\n──── %s ────\n" label;
  List.iter (fun (m : Behavioral_ir.bmodule) ->
    Printf.printf "module %s: %d signals, %d processes\n"
      m.name (List.length m.signals) (List.length m.processes);
    Printf.printf "  signals:\n";
    List.iter (fun (s : Behavioral_ir.bsignal) ->
      let w = match s.stype with
        | Behavioral_ir.BInt { width; _ } -> Printf.sprintf "bv%d" width
        | BArray { element; size } ->
            let ew = match element with
              | BInt { width; _ } -> width
              | _ -> 0 in
            Printf.sprintf "arr[%d]bv%d" size ew
        | BBool -> "bool"
        | _ -> "?" in
      let dir = match s.direction with
        | `Input -> "in" | `Output -> "out" | `Internal -> "wire" in
      Printf.printf "    %-6s %-12s %s\n" dir s.name w
    ) m.signals;
    Printf.printf "  funcs: %d\n" (List.length m.funcs);
    List.iter (fun (f : Behavioral_ir.bfunc) ->
      Printf.printf "    %s (is_task=%b, %d params, %d locals, %d body stmts)\n"
        f.fname f.is_task (List.length f.params)
        (List.length f.locals) (List.length f.body);
      List.iter (fun st ->
        Printf.printf "%s\n" (Behavioral_ir.string_of_bstmt 6 st)
      ) f.body
    ) m.funcs;
    Printf.printf "  mems: %d\n" (List.length m.mems);
    List.iter (fun (m : Behavioral_ir.bmem) ->
      Printf.printf "    %s: %dx%d (%s)\n"
        m.mname m.depth m.data_width
        (match m.kind with BRam -> "RAM" | BRom -> "ROM")
    ) m.mems;
    List.iteri (fun i (proc : Behavioral_ir.bprocess) ->
      let kind, body = match proc with
        | BSequential { body; _ } -> "Sequential", body
        | BCombinational { body; _ } -> "Combinational", body in
      Printf.printf "  process %d: %s\n" i kind;
      Printf.printf "%s\n"
        (String.concat "\n" (List.map (Behavioral_ir.string_of_bstmt 4) body))
    ) m.processes
  ) p.modules

let () =
  let sv = if Array.length Sys.argv >= 3 then Sys.argv.(2)
           else "/home/jonathan/System-Verilog-decompiler/sysver_tests/slib_counter.sv" in
  let top = if Array.length Sys.argv >= 2 then Sys.argv.(1)
            else "slib_counter" in
  Printf.printf "=== Verible ===\n";
  let p_v = Sv_lua.load_frontend ~frontend:"verible" ~top ~files:[sv] in
  dump "verible" p_v;
  Printf.printf "\n\n=== Slang ===\n";
  let p_s = Sv_lua.load_frontend ~frontend:"slang"  ~top ~files:[sv] in
  dump "slang"  p_s
