let dump_mems label (m : Behavioral_ir.bmodule) =
  Printf.printf "\n--- %s memories (%d) ---\n" label (List.length m.mems);
  List.iter (fun (mm : Behavioral_ir.bmem) ->
    let k = match mm.kind with Behavioral_ir.BRam -> "RAM" | BRom -> "ROM" in
    Printf.printf "  %s %-10s %dx%d (addr_w=%d) [%dW/%dR %s] -> %s\n"
      k mm.mname mm.depth mm.data_width mm.addr_width
      mm.n_write_ports mm.n_read_ports
      (if mm.read_is_sync then "sync" else "async")
      (Behavioral_meminfer.kind_label mm)
  ) m.mems

let dump_module label (m : Behavioral_ir.bmodule) =
  Printf.printf "\n=== %s side BIR for %s ===\n" label m.name;
  Printf.printf "Signals (%d):\n" (List.length m.signals);
  List.iter (fun (s : Behavioral_ir.bsignal) ->
    let dir = match s.direction with `Input -> "in" | `Output -> "out" | `Internal -> "int" in
    let typ = match s.stype with
      | Behavioral_ir.BInt {width;_} -> Printf.sprintf "int<%d>" width
      | BArray {element=BInt{width;_}; size} -> Printf.sprintf "array[%d]<int<%d>>" size width
      | BArray {element=_; size} -> Printf.sprintf "array[%d]<?>" size
      | BBool -> "bool"
      | _ -> "?"
    in
    Printf.printf "  %s %-20s %s\n" dir s.name typ
  ) m.signals;
  let dump_e e = print_string (Behavioral_ir.string_of_bexpr e) in
  let rec dump_s indent = function
    | Behavioral_ir.BAssign { lhs; rhs } ->
        Printf.printf "%s%s = " indent lhs; dump_e rhs; Printf.printf "\n"
    | BIf { condition; then_stmts; else_stmts } ->
        Printf.printf "%sif (" indent; dump_e condition; Printf.printf ")\n";
        List.iter (dump_s (indent ^ "  ")) then_stmts;
        if else_stmts <> [] then begin
          Printf.printf "%selse\n" indent;
          List.iter (dump_s (indent ^ "  ")) else_stmts
        end
    | BBlock ss -> List.iter (dump_s indent) ss
    | BCallStmt { func; args } ->
        Printf.printf "%scall %s(" indent func;
        List.iteri (fun i a -> if i > 0 then Printf.printf ", "; dump_e a) args;
        Printf.printf ")\n"
    | _ -> Printf.printf "%s<other>\n" indent
  in
  Printf.printf "Processes (%d):\n" (List.length m.processes);
  List.iter (function
    | Behavioral_ir.BCombinational c ->
        Printf.printf "  comb %s:\n" c.name;
        List.iter (dump_s "    ") c.body
    | BSequential s ->
        Printf.printf "  seq %s (clk=%s):\n" s.name s.clock;
        List.iter (dump_s "    ") s.body
  ) m.processes

let () =
  let (top, src) =
    if Array.length Sys.argv > 2 then (Sys.argv.(1), Sys.argv.(2))
    else ("picorv32_regs", "/home/jonathan/picorv32/picorv32.v")
  in
  let v = Verible_to_behavioral.convert_files
    ~top [ src ] in
  let v = Behavioral_meminfer.infer_program v in
  (match List.find_opt (fun m -> m.Behavioral_ir.name = top) v.modules with
   | Some m -> dump_module "Verible" m; dump_mems "Verible" m
   | None -> Printf.printf "[Verible] no module found\n");
  (* Verilator *)
  let mdir = "/tmp/dump_regs_vlt" in
  ignore (Sys.command (Printf.sprintf "rm -rf %s && mkdir -p %s" mdir mdir));
  ignore (Sys.command (Printf.sprintf
    "verilator --json-only -Wno-fatal -Wno-WIDTHTRUNC -Wno-WIDTHEXPAND \
     -Wno-UNOPTFLAT --top-module %s %s \
     --Mdir %s > /dev/null 2>&1" top src mdir));
  let json = Printf.sprintf "%s/V%s.tree.json" mdir top in
  match Verilator_to_behavioral.convert_verilator_json_to_behavioral json with
  | Some p ->
      let p = Behavioral_meminfer.infer_program p in
      (match List.find_opt (fun m -> m.Behavioral_ir.name = top) p.modules with
       | Some m -> dump_module "Verilator" m; dump_mems "Verilator" m
       | None -> Printf.printf "[Verilator] no module found\n")
  | None -> Printf.printf "[Verilator] failed\n"
