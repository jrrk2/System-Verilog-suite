(* Side-by-side BIR dump from verible, slang, verilator, sv-parser. *)
let dump label (p : Behavioral_ir.bprogram) =
  Printf.printf "\n──── %s ────\n" label;
  List.iter (fun (m : Behavioral_ir.bmodule) ->
    Printf.printf "module %s: %d signals, %d processes, %d funcs\n"
      m.name (List.length m.signals) (List.length m.processes)
      (List.length m.funcs);
    List.iter (fun (f : Behavioral_ir.bfunc) ->
      let pp_dir = function
        | `Input -> "in" | `Output -> "out" | `Inout -> "io" in
      let pstr = String.concat ", " (List.map (fun (n, t, d) ->
        let w = match t with
          | Behavioral_ir.BInt { width; _ } -> width
          | _ -> 0 in
        Printf.sprintf "%s %s:bv%d" (pp_dir d) n w) f.params) in
      Printf.printf "  func %s task=%b params=(%s)\n%s\n"
        f.fname f.is_task pstr
        (String.concat "\n"
           (List.map (Behavioral_ir.string_of_bstmt 4) f.body))
    ) m.funcs;
    List.iter (fun (s : Behavioral_ir.bsignal) ->
      let w = match s.stype with
        | Behavioral_ir.BInt { width; _ } -> Printf.sprintf "bv%d" width
        | BArray { element; size } ->
            let ew = match element with
              | BInt { width; _ } -> width | _ -> 0 in
            Printf.sprintf "arr[%d]bv%d" size ew
        | BBool -> "bool" | _ -> "?" in
      let dir = match s.direction with
        | `Input -> "in" | `Output -> "out" | `Internal -> "wire" in
      Printf.printf "    %-6s %-25s %s\n" dir s.name w
    ) m.signals;
    List.iteri (fun i (proc : Behavioral_ir.bprocess) ->
      let kind, body = match proc with
        | BSequential _ -> "Seq", (match proc with BSequential {body;_} -> body | _ -> [])
        | BCombinational _ -> "Comb", (match proc with BCombinational {body;_} -> body | _ -> []) in
      Printf.printf "  proc %d: %s\n%s\n" i kind
        (String.concat "\n" (List.map (Behavioral_ir.string_of_bstmt 4) body))
    ) m.processes
  ) p.modules

let () =
  if Array.length Sys.argv < 4 then begin
    Printf.eprintf "usage: %s <top> <sv> <json>\n" Sys.argv.(0); exit 1
  end;
  let top = Sys.argv.(1) and sv = Sys.argv.(2) and js = Sys.argv.(3) in
  let try_load fe files =
    try Some (Sv_lua.load_frontend ~frontend:fe ~top ~files)
    with e ->
      Printf.eprintf "[%s] %s\n" fe (Printexc.to_string e); None in
  (match try_load "verible"   [sv] with Some p -> dump "verible"   p | None -> ());
  (match try_load "slang"     [sv] with Some p -> dump "slang"     p | None -> ());
  (match try_load "verilator" [js] with Some p -> dump "verilator" p | None -> ());
  (match try_load "sv-parser" [sv] with Some p -> dump "sv-parser" p | None -> ())
