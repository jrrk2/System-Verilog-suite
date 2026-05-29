(* Minimal driver: convert a VHDL file to BIR and dump the result.
   Surfaces which constructs the existing converter handles and
   which fall through to the "Unhandled pattern" warning. *)

open Behavioral_ir

let dump_signal (s : bsignal) =
  let dir = match s.direction with
    | `Input -> "in" | `Output -> "out" | `Internal -> "int" in
  Printf.printf "  signal %-3s %-20s : %s\n"
    dir s.name
    (match s.stype with
     | BInt { width; signed } ->
         Printf.sprintf "%s<%d>"
           (match signed with Signed -> "int" | Unsigned -> "uint") width
     | BBool -> "bool"
     | BArray { element = BInt { width; _ }; size } ->
         Printf.sprintf "[0:%d] of %d-bit" (size - 1) width
     | _ -> "<other>")

let rec dump_stmt indent s =
  let pad = String.make indent ' ' in
  match s with
  | BAssign { lhs; rhs } ->
      Printf.printf "%s%s := %s;\n" pad lhs (string_of_bexpr rhs)
  | BIf { condition; then_stmts; else_stmts } ->
      Printf.printf "%sif (%s) then\n" pad (string_of_bexpr condition);
      List.iter (dump_stmt (indent + 2)) then_stmts;
      if else_stmts <> [] then begin
        Printf.printf "%selse\n" pad;
        List.iter (dump_stmt (indent + 2)) else_stmts
      end;
      Printf.printf "%send if\n" pad
  | BCase { selector; cases; default } ->
      Printf.printf "%scase (%s)\n" pad (string_of_bexpr selector);
      List.iter (fun (e, ss) ->
        Printf.printf "%s  when %s =>\n" pad (string_of_bexpr e);
        List.iter (dump_stmt (indent + 4)) ss
      ) cases;
      if default <> [] then begin
        Printf.printf "%s  default =>\n" pad;
        List.iter (dump_stmt (indent + 4)) default
      end;
      Printf.printf "%sendcase\n" pad
  | BBlock ss ->
      Printf.printf "%sbegin\n" pad;
      List.iter (dump_stmt (indent + 2)) ss;
      Printf.printf "%send\n" pad
  | BCallStmt { func; args } ->
      Printf.printf "%s%s(%s);\n" pad func
        (String.concat ", " (List.map string_of_bexpr args))
  | BFor _ -> Printf.printf "%s<for-loop>\n" pad
  | BWhile _ -> Printf.printf "%s<while-loop>\n" pad
  | BReturn _ -> Printf.printf "%sreturn;\n" pad

let dump_process (p : bprocess) =
  match p with
  | BSequential { name; clock; clock_edge; reset; reset_async; body; _ } ->
      Printf.printf "  process %s [seq, clock=%s%s, reset=%s%s]:\n"
        name clock
        (match clock_edge with `Pos -> "↑" | `Neg -> "↓")
        (match reset with Some r -> r | None -> "—")
        (if reset_async then " (async)" else " (sync)");
      List.iter (dump_stmt 4) body
  | BCombinational { name; sensitivity; body; _ } ->
      Printf.printf "  process %s [comb, sens=%s]:\n"
        name
        (String.concat "," (List.map (function
          | BPosEdge s | BNegEdge s | BLevel s -> s
          | BAny -> "*") sensitivity));
      List.iter (dump_stmt 4) body

let dump_module (m : bmodule) =
  Printf.printf "\nmodule %s:\n" m.name;
  Printf.printf " signals (%d):\n" (List.length m.signals);
  List.iter dump_signal m.signals;
  Printf.printf " processes (%d):\n" (List.length m.processes);
  List.iter dump_process m.processes;
  if m.instances <> [] then begin
    Printf.printf " instances (%d):\n" (List.length m.instances);
    List.iter (fun (i : binstance) ->
      Printf.printf "  %s : %s  (ports: %d)\n"
        i.inst_name i.module_name (List.length i.port_connections)
    ) m.instances
  end

(* Walk the instance graph rooted at [top].  Mirrors Vivado's
   `get_cells -hierarchical` view: emit one line per transitive
   instance with the dotted path. *)
let dump_hierarchy ~top (modules : bmodule list) =
  let by_name = Hashtbl.create 32 in
  List.iter (fun (m : bmodule) -> Hashtbl.replace by_name m.name m) modules;
  Printf.printf "\n=== HIERARCHY rooted at %s ===\n" top;
  let rec walk path m =
    List.iter (fun (i : binstance) ->
      let p = if path = "" then i.inst_name
              else path ^ "/" ^ i.inst_name in
      Printf.printf "  %-50s %s\n" p i.module_name;
      (match Hashtbl.find_opt by_name i.module_name with
       | Some child -> walk p child
       | None -> ())
    ) m.instances
  in
  match Hashtbl.find_opt by_name top with
  | Some m -> walk "" m
  | None -> Printf.printf "  ✗ top entity %s not found\n" top

let () =
  if Array.length Sys.argv < 2 then begin
    prerr_endline "usage: test_vhdl_bir_dump [--multi <top>] <file.vhd> [more...]";
    exit 1
  end;
  let argv = Array.to_list Sys.argv in
  match argv with
  | _ :: "--multi" :: top :: files when files <> [] ->
      (* Parse each file, collect bmodules into one program. *)
      let modules = List.concat_map (fun f ->
        match Vhdl_to_behavioral.convert_vhdl_file_to_behavioral f with
        | None ->
            Printf.printf "  ✗ %s convert failed\n" (Filename.basename f); []
        | Some prog -> prog.modules
      ) files in
      Printf.printf "\n===== multi-file mode (%d modules) =====\n"
        (List.length modules);
      List.iter (fun (m : bmodule) ->
        Printf.printf "  module %-32s signals=%d procs=%d insts=%d\n"
          m.name (List.length m.signals)
          (List.length m.processes) (List.length m.instances)
      ) modules;
      dump_hierarchy ~top modules
  | _ :: files ->
      List.iter (fun f ->
        Printf.printf "\n===== %s =====\n" (Filename.basename f);
        match Vhdl_to_behavioral.convert_vhdl_file_to_behavioral f with
        | None -> Printf.printf "  ✗ CONVERT FAILED\n"
        | Some prog -> List.iter dump_module prog.modules
      ) files
  | _ -> ()
