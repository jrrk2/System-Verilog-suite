(* Lua scripting layer for sv_decompiler.
 *
 * Modeled directly on hardcaml-lua's myluaclient.ml: a small sum type
 * of artifacts (Prog/Mod/Lib/Bool/Str), a hashtable keyed by string
 * handles, and one Lua-callable per existing subcommand. The Lua side
 * only ever sees opaque handle strings; OCaml values stay in the
 * hashtable.
 *
 * Lua API (all under the `svd` module):
 *
 *   h = svd.parse(frontend, top, {file1, file2, …})  -> prog handle
 *   h = svd.pick(prog_handle, top)                   -> module handle
 *   r = svd.miter(mod_a, mod_b)                      -> "EQUIVALENT" / "DIFFER"
 *   r = svd.gate_miter(top, beh, gate)               -> same, with default Liberty
 *   r = svd.gate_miter(top, beh, gate, lib)          -> same, explicit Liberty
 *   h = svd.liberty(file)                            -> Liberty handle
 *   h = svd.expand(prog_handle, lib_handle)          -> new prog handle
 *   s = svd.bir(handle)                              -> textual BIR / dump
 *   s = svd.name(handle)                             -> stored name
 *   s = svd.items()                                  -> tab-separated index *)

open Behavioral_ir

(* ──────────────────────────────────────────────────────────────────
 * Handle storage *)

type luaitm =
  | Prog of string * bprogram                 (* label, program *)
  | Mod  of string * bmodule * bprogram       (* mod name, bmodule, owning program (for hier flatten) *)
  | Lib  of string * Sv_liberty.library_info

let lhash : (string, luaitm) Hashtbl.t = Hashtbl.create 64

let nxtitm =
  let c = ref 0 in
  fun () -> incr c; "itm" ^ string_of_int !c

(* Insert and return a unique handle. If an identical artifact is
 * already stored, reuse its handle (cheap dedup mirrors hardcaml-lua). *)
let hadd x =
  let found = ref None in
  Hashtbl.iter (fun k v -> match !found, x, v with
    | Some _, _, _ -> ()
    | None, Prog (n, p), Prog (n', p') when n = n' && p == p' -> found := Some k
    | None, Mod  (n, m, _), Mod  (n', m', _) when n = n' && m == m' -> found := Some k
    | None, Lib  (n, l), Lib  (n', l') when n = n' && l == l' -> found := Some k
    | _ -> ()
  ) lhash;
  match !found with
  | Some k -> k
  | None ->
      let h = nxtitm () in
      Hashtbl.add lhash h x;
      h

let find_prog h =
  match Hashtbl.find_opt lhash h with
  | Some (Prog (n, p)) -> (n, p)
  | _ -> failwith ("handle " ^ h ^ " is not a program")

let find_mod h =
  match Hashtbl.find_opt lhash h with
  | Some (Mod (n, m, p)) -> (n, m, p)
  | _ -> failwith ("handle " ^ h ^ " is not a module")

let find_lib h =
  match Hashtbl.find_opt lhash h with
  | Some (Lib (n, l)) -> (n, l)
  | _ -> failwith ("handle " ^ h ^ " is not a library")

(* ──────────────────────────────────────────────────────────────────
 * Frontend / pipeline shims — duplicated from sv_decompiler.ml's
 * load_frontend so the Lua layer doesn't pull in the executable. The
 * shared library functions (Verible_to_behavioral, Slang_to_behavioral,
 * Rtlil_to_behavioral, etc.) do the real work. *)

let find_yosys () =
  let home = try Sys.getenv "HOME" with Not_found -> "/root" in
  List.find_opt (fun p ->
    if String.length p > 0 && p.[0] = '/' then Sys.file_exists p
    else Sys.command (Printf.sprintf "command -v %s > /dev/null" p) = 0
  ) [
    home ^ "/oss-cad-suite/bin/yosys";
    "/usr/local/bin/yosys";
    "/usr/bin/yosys";
    "yosys";
  ]

let run_yosys_to_rtlil ~top ~files ~out =
  let yosys = match find_yosys () with
    | Some y -> y
    | None -> failwith "yosys not found" in
  let script = Filename.temp_file "yosys_" ".ys" in
  let oc = open_out script in
  if Sys.getenv_opt "YOSYS_SLANG" <> None then begin
    Printf.fprintf oc "plugin -i slang\n";
    Printf.fprintf oc "read_slang --top %s %s\n" top
      (String.concat " " files);
    Printf.fprintf oc "hierarchy -top %s\nproc\nflatten\n" top
  end else begin
    Printf.fprintf oc "read_verilog -sv %s\n" (String.concat " " files);
    Printf.fprintf oc
      "hierarchy -top %s\nproc\nopt -fast\nflatten\nopt -fast\n" top
  end;
  Printf.fprintf oc "write_rtlil %s\n" out;
  close_out oc;
  let rc = Sys.command
             (Printf.sprintf "%s -q -s %s 2>&1"
                (Filename.quote yosys) (Filename.quote script)) in
  (try Sys.remove script with _ -> ());
  if rc <> 0 then failwith (Printf.sprintf "yosys exit %d" rc)

let load_frontend ~frontend ~top ~files : bprogram =
  match frontend with
  | "verible" ->
      Verible_to_behavioral.convert_files ~top files
  | "verible-ext" ->
      Verible_to_behavioral.convert_files_with_externals ~top files
  | "slang" ->
      (match Slang_to_behavioral.convert_files ~top files with
       | Some p -> p
       | None -> failwith "slang frontend failed")
  | "yosys" ->
      let tmp = Filename.temp_file "yosys_" ".il" in
      run_yosys_to_rtlil ~top ~files ~out:tmp;
      let p = Rtlil_to_behavioral.convert_file tmp in
      (try Sys.remove tmp with _ -> ());
      p
  | "verilator" ->
      (match files with
       | [j] ->
           (match Verilator_to_behavioral.convert_verilator_json_to_behavioral j with
            | Some p -> p
            | None -> failwith "verilator JSON parse failed")
       | _ -> failwith "verilator frontend takes a single .json")
  | "vhdl" ->
      (match files with
       | [f] ->
           (match Vhdl_to_behavioral.convert_vhdl_file_to_behavioral f with
            | Some p -> p
            | None -> failwith "vhdl frontend failed")
       | _ -> failwith "vhdl frontend takes a single .vhd")
  | "surelog" ->
      (* Surelog UHDM dump path.  The frontend currently extracts
         module-level port surface only; processes/cont_assigns are
         the open task #50.  Useful as an interface-shape oracle and
         as a pipecleaner for leaf cells, but not yet a full Z3
         miter peer.  Argument is a path to a pre-captured
         uhdm-dump text file. *)
      (match files with
       | [f] when Filename.check_suffix f ".dump" ->
           Surelog_to_behavioral.convert_dump_file f
       | _ -> failwith
                "surelog frontend takes a single .dump (run \
                 `surelog -parse -sverilog FILE.sv && uhdm-dump …` first)")
  | other -> failwith ("unknown frontend: " ^ other)

(* ──────────────────────────────────────────────────────────────────
 * Lua-callable shims. Each takes/returns string handles. *)

let lparse frontend top files =
  let p = load_frontend ~frontend ~top ~files in
  hadd (Prog (top, p))

(* No-top "read everything" entry — see Verible_to_behavioral.convert_files_all.
 * Currently only the verible frontend has a no-top path; the others fall
 * back to load_frontend with "" as top, which slang/yosys will reject —
 * fine, the GUI guards on frontend before calling this. *)
let lparse_all frontend files =
  let p = match frontend with
    | "verible" -> Verible_to_behavioral.convert_files_all files
    | other -> load_frontend ~frontend:other ~top:"" ~files
  in
  hadd (Prog ("(all)", p))

let lpick prog_h top =
  let _, p = find_prog prog_h in
  match List.find_opt (fun (m : bmodule) -> m.name = top) p.modules with
  | Some m -> hadd (Mod (top, m, p))
  | None ->
      failwith (Printf.sprintf "no module '%s' in %s" top prog_h)

(* Verification pipeline (mirrors cmd_miter in sv_decompiler.ml):
 *   1. Behavioral_arch_subst — abstract attributed adder/mul leaves
 *      to BBinOp ops gated on a `verify-arch` certificate.
 *   2. Behavioral_hier — transiently flatten what remains for Z3.
 * The source bprograms are not modified. *)
let prep_for_z3 (m : bmodule) (p : bprogram) : bmodule =
  let p, _n = Behavioral_arch_subst.substitute_program p in
  match List.find_opt (fun (mm : bmodule) -> mm.name = m.name) p.modules with
  | None -> m
  | Some m' ->
      if m'.instances = [] then m'
      else Behavioral_hier.flatten_for_z3 p ~top:m'.name

let lmiter a_h b_h =
  let (_, ma, pa) = find_mod a_h in
  let (_, mb, pb) = find_mod b_h in
  let ma' = prep_for_z3 ma pa in
  let mb' = prep_for_z3 mb pb in
  if Z3_miter.check_miter_equivalence ma' mb' then "EQUIVALENT" else "DIFFER"

let default_lib () =
  let home = try Sys.getenv "HOME" with Not_found -> "" in
  home ^ "/hardcaml-lua.0.0.1/liberty/simcells.lib"

let lliberty file =
  let lib = Sv_liberty.parse_liberty_file file in
  hadd (Lib (lib.lib_name, lib))

let lexpand prog_h lib_h =
  let label, p = find_prog prog_h in
  let _, lib  = find_lib lib_h in
  let p' = Gate_netlist_to_behavioral.expand_program lib p in
  hadd (Prog (label, p'))

let lgate_miter top beh gate lib_opt =
  let lib_path = match lib_opt with "" -> default_lib () | s -> s in
  if not (Sys.file_exists lib_path) then
    failwith ("Liberty file not found: " ^ lib_path);
  let lib = Sv_liberty.parse_liberty_file lib_path in
  let beh_p =
    Verible_to_behavioral.convert_files ~top [beh] in
  let gate_clean =
    Gate_netlist_to_behavioral.preprocess_gate_file gate in
  let gate_p =
    Verible_to_behavioral.convert_files_with_externals
      ~top [gate_clean] in
  let gate_p = Gate_netlist_to_behavioral.expand_program lib gate_p in
  let pick label src =
    match List.find_opt (fun (m : bmodule) -> m.name = top) src with
    | Some m -> m
    | None -> failwith (label ^ ": no module " ^ top) in
  let mb = pick "behavioral" beh_p.modules in
  let mg = pick "gate"       gate_p.modules in
  if Z3_miter.check_miter_equivalence mb mg then "EQUIVALENT" else "DIFFER"

let lbir h =
  match Hashtbl.find_opt lhash h with
  | Some (Prog (_, p))    -> string_of_bprogram p
  | Some (Mod  (_, m, _)) -> string_of_bmodule m
  | Some (Lib  (n, l)) ->
      Printf.sprintf "library %s: %d cells" n (Hashtbl.length l.cells)
  | None -> failwith ("unknown handle " ^ h)

(* Critical-path timing report for a module. Returns the printable
 * report (same string the CLI prints). Optional target-depth: if
 * provided, also returns suggested cert-gated upgrades. *)
let ltiming h target_depth =
  let _, m, _ = find_mod h in
  let arrivals = Behavioral_timing.compute_arrivals m in
  let paths = Behavioral_timing.endpoint_paths arrivals m in
  let target = if target_depth <= 0 then max_int else target_depth in
  let report = Behavioral_timing.report ~target_depth:target paths in
  let upgrades =
    if target = max_int then []
    else
      let failing =
        List.filter (fun (p : Behavioral_timing.path_report) ->
          p.arrival > target) paths in
      Behavioral_timing.suggest_upgrades failing in
  report ^ Behavioral_timing.format_upgrades upgrades

let linsts h =
  match Hashtbl.find_opt lhash h with
  | Some (Mod (_, m, _)) ->
      String.concat "\n"
        (List.map (fun (i : binstance) ->
          let conns = String.concat ", "
            (List.map (fun (p, e) ->
              Printf.sprintf ".%s(%s)" p (string_of_bexpr e)
            ) i.port_connections) in
          Printf.sprintf "  %s : %s (%s)"
            i.inst_name i.module_name conns
        ) m.instances)
  | _ -> "<not a module>"

let lname h =
  match Hashtbl.find_opt lhash h with
  | Some (Prog (n, _)) | Some (Mod (n, _, _)) | Some (Lib (n, _)) -> n
  | None -> failwith ("unknown handle " ^ h)

(* ──────────────────────────────────────────────────────────────────
 * HDL emit + cross-translate.  These expose the BIR→Verilog and
 * BIR→VHDL emitters plus the convert_hdl pipeline (license-header
 * preserving, language-detected by extension). *)

let lemit_verilog h =
  match Hashtbl.find_opt lhash h with
  | Some (Prog (_, p)) -> Behavioral_to_verilog.verilog_of_program p
  | Some (Mod  (_, m, _)) ->
      Behavioral_to_verilog.verilog_of_program
        { modules=[m]; library_cells=[] }
  | _ -> failwith ("emit_verilog: not a program/module handle: " ^ h)

let lemit_vhdl h =
  match Hashtbl.find_opt lhash h with
  | Some (Prog (_, p)) -> Behavioral_to_vhdl.vhdl_of_program p
  | Some (Mod  (_, m, _)) ->
      Behavioral_to_vhdl.vhdl_of_program
        { modules=[m]; library_cells=[] }
  | _ -> failwith ("emit_vhdl: not a program/module handle: " ^ h)

let lwrite_verilog h path =
  let body = lemit_verilog h in
  let oc = open_out path in
  output_string oc body;
  close_out oc;
  "ok"

let lwrite_vhdl h path =
  let body = lemit_vhdl h in
  let oc = open_out path in
  output_string oc body;
  close_out oc;
  "ok"

(* Run the full convert_hdl pipeline (header preservation +
 * frontend dispatch + emitter dispatch) without spawning a subprocess
 * — gives Lua scripts the same end-to-end translation that the
 * convert_hdl exe offers, and returns the output path. *)
let lconvert_hdl input output =
  let kind p =
    match String.lowercase_ascii (Filename.extension p) with
    | ".vhd" | ".vhdl" -> `Vhdl
    | ".v" | ".sv" -> `Verilog
    | _ -> failwith ("convert_hdl: unknown extension: " ^ p)
  in
  let src = kind input and dst = kind output in
  (* read source *)
  let ic = open_in input in
  let n = in_channel_length ic in
  let buf = Bytes.create n in
  really_input ic buf 0 n;
  close_in ic;
  let src_text = Bytes.unsafe_to_string buf in
  (* extract leading comment block (matching convert_hdl's logic) *)
  let lines = String.split_on_char '\n' src_text in
  let is_blank s = String.trim s = "" in
  let comment_match k s =
    let s = String.trim s in
    String.length s >= 2 &&
    (match k with
     | `Vhdl -> String.sub s 0 2 = "--"
     | `Verilog -> String.sub s 0 2 = "//" || String.sub s 0 2 = "/*")
  in
  let rec take acc = function
    | [] -> List.rev acc
    | l :: tl when is_blank l || comment_match src l -> take (l :: acc) tl
    | _ -> List.rev acc
  in
  let header_lines = take [] lines in
  let strip_marker s =
    let s = String.trim s in
    let lp p = let lp = String.length p in
      if String.length s >= lp && String.sub s 0 lp = p
      then String.sub s lp (String.length s - lp) |> String.trim else s in
    match src with
    | `Vhdl -> lp "--"
    | `Verilog ->
        let s = lp "//" in let s = if String.length s >= 2 && String.sub s 0 2 = "/*"
                                   then String.sub s 2 (String.length s - 2) else s in
        let n = String.length s in
        let s = if n >= 2 && String.sub s (n-2) 2 = "*/" then String.sub s 0 (n-2) else s in
        String.trim s
  in
  let prefix = match dst with `Vhdl -> "-- " | `Verilog -> "// " in
  let header =
    if header_lines = [] then ""
    else String.concat "\n"
           (List.map (fun l ->
              let stripped = strip_marker l in
              if stripped = "" then "" else prefix ^ stripped)
              header_lines) ^ "\n"
  in
  let prog =
    match src with
    | `Vhdl ->
        (match Vhdl_to_behavioral.convert_vhdl_file_to_behavioral input with
         | Some p -> p | None -> failwith "vhdl frontend failed")
    | `Verilog ->
        let auto_top () =
          let re = Str.regexp "^[ \t]*module[ \t\n]+\\([A-Za-z_][A-Za-z0-9_]*\\)" in
          try let _ = Str.search_forward re src_text 0 in
              Str.matched_group 1 src_text
          with Not_found -> Filename.remove_extension (Filename.basename input)
        in
        Verible_to_behavioral.convert_files ~top:(auto_top ()) [input]
  in
  let body = match dst with
    | `Vhdl -> Behavioral_to_vhdl.vhdl_of_program prog
    | `Verilog -> Behavioral_to_verilog.verilog_of_program prog
  in
  let oc = open_out output in
  if header <> "" then output_string oc header;
  output_string oc body;
  close_out oc;
  output

let litems () =
  let lst = Hashtbl.fold (fun k v acc ->
    let kind = match v with
      | Prog (n, p) ->
          Printf.sprintf "program %s (%d modules)" n
            (List.length p.modules)
      | Mod (n, _, _) -> Printf.sprintf "module %s" n
      | Lib (n, l) ->
          Printf.sprintf "library %s (%d cells)" n
            (Hashtbl.length l.cells)
    in
    (k ^ "\t" ^ kind) :: acc
  ) lhash [] in
  String.concat "\n" (List.sort compare lst)

(* ──────────────────────────────────────────────────────────────────
 * GUI hooks — populated by sv_gui.ml at startup so the embedded Lua
 * interpreter can drive lablgtk3 widgets. CLI users (sv_decompiler,
 * sv_main_unified, …) never set these, so the gui.* Lua functions
 * collapse to no-ops there. Hooks stay primitive (string/unit) so this
 * file does NOT depend on lablgtk3. *)

let gui_add_menu_hook       : (string -> unit) ref = ref (fun _ -> ())
let gui_add_item_hook       : (string -> string -> string -> unit) ref =
  ref (fun _ _ _ -> ())
let gui_set_text_hook       : (string -> unit) ref = ref (fun _ -> ())
let gui_get_text_hook       : (unit -> string) ref = ref (fun () -> "")
let gui_append_text_hook    : (string -> unit) ref = ref (fun _ -> ())
let gui_message_hook        : (string -> unit) ref = ref (fun s -> print_endline s)
let gui_error_hook          : (string -> unit) ref =
  ref (fun s -> prerr_endline s)
let gui_open_file_hook      : (unit -> string) ref = ref (fun () -> "")
let gui_save_file_hook      : (unit -> string) ref = ref (fun () -> "")
let gui_set_status_hook     : (string -> unit) ref = ref (fun _ -> ())
let gui_quit_hook           : (unit -> unit) ref = ref (fun () -> ())

let lgui_add_menu  name      = !gui_add_menu_hook name; ""
let lgui_add_item  m l h     = !gui_add_item_hook m l h; ""
let lgui_set_text  s         = !gui_set_text_hook s; ""
let lgui_get_text  ()        = !gui_get_text_hook ()
let lgui_append_text s       = !gui_append_text_hook s; ""
let lgui_message   s         = !gui_message_hook s; ""
let lgui_error     s         = !gui_error_hook s; ""
let lgui_open_file ()        = !gui_open_file_hook ()
let lgui_save_file ()        = !gui_save_file_hook ()
let lgui_set_status s        = !gui_set_status_hook s; ""
let lgui_quit      ()        = !gui_quit_hook (); ""

(* ──────────────────────────────────────────────────────────────────
 * lua-ml interpreter setup. Boilerplate copied from
 * hardcaml-lua/myluaclient.ml; the Char/Pair user-types are kept so the
 * standard library combine works, but we don't expose them in scripts. *)

module LuaChar = struct
  type 'a t       = char
  let tname       = "char"
  let eq _        = fun x y -> x = y
  let to_string   = fun _ c -> String.make 1 c
end

module Pair = struct
  type 'a t       = 'a * 'a
  let tname       = "pair"
  let eq _        = fun x y -> x = y
  let to_string   = fun f (x, y) -> Printf.sprintf "(%s,%s)" (f x) (f y)
end

module T =
  Lua.Lib.Combine.T3
    (LuaChar)
    (Pair)
    (Luaiolib.T)

module LuaCharT = T.TV1
module PairT    = T.TV2
module LuaioT   = T.TV3

module MakeLib
    (CharV : Lua.Lib.TYPEVIEW with type 'a t = 'a LuaChar.t)
    (PairV : Lua.Lib.TYPEVIEW with type 'a t = 'a Pair.t
                              and  type 'a combined = 'a CharV.combined)
  : Lua.Lib.USERCODE with type 'a userdata' = 'a CharV.combined = struct

  type 'a userdata' = 'a PairV.combined
  module M (C : Lua.Lib.CORE with type 'a V.userdata' = 'a userdata') = struct
    module V = C.V
    let ( **-> )  = V.( **-> )
    let ( **->> ) x y = x **-> V.result y

    let wrap1 f a   = try f a   with e -> Printexc.print_backtrace stdout; raise e
    let wrap2 f a b = try f a b with e -> Printexc.print_backtrace stdout; raise e
    let wrap3 f a b c   = try f a b c   with e -> Printexc.print_backtrace stdout; raise e
    let wrap4 f a b c d = try f a b c d with e -> Printexc.print_backtrace stdout; raise e

    let init g =
      C.register_module "svd" [
        "parse",      V.efunc (V.string **-> V.string **-> V.list V.string
                               **->> V.string)
                       (wrap3 lparse);
        "parse_all",  V.efunc (V.string **-> V.list V.string
                               **->> V.string)
                       (wrap2 lparse_all);
        "pick",       V.efunc (V.string **-> V.string **->> V.string)
                       (wrap2 lpick);
        "miter",      V.efunc (V.string **-> V.string **->> V.string)
                       (wrap2 lmiter);
        "liberty",    V.efunc (V.string **->> V.string)
                       (wrap1 lliberty);
        "expand",     V.efunc (V.string **-> V.string **->> V.string)
                       (wrap2 lexpand);
        "gate_miter", V.efunc (V.string **-> V.string **-> V.string
                               **-> V.string **->> V.string)
                       (wrap4 lgate_miter);
        "bir",        V.efunc (V.string **->> V.string) (wrap1 lbir);
        "insts",      V.efunc (V.string **->> V.string) (wrap1 linsts);
        "timing",     V.efunc (V.string **-> V.int **->> V.string)
                       (wrap2 ltiming);
        "name",       V.efunc (V.string **->> V.string) (wrap1 lname);
        "items",      V.efunc (V.unit **->> V.string)   (wrap1 litems);
        "emit_verilog",  V.efunc (V.string **->> V.string) (wrap1 lemit_verilog);
        "emit_vhdl",     V.efunc (V.string **->> V.string) (wrap1 lemit_vhdl);
        "write_verilog", V.efunc (V.string **-> V.string **->> V.string)
                          (wrap2 lwrite_verilog);
        "write_vhdl",    V.efunc (V.string **-> V.string **->> V.string)
                          (wrap2 lwrite_vhdl);
        "convert_hdl",   V.efunc (V.string **-> V.string **->> V.string)
                          (wrap2 lconvert_hdl);
      ] g;
      C.register_module "gui" [
        "add_menu",    V.efunc (V.string **->> V.string) (wrap1 lgui_add_menu);
        "add_item",    V.efunc (V.string **-> V.string **-> V.string
                                **->> V.string)
                       (wrap3 lgui_add_item);
        "set_text",    V.efunc (V.string **->> V.string) (wrap1 lgui_set_text);
        "get_text",    V.efunc (V.unit **->> V.string)   (wrap1 lgui_get_text);
        "append_text", V.efunc (V.string **->> V.string)
                       (wrap1 lgui_append_text);
        "message",     V.efunc (V.string **->> V.string) (wrap1 lgui_message);
        "error",       V.efunc (V.string **->> V.string) (wrap1 lgui_error);
        "open_file",   V.efunc (V.unit **->> V.string)   (wrap1 lgui_open_file);
        "save_file",   V.efunc (V.unit **->> V.string)   (wrap1 lgui_save_file);
        "set_status",  V.efunc (V.string **->> V.string)
                       (wrap1 lgui_set_status);
        "quit",        V.efunc (V.unit **->> V.string)   (wrap1 lgui_quit);
      ] g
  end
end

module W = Lua.Lib.WithType (T)
module C =
  Lua.Lib.Combine.C5
    (Luaiolib.Make (LuaioT))
    (Luacamllib.Make (LuaioT))
    (W (Luastrlib.M))
    (W (Luamathlib.M))
    (MakeLib (LuaCharT) (PairT))

module I =
  Lua.MakeInterp
    (Lua.Parser.MakeStandard)
    (Lua.MakeEval (T) (C))

(* Public entry: run a Lua script file and return its exit code (0 on
 * success, 1 on Lua exception). *)
let run_script path =
  if not (Sys.file_exists path) then begin
    Printf.eprintf "lua script not found: %s\n" path;
    1
  end else begin
    let ic = open_in path in
    let buf = Buffer.create 1024 in
    (try while true do
       Buffer.add_channel buf ic 4096
     done with End_of_file -> ());
    close_in ic;
    let state = I.mk () in
    try
      ignore (I.dostring state (Buffer.contents buf));
      0
    with e ->
      Printf.eprintf "lua: %s\n" (Printexc.to_string e);
      1
  end
