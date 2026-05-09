(* Surelog UHDM dump → Behavioral_ir bridge — DEPRECATED for Z3 oracle work.
 *
 * Walks the token tree produced by Surelog_uhdm.ml_start (a hierarchy
 * of TUPLE2/TUPLE3/.../TLIST + terminal tokens) and emits a
 * Behavioral_ir.bprogram with module-level port surface only.
 *
 * **Status (2026-05-09):** parked as a port-surface pipecleaner; not
 * a Z3 oracle peer.  The fuller plan (cont_assigns, processes,
 * instances, typespec width resolution — pending task #50) was
 * abandoned in favour of slang because:
 *   1. Slang is a strict superset for elaboration coverage —
 *      parses CVA6's type parameters which Surelog can't (see
 *      project memory `project_surelog_role.md`).
 *   2. Slang is already a working Z3 oracle (test_z3_oracle slang
 *      verible counter → ✅ FORMALLY EQUIVALENT, eth_rstgen
 *      passes, multiple sv-tests cases pass).
 *   3. The remaining surelog→BIR work is ~200–300 lines of UHDM
 *      operator-tree walking just to *match* what slang already
 *      provides — pure duplication.
 *
 * **What stays:** Module names + ports.  Width extraction below
 * walks for inline `Vpisize:N` annotations when present (post-elab
 * dumps).  The lex/parse + token-tree infrastructure is preserved
 * so users with a UHDM dump can sanity-check it lands; for actual
 * Z3 oracle work, route through slang via test_z3_oracle.exe.
 *
 * Coverage today:
 *   - Module names, ports (direction, width)
 *   - Width: Vpisize annotations on logic_typespec (post-elab only)
 *
 * Won't fix here (use slang):
 *   - Always-block bodies, continuous assigns, instances, funcs *)

open Surelog_uhdm
open Behavioral_ir

(* ─── Token-tree walking ─────────────────────────────────────────── *)

(* Iterate over the immediate children of a tuple/tlist node. *)
let children = function
  | TLIST xs -> xs
  | TUPLE2 (a, b) -> [a; b]
  | TUPLE3 (a, b, c) -> [a; b; c]
  | TUPLE4 (a, b, c, d) -> [a; b; c; d]
  | TUPLE5 (a, b, c, d, e) -> [a; b; c; d; e]
  | TUPLE6 (a, b, c, d, e, f) -> [a; b; c; d; e; f]
  | TUPLE7 (a, b, c, d, e, f, g) -> [a; b; c; d; e; f; g]
  | _ -> []

(* True if a node is `TUPLE2 (kw, _)` for the given kw token. *)
let is_tagged kw = function
  | TUPLE2 (k, _) when k = kw -> true
  | _ -> false

(* Pull the payload of `TUPLE2 (kw, payload)` for the first matching child. *)
let find_tag kw t =
  List.find_map (function
    | TUPLE2 (k, v) when k = kw -> Some v
    | _ -> None
  ) (children t)

(* All payloads for `TUPLE2 (kw, _)` children. *)
let find_tag_all kw t =
  List.filter_map (function
    | TUPLE2 (k, v) when k = kw -> Some v
    | _ -> None
  ) (children t)

(* Strip a `work@<name>` wrapper. Used for vpiName/vpiFullName values. *)
let unwrap_workname = function
  | STRING s ->
      (try
        let i = String.index s '@' in
        String.sub s (i + 1) (String.length s - i - 1)
      with Not_found -> s)
  | TLIST xs ->
      (* fullnam often comes back as TLIST [STRING; STRING; ...] for
       * dotted paths — flatten with '.' separators. *)
      String.concat "."
        (List.filter_map (function STRING s -> Some s | _ -> None) xs)
  | _ -> ""

let str_of = function STRING s -> s | _ -> ""

(* ─── Direction decoding ─────────────────────────────────────────── *)

(* vpiDirection codes (per UHDM): 1=in, 2=out, 3=inout. The grammar
 * encodes these as `TUPLE2 (Vpidirection, Int n)`. *)
(* bsignal only has `Input/`Output/`Internal — coerce inout (3) to
 * internal until BIR grows a real inout direction. *)
let direction_of_int = function
  | 1 -> `Input
  | 2 -> `Output
  | _ -> `Internal

(* Walk past the wrapping `TUPLE2 (Port, body)` /
 * `TUPLE2 (Logic_net, body)` produced by the grammar so callers see
 * the inner property list directly. *)
let unwrap_kind kw t =
  match t with
  | TUPLE2 (k, body) when k = kw -> body
  | _ -> t

let direction_of_port port =
  let port = unwrap_kind Port port in
  match find_tag Vpidirection port with
  | Some (Int n) -> direction_of_int n
  | _ -> `Internal

(* ─── Signal extraction ──────────────────────────────────────────── *)

(* Build a port-name → direction map by scanning the module's vpiPort
 * children. Logic_net entries are tagged Internal until a port pulls
 * them into the I/O surface. *)
let port_directions m_node =
  let tbl = Hashtbl.create 16 in
  List.iter (fun port_node ->
    let inner = unwrap_kind Port port_node in
    match find_tag Vpiname inner with
    | Some (STRING n) ->
        Hashtbl.replace tbl n (direction_of_port inner)
    | _ -> ()
  ) (find_tag_all Vpiport m_node);
  tbl

(* Logic_net carries the signal width via a vpiTypespec → ref_typespec
 * → logic_typespec ladder.  Walk the subtree looking for the first
 * `Vpisize:N` annotation; that's the post-elab width.  Falls back to
 * 1 when the dump is pre-elab (no typespec was resolved) — that's
 * the common case for the apb_uart fixture which is a Pre-Elab dump. *)
let rec find_size t =
  match t with
  | TUPLE2 (Vpisize, Int n) -> Some n
  | TUPLE2 (_, _) | TUPLE3 _ | TUPLE4 _ | TUPLE5 _
  | TUPLE6 _ | TUPLE7 _ | TLIST _ ->
      List.fold_left (fun acc c ->
        match acc with Some _ -> acc | None -> find_size c)
        None (children t)
  | _ -> None

let width_of_logic_net node =
  match find_size node with Some n when n > 0 -> n | _ -> 1

let extract_signal m_node port_dir net_node =
  let inner = unwrap_kind Logic_net net_node in
  match find_tag Vpiname inner with
  | None -> None
  | Some (STRING name) ->
      let direction =
        try Hashtbl.find port_dir name
        with Not_found -> `Internal
      in
      let width = width_of_logic_net inner in
      let _ = m_node in
      Some {
        name;
        stype = BInt { width; signed = Unsigned };
        direction;
        initial_value = None; attrs = []; 
      }
  | _ -> None

let extract_signals m_node =
  let pd = port_directions m_node in
  List.filter_map (extract_signal m_node pd) (find_tag_all Vpinet m_node)

(* ─── Module assembly ────────────────────────────────────────────── *)

(* A module_inst node carries a `Vpidefname` whose payload is the
 * module's definition name (`work@apb_uart` etc.). Strip the `work@`
 * prefix to match how Verilator/Verible/Yosys name them. *)
let module_name m_node =
  match find_tag Vpidefname m_node with
  | Some n -> unwrap_workname n
  | None ->
      (match find_tag Vpiname m_node with
       | Some n -> unwrap_workname n
       | None -> "<unnamed>")

let extract_module m_node : bmodule =
  let name = module_name m_node in
  let signals = extract_signals m_node in
  (* Processes/cont_assigns/instances NOT decoded.  Slang covers the
   * same SystemVerilog corpus (and more — it parses CVA6's type
   * parameters which Surelog can't), is already Z3-capable, and
   * needs none of the UHDM operator-tree walking that a full
   * surelog → BIR would require.  Keep this frontend at port-
   * surface granularity as a UHDM dump pipecleaner; for actual Z3
   * oracle work, route through slang via test_z3_oracle.exe. *)
  {
    name;
    params = [];
    signals;
    processes = [];
    instances = [];
    funcs = [];
    mems = []; attrs = [];
  }

(* ─── Top-level: find every `Uhdmallmodules` payload ─────────────── *)

(* Each top-level `TUPLE2 (Uhdmallmodules, body)` is ONE module —
 * `body` is the TLIST of that module's properties (Vpiname, Vpiport,
 * Vpinet, Vpiprocess, …), NOT a list of modules. Same for
 * Uhdmtopmodules (the design's top — usually duplicates one of the
 * Uhdmallmodules entries; dedup by name later).
 *
 * We synthesise a fresh TUPLE2 (Module_inst, body) for each so that
 * find_tag/find_tag_all helpers downstream see a uniform shape. *)
let collect_module_insts (tokens : token list) : token list =
  List.filter_map (function
    | TUPLE2 (Uhdmallmodules, body)
    | TUPLE2 (Uhdmtopmodules, body) -> Some body
    | _ -> None
  ) tokens

let convert_tokens (tokens : token list) : bprogram =
  let mod_nodes = collect_module_insts tokens in
  let modules = List.map extract_module mod_nodes in
  (* Dedup by name — same module appears under both All and Top *)
  let seen = Hashtbl.create 16 in
  let unique = List.filter (fun m ->
    if Hashtbl.mem seen m.name then false
    else (Hashtbl.add seen m.name (); true)
  ) modules in
  { modules = unique;
    library_cells = [] }

(* Convenience: parse a uhdm-dump text file and convert to BIR. *)
let convert_dump_file path =
  let ic = open_in path in
  let lb = Lexing.from_channel ic in
  let lex = Surelog_uhdm_lex.deflate Surelog_uhdm_lex.token in
  let (_cache, tokens) = Surelog_uhdm.ml_start lex lb in
  close_in ic;
  convert_tokens tokens
