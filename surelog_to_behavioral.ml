(* Surelog UHDM dump → Behavioral_ir bridge.
 *
 * Walks the token tree produced by Surelog_uhdm.ml_start (a hierarchy
 * of TUPLE2/TUPLE3/.../TLIST + terminal tokens) and emits a
 * Behavioral_ir.bprogram. This is the 5th elaboration reference point
 * after Verilator JSON, Verible parse-tree, Yosys RTLIL, and Vivado VHDL.
 *
 * Coverage today (developed against test/surelog/apb_uart.dump):
 *   - Module names, ports (direction, width), nets
 *   - Stub processes (no body conversion yet — emits an empty
 *     BCombinational so module structure round-trips)
 *
 * Not yet:
 *   - Always-block bodies (vpiAlways → BSequential/BCombinational)
 *   - Continuous assigns (vpiContAssign → BCombinational)
 *   - Module instances (vpiInstance → binstance)
 *   - Function/task bodies *)

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
 * → logic_typespec ladder. The dump usually emits this as `, line:L:C,
 * endln:L:C` annotations on logic_typespec — width has to come from the
 * vpiSize attribute, which appears on constant nodes but not on plain
 * logic_typespec. For now we default to 1; the BSequential/BCombinational
 * encoder will widen when it sees concrete usage. Future work: parse
 * vpiTypespec → vpiActual → logic_typespec → range to get the real width. *)
let width_of_logic_net _node = 1

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
        initial_value = None;
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
  (* Stub: we don't decode processes/assigns/instances yet — the
   * structure below just records that a module exists with the right
   * signal surface. *)
  {
    name;
    params = [];
    signals;
    processes = [];
    instances = [];
    funcs = [];
    mems = [];
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
