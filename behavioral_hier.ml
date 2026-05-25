(* Hierarchical encode helper.
 *
 * Produces a transient flat bmodule by inlining every reachable
 * `binstance` in a top-level module. Used only at Z3-encode time —
 * the source bprogram is left intact, so module boundaries remain
 * available to downstream tools (timing, layout, hierarchical
 * verification certificates).
 *
 * Compared to Behavioral_flatten.flatten_program (which implements
 * specialisation-aware flattening for the cva6 sweep), this is a
 * straight recursive inliner with name prefixing — purpose-built for
 * the Z3 miter and intentionally simple. *)

open Behavioral_ir

let pname prefix name =
  if prefix = "" then name else prefix ^ "__" ^ name

(* Walk an expression. Substitution wins over prefixing: if a BVar's
 * name is in `subst`, replace with that bexpr verbatim (its sub-
 * expressions stay in *parent* scope, so we don't recurse into them).
 * Otherwise, prefix the BVar name with the current scope. *)
let rec sub_bexpr ~subst ~prefix = function
  | BVar n ->
      (match List.assoc_opt n subst with
       | Some e -> e
       | None -> BVar (pname prefix n))
  | BConst _ as e -> e
  | BBinOp { op; lhs; rhs; result_type } ->
      BBinOp { op;
               lhs = sub_bexpr ~subst ~prefix lhs;
               rhs = sub_bexpr ~subst ~prefix rhs;
               result_type }
  | BUnOp { op; operand; result_type } ->
      BUnOp { op;
              operand = sub_bexpr ~subst ~prefix operand;
              result_type }
  | BSelect { array; index } ->
      BSelect { array = sub_bexpr ~subst ~prefix array;
                index = sub_bexpr ~subst ~prefix index }
  | BSlice { signal; msb; lsb } ->
      BSlice { signal = sub_bexpr ~subst ~prefix signal; msb; lsb }
  | BConcat es -> BConcat (List.map (sub_bexpr ~subst ~prefix) es)
  | BReplicate { count; value } ->
      BReplicate { count; value = sub_bexpr ~subst ~prefix value }
  | BCond { condition; then_val; else_val } ->
      BCond { condition  = sub_bexpr ~subst ~prefix condition;
              then_val   = sub_bexpr ~subst ~prefix then_val;
              else_val   = sub_bexpr ~subst ~prefix else_val }
  | BCall { func; args } ->
      BCall { func; args = List.map (sub_bexpr ~subst ~prefix) args }

(* Walk a statement. lhs_subst maps `formal` → `parent_actual_name` for
 * the child's output ports — when the child writes to its formal-out,
 * the inlined assignment writes to the parent's actual wire instead,
 * skipping the prefix. *)
let rec sub_bstmt ~subst ~lhs_subst ~prefix = function
  | BAssign { lhs; rhs } ->
      let new_lhs =
        match List.assoc_opt lhs lhs_subst with
        | Some n -> n
        | None -> pname prefix lhs in
      BAssign { lhs = new_lhs; rhs = sub_bexpr ~subst ~prefix rhs }
  | BIf { condition; then_stmts; else_stmts } ->
      BIf { condition  = sub_bexpr ~subst ~prefix condition;
            then_stmts = List.map (sub_bstmt ~subst ~lhs_subst ~prefix)
                                  then_stmts;
            else_stmts = List.map (sub_bstmt ~subst ~lhs_subst ~prefix)
                                  else_stmts }
  | BCase { selector; cases; default } ->
      BCase { selector = sub_bexpr ~subst ~prefix selector;
              cases    = List.map (fun (v, body) ->
                (sub_bexpr ~subst ~prefix v,
                 List.map (sub_bstmt ~subst ~lhs_subst ~prefix) body))
                cases;
              default  = List.map (sub_bstmt ~subst ~lhs_subst ~prefix)
                                  default }
  | BWhile { condition; body } ->
      BWhile { condition = sub_bexpr ~subst ~prefix condition;
               body      = List.map (sub_bstmt ~subst ~lhs_subst ~prefix)
                                    body }
  | BFor { init; condition; update; body } ->
      BFor { init      = sub_bstmt ~subst ~lhs_subst ~prefix init;
             condition = sub_bexpr ~subst ~prefix condition;
             update    = sub_bstmt ~subst ~lhs_subst ~prefix update;
             body      = List.map (sub_bstmt ~subst ~lhs_subst ~prefix)
                                  body }
  | BBlock body ->
      BBlock (List.map (sub_bstmt ~subst ~lhs_subst ~prefix) body)
  | BCallStmt { func; args } ->
      BCallStmt { func; args = List.map (sub_bexpr ~subst ~prefix) args }
  | BReturn None -> BReturn None
  | BReturn (Some e) -> BReturn (Some (sub_bexpr ~subst ~prefix e))

(* Resolve a bare signal NAME (clock / reset / sensitivity) through the
   port connections: an input port wired to a parent net [BVar pn] becomes
   [pn], an output port becomes its lhs_subst actual, otherwise it's an
   internal signal and gets the instance prefix.  Without this, an inlined
   module's `@posedge clk` becomes `@posedge inst__clk` with no driver. *)
let sub_name ~subst ~lhs_subst ~prefix n =
  match List.assoc_opt n subst with
  | Some (BVar pn) -> pn
  | Some _ -> pname prefix n
  | None ->
    (match List.assoc_opt n lhs_subst with
     | Some pn -> pn
     | None -> pname prefix n)

let sub_process ~subst ~lhs_subst ~prefix = function
  | BCombinational { name; sensitivity; body } ->
      BCombinational {
        name = pname prefix name;
        sensitivity = List.map (function
          | BPosEdge n -> BPosEdge (sub_name ~subst ~lhs_subst ~prefix n)
          | BNegEdge n -> BNegEdge (sub_name ~subst ~lhs_subst ~prefix n)
          | BLevel   n -> BLevel   (sub_name ~subst ~lhs_subst ~prefix n)
          | BAny       -> BAny
        ) sensitivity;
        body = List.map (sub_bstmt ~subst ~lhs_subst ~prefix) body }
  | BSequential { name; clock; clock_edge;
                  reset; reset_edge; reset_async; body } ->
      BSequential {
        name        = pname prefix name;
        clock       = sub_name ~subst ~lhs_subst ~prefix clock;
        clock_edge;
        reset       = (match reset with
                       | None -> None
                       | Some n -> Some (sub_name ~subst ~lhs_subst ~prefix n));
        reset_edge;
        reset_async;
        body        = List.map (sub_bstmt ~subst ~lhs_subst ~prefix) body }

(* Inline `child` (already flattened) into `parent` at instance `inst`.
 * Returns a new bmodule with child's signals/processes added under the
 * `<inst.inst_name>__` prefix, port pins routed via subst/lhs_subst. *)
let inline_instance ~debug (parent : bmodule) (inst : binstance)
                    (child : bmodule) : bmodule =
  let prefix = inst.inst_name in
  (* Classify each child port pin by direction. *)
  let port_dirs = List.filter_map (fun (s : bsignal) ->
    match s.direction with
    | `Input  -> Some (s.name, `Input)
    | `Output -> Some (s.name, `Output)
    | _ -> None
  ) child.signals in
  (* subst routes READS of a child port to its parent net.  Inputs map to
     the connected actual; OUTPUTS that are read internally (mem_addr,
     pcpi_*, …) must also map to their parent net — otherwise an internal
     read becomes inst__port (prefixed, undriven) while the driver uses the
     parent net via lhs_subst, splitting them. *)
  let subst = List.filter_map (fun (formal, actual) ->
    match List.assoc_opt formal port_dirs, actual with
    | Some `Input, _ -> Some (formal, actual)
    | Some `Output, BVar _ -> Some (formal, actual)
    | _ -> None
  ) inst.port_connections in
  let lhs_subst = List.filter_map (fun (formal, actual) ->
    match List.assoc_opt formal port_dirs, actual with
    | Some `Output, BVar n -> Some (formal, n)
    | Some `Output, _ ->
        if debug then
          Printf.eprintf
            "[hier] %s.%s drives non-BVar; bit-blast not yet supported\n"
            inst.inst_name formal;
        None
    | _ -> None
  ) inst.port_connections in
  (* Promote child's internal signals to parent-scope with prefix. Skip
   * port pins — their names already resolve via subst / lhs_subst. *)
  (* Promote every child signal (incl. port pins) under the prefix as an
     internal net.  Connected ports are remapped to parent nets via
     subst/lhs_subst (leaving the promoted copy dead), but UNconnected or
     conditionally-undriven pins (e.g. external pcpi/trace when those
     features are off) stay declared so create_circuit ties them to 0 at
     the right width instead of leaving a dangling reference. *)
  let new_signals =
    List.fold_left (fun acc (s : bsignal) ->
      { s with name = pname prefix s.name; direction = `Internal } :: acc)
      parent.signals child.signals
  in
  let new_processes =
    List.fold_left (fun acc proc ->
      sub_process ~subst ~lhs_subst ~prefix proc :: acc
    ) parent.processes child.processes in
  { parent with
    signals   = new_signals;
    processes = new_processes }

(* Flatten a top-level module by recursively inlining every reachable
 * binstance. Memoised by module name so cells used multiple times only
 * pay the recursion cost once.
 *
 * Returns a transient flat bmodule. The input bprogram is unchanged. *)
(* Strip the parameter-specialisation suffix from a module name. The
 * Verible / yosys-slang pipeline names specialised siblings as
 * `<base>__<suffix>` (e.g. `popcount__IW16`). *)
let base_of_name n =
  try
    let i = Str.search_forward (Str.regexp "__") n 0 in
    String.sub n 0 i
  with Not_found -> n

let flatten_for_z3 ?(debug = false) (p : bprogram) ~top : bmodule =
  let by_name = Hashtbl.create 16 in
  List.iter (fun m -> Hashtbl.replace by_name m.name m) p.modules;
  let by_base = List.fold_left (fun acc (m : bmodule) ->
    let b = base_of_name m.name in
    let bucket = try List.assoc b acc with Not_found -> [] in
    (b, m :: bucket) :: List.remove_assoc b acc
  ) [] p.modules in
  let cache = Hashtbl.create 16 in
  (* For an instance whose `module_name` doesn't resolve directly,
   * try `<base>__*` siblings disambiguated by Behavioral_flatten's
   * port-shape match. Needs caller context for the width scoring. *)
  let lookup_resolving ~parent (i : binstance) =
    match Hashtbl.find_opt by_name i.module_name with
    | Some m -> Some m
    | None ->
        let b = base_of_name i.module_name in
        let candidates =
          try List.assoc b by_base with Not_found -> [] in
        if candidates = [] then None
        else begin
          let caller_widths = List.filter_map (fun (s : bsignal) ->
            let w = match s.stype with
              | BInt { width; _ } -> width
              | BArray { size; element = BInt { width; _ }; _ } ->
                  size * width
              | _ -> 0 in
            if w > 0 then Some (s.name, w) else None
          ) parent.signals in
          let chosen = Behavioral_flatten.pick_specialised
            ~caller_signal_widths:caller_widths ~candidates i in
          if debug then
            (match chosen with
             | Some m ->
                 Printf.eprintf "[hier] %s: %s → specialised %s\n"
                   parent.name i.module_name m.name
             | None ->
                 Printf.eprintf "[hier] %s: %s → no specialised match\n"
                   parent.name i.module_name);
          chosen
        end
  in
  let rec flatten ~parent (i : binstance) : bmodule =
    match lookup_resolving ~parent i with
    | None ->
        (* External / leaf cell — Liberty cells, vendor primitives.
         * Empty placeholder; caller is responsible for handling
         * unresolved references (typically via expand_program). *)
        { name = i.module_name; params = []; signals = []; processes = [];
          instances = []; funcs = []; mems = []; attrs = [] }
    | Some m ->
        match Hashtbl.find_opt cache m.name with
        | Some cached -> cached
        | None ->
            let m_flat = flatten_one m in
            Hashtbl.replace cache m.name m_flat;
            m_flat
  and flatten_one (m : bmodule) : bmodule =
    let acc = ref { m with instances = [] } in
    List.iter (fun (i : binstance) ->
      let child_flat = flatten ~parent:m i in
      acc := inline_instance ~debug !acc i child_flat
    ) m.instances;
    !acc
  in
  match Hashtbl.find_opt by_name top with
  | Some m -> flatten_one m
  | None ->
      failwith ("flatten_for_z3: no module '" ^ top ^ "' in program")
