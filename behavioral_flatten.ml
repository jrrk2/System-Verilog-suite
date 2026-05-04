(* Module-instance flattening pass on Behavioral IR.
 *
 * Verible's specialise_design produces popcount__W64, popcount__W32,
 * etc. as separate bmodules — but each one's body still contains
 * `binstance` nodes that reference the BASE name "popcount", not the
 * specialised name. The Z3 miter operates on a SINGLE bmodule and
 * doesn't traverse instances, so an output that depends on a child
 * module's logic looks like a free variable to it (typically defaults
 * to 0), causing Z3 mismatches against the fully-elaborated Vivado
 * netlist.
 *
 * This pass walks each parent bmodule, picks the right specialised
 * child for each instance by base-name + port-shape match, and
 * inlines the child's signals + processes into the parent — same
 * pattern as Verilator's V3Inline does for tasks/functions, applied
 * to module hierarchy.
 *
 * Restrictions:
 * - Only inlines combinational children (no FFs). Hierarchy inlining
 *   for FF-bearing children needs FF lifting and is left for later.
 * - Recursive cycles guarded by a depth limit (default 16) — popcount
 *   bottoms out at W=1/2 in 6 levels, this gives slack.
 * - Internal child signals are renamed with the instance label as
 *   prefix to avoid collisions when multiple instances of the same
 *   module exist in the same parent. *)

open Behavioral_ir

let max_depth = 16

(* Compute the port shape (name → width) of a bmodule, for the I/O
 * ports only. Used to disambiguate sibling specialisations sharing a
 * base name. *)
let port_shape (m : bmodule) =
  List.filter_map (fun (s : bsignal) ->
    match s.direction with
    | `Input | `Output ->
        let w = match s.stype with
          | BInt { width; _ } -> width
          | BArray { size; element = BInt { width; _ }; _ } -> size * width
          | _ -> 0
        in
        Some (s.name, w)
    | _ -> None
  ) m.signals
  |> List.sort compare

(* Width of the expression connected at an instance port.
 * Conservative — for non-constant exprs we use the source signal's
 * declared width if we can find it. *)
let rec expr_width signal_widths = function
  | BConst { width; _ } -> width
  | BVar n ->
      (try List.assoc n signal_widths with Not_found -> 0)
  | BBinOp { result_type = BInt { width; _ }; _ }
  | BUnOp  { result_type = BInt { width; _ }; _ } -> width
  | BSlice { msb; lsb; _ } -> msb - lsb + 1
  | BConcat es ->
      List.fold_left (fun a e -> a + expr_width signal_widths e) 0 es
  | BReplicate { count; value } -> count * expr_width signal_widths value
  | BCond { then_val; _ } -> expr_width signal_widths then_val
  | _ -> 0

(* Pick the specialised child for an instance — match base name,
 * then disambiguate by port-shape compatibility with the
 * connected widths at the instance site. *)
let pick_specialised
    ~caller_signal_widths
    ~candidates
    (i : binstance) =
  let inst_shape =
    List.filter_map (fun (port, expr) ->
      let w = expr_width caller_signal_widths expr in
      if w > 0 then Some (port, w) else None
    ) i.port_connections
    |> List.sort compare
  in
  (* Score each candidate by # ports whose width matches. *)
  let scored = List.map (fun (m : bmodule) ->
    let shape = port_shape m in
    let common = List.filter (fun (p, w) ->
      try List.assoc p shape = w with Not_found -> false
    ) inst_shape in
    (List.length common, m)
  ) candidates in
  match List.sort (fun (a, _) (b, _) -> compare b a) scored with
  | (n, m) :: _ when n > 0 -> Some m
  | _ ->
      (* No port matched; if there's exactly one candidate, use it. *)
      (match candidates with [m] -> Some m | _ -> None)

(* Rename every BVar/BAssign reference in an expr or stmt that
 * touches an internal child-signal name — prefix with `pfx ^ "."`
 * to namespace it under the instance label. *)
let prefix_internal pfx internals expr =
  let is_internal n = List.mem n internals in
  let rec map_e = function
    | BVar n when is_internal n -> BVar (pfx ^ "." ^ n)
    | BVar _ as e -> e
    | BConst _ as e -> e
    | BBinOp { op; lhs; rhs; result_type } ->
        BBinOp { op; lhs = map_e lhs; rhs = map_e rhs; result_type }
    | BUnOp { op; operand; result_type } ->
        BUnOp { op; operand = map_e operand; result_type }
    | BCond { condition; then_val; else_val } ->
        BCond { condition = map_e condition;
                then_val = map_e then_val;
                else_val = map_e else_val }
    | BConcat es -> BConcat (List.map map_e es)
    | BReplicate { count; value } -> BReplicate { count; value = map_e value }
    | BSelect { array; index } -> BSelect { array = map_e array; index = map_e index }
    | BSlice { signal; msb; lsb } -> BSlice { signal = map_e signal; msb; lsb }
    | BCall { func; args } -> BCall { func; args = List.map map_e args }
  in map_e expr

let prefix_internal_stmt pfx internals stmt =
  let mp = prefix_internal pfx internals in
  let rec map_s = function
    | BAssign { lhs; rhs } ->
        let lhs' = if List.mem lhs internals then pfx ^ "." ^ lhs else lhs in
        BAssign { lhs = lhs'; rhs = mp rhs }
    | BIf { condition; then_stmts; else_stmts } ->
        BIf { condition = mp condition;
              then_stmts = List.map map_s then_stmts;
              else_stmts = List.map map_s else_stmts }
    | BCase { selector; cases; default } ->
        BCase { selector = mp selector;
                cases = List.map (fun (e, ss) ->
                          (mp e, List.map map_s ss)) cases;
                default = List.map map_s default }
    | BBlock ss -> BBlock (List.map map_s ss)
    | BCallStmt { func; args } -> BCallStmt { func; args = List.map mp args }
    | BReturn (Some e) -> BReturn (Some (mp e))
    | other -> other
  in map_s stmt

(* Substitute every BVar/BAssign reference to an INPUT-port name
 * with the corresponding caller-supplied expression. Symmetrically
 * for OUTPUT ports — caller-side reads of `inst.out_signal` (which
 * we encoded via BVar with the inst name) should map to the
 * child's output signal post-rename. *)
let substitute_port_inputs port_subst expr =
  let rec map_e = function
    | BVar n as e ->
        (try List.assoc n port_subst with Not_found -> e)
    | BConst _ as e -> e
    | BBinOp { op; lhs; rhs; result_type } ->
        BBinOp { op; lhs = map_e lhs; rhs = map_e rhs; result_type }
    | BUnOp { op; operand; result_type } ->
        BUnOp { op; operand = map_e operand; result_type }
    | BCond { condition; then_val; else_val } ->
        BCond { condition = map_e condition;
                then_val = map_e then_val;
                else_val = map_e else_val }
    | BConcat es -> BConcat (List.map map_e es)
    | BReplicate { count; value } -> BReplicate { count; value = map_e value }
    | BSelect { array; index } -> BSelect { array = map_e array; index = map_e index }
    | BSlice { signal; msb; lsb } -> BSlice { signal = map_e signal; msb; lsb }
    | BCall { func; args } -> BCall { func; args = List.map map_e args }
  in map_e expr

let substitute_port_inputs_stmt port_subst stmt =
  let mp = substitute_port_inputs port_subst in
  (* LHS rewrite: when port_subst maps a name to BVar n', the rewrite
   * for an assignment to that name becomes an assignment to n'.
   * For non-BVar substitutions (e.g. expression-shaped input ports
   * being assigned in the child — should never happen in well-formed
   * SV) we leave the LHS as-is. *)
  let lhs_rename name =
    match List.assoc_opt name port_subst with
    | Some (BVar n') -> n'
    | _ -> name
  in
  let rec map_s = function
    | BAssign { lhs; rhs } ->
        BAssign { lhs = lhs_rename lhs; rhs = mp rhs }
    | BIf { condition; then_stmts; else_stmts } ->
        BIf { condition = mp condition;
              then_stmts = List.map map_s then_stmts;
              else_stmts = List.map map_s else_stmts }
    | BCase { selector; cases; default } ->
        BCase { selector = mp selector;
                cases = List.map (fun (e, ss) ->
                          (mp e, List.map map_s ss)) cases;
                default = List.map map_s default }
    | BBlock ss -> BBlock (List.map map_s ss)
    | BCallStmt { func; args } -> BCallStmt { func; args = List.map mp args }
    | BReturn (Some e) -> BReturn (Some (mp e))
    | other -> other
  in map_s stmt

(* Inline one combinational instance into a parent. Returns updated
 * parent (signals + processes augmented, instance removed) when
 * inlinable, otherwise the parent unchanged. *)
let inline_instance ~child (parent : bmodule) (i : binstance) : bmodule =
  let has_ff = List.exists (function BSequential _ -> true | _ -> false)
                 child.processes in
  if has_ff then parent  (* skip FF-bearing children for now *)
  else begin
    (* Classify child signals into ports vs internals. *)
    let inputs =
      List.filter_map (fun (s : bsignal) ->
        if s.direction = `Input then Some s.name else None) child.signals in
    let outputs =
      List.filter_map (fun (s : bsignal) ->
        if s.direction = `Output then Some s.name else None) child.signals in
    let internals =
      List.filter_map (fun (s : bsignal) ->
        if s.direction = `Internal then Some s.name else None) child.signals in
    (* Build port substitution: input port name → caller-supplied expr.
     * For outputs we also map them via the renaming so the parent's
     * later read of the inst.out_signal sees the right wire. *)
    let port_subst =
      List.filter_map (fun (port, expr) ->
        if List.mem port inputs then Some (port, expr)
        else None
      ) i.port_connections
    in
    (* Outputs get a unique parent-side name — the caller's
     * connected expression's BVar (if any) becomes the alias. *)
    let output_aliases =
      List.filter_map (fun (port, expr) ->
        if List.mem port outputs then
          (match expr with
           | BVar caller_var ->
               Some (port, caller_var)
           | _ -> None)
        else None
      ) i.port_connections
    in
    let renamed_internal =
      List.map (fun n -> (n, i.inst_name ^ "." ^ n)) internals in
    (* Compose the rename maps: input-port reads inside child →
     * caller expr; output-port writes inside child → caller's
     * connected wire; internals → instance-prefixed. *)
    let all_subst =
      port_subst
      @ List.map (fun (k, v) -> (k, BVar v)) output_aliases
      @ List.map (fun (k, v) -> (k, BVar v)) renamed_internal
    in
    let rewrite_stmt = substitute_port_inputs_stmt all_subst in
    let rewrite_proc = function
      | BCombinational c ->
          BCombinational { c with body = List.map rewrite_stmt c.body;
                                  name = i.inst_name ^ "." ^ c.name }
      | BSequential s ->
          BSequential { s with body = List.map rewrite_stmt s.body;
                               name = i.inst_name ^ "." ^ s.name }
    in
    (* Bring child's internal signals into the parent (renamed). *)
    let new_signals =
      List.filter_map (fun (s : bsignal) ->
        if s.direction = `Internal then
          Some { s with name = i.inst_name ^ "." ^ s.name }
        else None
      ) child.signals
    in
    let _ = prefix_internal in
    let _ = prefix_internal_stmt in
    { parent with
      signals = parent.signals @ new_signals;
      processes = parent.processes @ List.map rewrite_proc child.processes;
      instances = List.filter (fun x -> x.inst_name <> i.inst_name) parent.instances;
    }
  end

(* Flatten a module: pick the right specialised child for each
 * instance, inline if combinational. Repeat until no further
 * inlining happens or depth limit hit. *)
let flatten_module ~by_base ~by_name (parent : bmodule) : bmodule =
  let signal_widths = List.map (fun (s : bsignal) ->
    let w = match s.stype with
      | BInt { width; _ } -> width
      | BArray { size; element = BInt { width; _ }; _ } -> size * width
      | _ -> 0
    in (s.name, w)
  ) parent.signals in
  let rec loop depth p =
    if depth >= max_depth then p
    else begin
      let progress = ref false in
      let p' = List.fold_left (fun acc i ->
        let candidates =
          (* Exact name match first, then base-name match. *)
          (match List.assoc_opt i.module_name by_name with
           | Some m -> [m] | None ->
               (try List.assoc i.module_name by_base
                with Not_found -> []))
        in
        match pick_specialised ~caller_signal_widths:signal_widths
                ~candidates i with
        | None -> acc
        | Some child ->
            let acc' = inline_instance ~child acc i in
            if List.length acc'.instances < List.length acc.instances then
              progress := true;
            acc'
      ) p p.instances in
      if !progress then loop (depth + 1) p' else p'
    end
  in
  loop 0 parent

let flatten_program (p : bprogram) : bprogram =
  let debug = Sys.getenv_opt "FLAT_DEBUG" <> None in
  let by_name = List.map (fun (m : bmodule) -> (m.name, m)) p.modules in
  if debug then begin
    Printf.eprintf "[flat] %d modules, instances per module:\n%!" (List.length p.modules);
    List.iter (fun (m : bmodule) ->
      if m.instances <> [] then
        Printf.eprintf "  %s has %d instances: %s\n%!" m.name
          (List.length m.instances)
          (String.concat ", "
            (List.map (fun i -> i.inst_name ^ "→" ^ i.module_name)
                      m.instances))
    ) p.modules
  end;
  (* Group siblings sharing a base name (everything before "__"). *)
  let base_of n =
    try
      let i = Str.search_forward (Str.regexp "__") n 0 in
      String.sub n 0 i
    with Not_found -> n
  in
  let by_base = List.fold_left (fun acc (m : bmodule) ->
    let b = base_of m.name in
    let bucket = try List.assoc b acc with Not_found -> [] in
    (b, m :: bucket) :: List.remove_assoc b acc
  ) [] p.modules in
  let result = { p with modules = List.map (flatten_module ~by_base ~by_name) p.modules } in
  if debug then begin
    Printf.eprintf "[flat] AFTER pass: %d modules, residual instances:\n%!"
      (List.length result.modules);
    List.iter (fun (m : bmodule) ->
      if m.instances <> [] then
        Printf.eprintf "  %s STILL has %d instances: %s\n%!" m.name
          (List.length m.instances)
          (String.concat ", "
            (List.map (fun i -> i.inst_name ^ "→" ^ i.module_name)
                      m.instances))
    ) result.modules
  end;
  result
