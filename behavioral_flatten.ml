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

(* Cap on the flatten loop iteration count AND on the recursion depth
 * encoded in migrated instance names (number of "." separators in
 * inst_name). The motivating worst case is popcount with IW256 which
 * needs 8 levels of recursion (256→128→…→1). 10 gives slack while
 * keeping per-module instance count bounded at 2^10 = 1024 — large but
 * not astronomical. Without this cap, a module with combinational
 * children that themselves have combinational children doubles its
 * instance count each pass, and 2^16 (the previous default) materialises
 * 65K migrated instances per parent on cva6, hanging the run. *)
let max_depth = 10

(* Separator for the hierarchical names flattening builds (`soc$memory$doa`).
   '$' rather than '.' because '$' is a legal character inside a Verilog
   identifier and '.' is not: with a dot, every flattened net has to be emitted
   as an escaped identifier (`\soc.memory.doa `), and any tool downstream that
   does not handle escaped identifiers -- or that treats the dot as a hierarchy
   reference -- gets it wrong.  With '$' the name is an ordinary identifier
   everywhere.  Note [inst_dot_depth] still counts separators for the recursion
   cap, so it counts this one. *)
let hier_sep = '$'

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
 * touches an internal child-signal name — prefix with `pfx ^ hier_sep`
 * to namespace it under the instance label. *)
let prefix_internal pfx internals expr =
  let is_internal n = List.mem n internals in
  let rec map_e = function
    | BVar n when is_internal n -> BVar (pfx ^ String.make 1 hier_sep ^ n)
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
        let lhs' = if List.mem lhs internals then pfx ^ String.make 1 hier_sep ^ lhs else lhs in
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
let inline_instance ?(force_ff=false) ~child (parent : bmodule) (i : binstance) : bmodule =
  let has_ff = List.exists (function BSequential _ -> true | _ -> false)
                 child.processes in
  if has_ff && not force_ff then parent  (* skip FF-bearing children unless forced *)
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
     * connected expression's BVar (if any) becomes the alias. We
     * also accept BSlice(BVar) and pull the bare variable: Vivado's
     * port_maps often look like `popcount_o(1 downto 0) =>
     * left_child_result(1 downto 0)`, where the slice covers the
     * whole receiving signal anyway. Without this, the inlined output
     * assignment writes to the un-renamed `popcount_o`, polluting
     * the parent's namespace and leaving the caller's wire free. *)
    let output_aliases =
      List.filter_map (fun (port, expr) ->
        if List.mem port outputs then
          (match expr with
           | BVar caller_var ->
               Some (port, caller_var)
           | BSlice { signal = BVar caller_var; _ } ->
               Some (port, caller_var)
           | _ -> None)
        else None
      ) i.port_connections
    in
    (* A child port the instance does NOT connect -- an unused output like
       picorv32's pcpi_rs2 when the co-processor interface is off, or one bound
       to an expression we cannot alias -- is covered by neither substitution
       above, so the inlined processes go on writing the bare CHILD port name
       into a parent that never declares it.  Verilog's implicit nets then make
       it a 1-bit wire and a 32-bit value is silently truncated; SystemVerilog
       just refuses the file.  Namespace them like internals, which both
       declares them and keeps two instances of the same child apart. *)
    let unbound_ports =
      List.filter
        (fun p ->
          (List.mem p outputs || List.mem p inputs)
          && not (List.mem_assoc p output_aliases)
          && not (List.mem_assoc p port_subst))
        (inputs @ outputs)
    in
    let renamed_internal =
      List.map (fun n -> (n, i.inst_name ^ String.make 1 hier_sep ^ n))
        (internals @ unbound_ports) in
    (* Compose the rename maps: input-port reads inside child →
     * caller expr; output-port writes inside child → caller's
     * connected wire; internals → instance-prefixed. *)
    let all_subst =
      port_subst
      @ List.map (fun (k, v) -> (k, BVar v)) output_aliases
      @ List.map (fun (k, v) -> (k, BVar v)) renamed_internal
    in
    let rewrite_stmt = substitute_port_inputs_stmt all_subst in
    (* A BSequential's clock and reset are NAME FIELDS, not expressions in the
       body, so the body rewrite above does not touch them.  Left alone, an
       inlined register keeps the CHILD's name for its clock -- `clk` when the
       parent's net is `clk_sys` -- and since only the child's INTERNAL signals
       are brought into the parent, that name now refers to nothing at all.
       The register is silently unclocked, and any later analysis reads it as
       an extra clock domain that does not exist.  Map both through the same
       port substitution as the body. *)
    let rewrite_name n =
      match List.assoc_opt n all_subst with
      | Some (BVar v) -> v
      | _ -> n
    in
    let rewrite_proc = function
      | BCombinational c ->
          BCombinational { c with body = List.map rewrite_stmt c.body;
                                  name = i.inst_name ^ String.make 1 hier_sep ^ c.name }
      | BSequential s ->
          BSequential { s with body = List.map rewrite_stmt s.body;
                               name = i.inst_name ^ String.make 1 hier_sep ^ s.name;
                               clock = rewrite_name s.clock;
                               reset = Option.map rewrite_name s.reset;
                               (* in-cycle blocking temps are child names too *)
                               blocking_vars = List.map rewrite_name s.blocking_vars }
    in
    (* Bring child's internal signals into the parent (renamed). *)
    let new_signals =
      List.filter_map (fun (s : bsignal) ->
        (* Unbound ports come across too, as internals, carrying the child's
           DECLARED WIDTH -- which is the whole point: the parent has no other
           source for it. *)
        if s.direction = `Internal || List.mem s.name unbound_ports then
          Some { s with name = i.inst_name ^ String.make 1 hier_sep ^ s.name;
                        direction = `Internal }
        else None
      ) child.signals
    in
    let _ = prefix_internal in
    let _ = prefix_internal_stmt in
    (* Migrate the child's own instances into the parent, with the
     * inst_name prefixed so they don't collide with the parent's
     * instances. The flatten loop will then pick them up next iteration
     * and recurse. Without this, multi-level recursive inlining (e.g.
     * popcount__IW16 → IW8 → IW4 → IW2) loses everything past the
     * first level — the IW8's own IW4 instances vanish.
     *
     * Depth cap: count "." separators in the prospective new inst_name.
     * Verible's specialise_design only emits a few discrete sibling
     * versions; if the pick_specialised fallback ever maps an instance
     * back to a child with the same module_name as its caller (because
     * port-shape disambiguation tied), we'd recurse forever. Cap the
     * migration name depth at `max_depth` (10) — covers popcount up to
     * IW1024 (log2=10) and bounds total per-parent instance count at
     * 2^10 = 1024. *)
    let inst_dot_depth s =
      let n = ref 0 in
      String.iter (fun c -> if c = hier_sep then incr n) s;
      !n
    in
    let migrated_instances =
      if inst_dot_depth i.inst_name >= max_depth then []
      else
        List.map (fun (ci : binstance) ->
          { ci with
            inst_name = i.inst_name ^ String.make 1 hier_sep ^ ci.inst_name;
            (* Rewrite the connected port-expressions: child's port
             * connections reference signals (input ports of the child,
             * which now map to the inlined-caller's connected exprs;
             * internal signals, now prefixed). The same all_subst
             * dictionary built above does both. *)
            port_connections = List.map (fun (p, e) ->
              (p, substitute_port_inputs all_subst e)
            ) ci.port_connections;
          }
        ) child.instances
    in
    { parent with
      signals = parent.signals @ new_signals;
      processes = parent.processes @ List.map rewrite_proc child.processes;
      instances =
        List.filter (fun x -> x.inst_name <> i.inst_name) parent.instances
        @ migrated_instances;
    }
  end

(* Flatten a module: pick the right specialised child for each
 * instance, inline if combinational. Repeat until no further
 * inlining happens or depth limit hit. With migration enabled in
 * inline_instance, each pass may add migrated grandchild instances
 * — those get processed on subsequent passes. Bounded by max_depth
 * which caps recursive specialisations like popcount__IW16→IW1. *)
let flatten_module ?(force_ff=false) ~by_base ~by_name (parent : bmodule) : bmodule =
  let debug = Sys.getenv_opt "FLAT_DEBUG" <> None in
  let signal_widths = List.map (fun (s : bsignal) ->
    let w = match s.stype with
      | BInt { width; _ } -> width
      | BArray { size; element = BInt { width; _ }; _ } -> size * width
      | _ -> 0
    in (s.name, w)
  ) parent.signals in
  let rec loop depth p =
    if debug then
      Printf.eprintf "[flat] %s depth=%d insts=%d sigs=%d procs=%d\n%!"
        parent.name depth
        (List.length p.instances) (List.length p.signals) (List.length p.processes);
    if depth >= max_depth then p
    else begin
      let inlined_any = ref false in
      let p' = List.fold_left (fun acc i ->
        let candidates =
          (match List.assoc_opt i.module_name by_name with
           | Some m -> [m] | None ->
               (try List.assoc i.module_name by_base
                with Not_found -> []))
        in
        match pick_specialised ~caller_signal_widths:signal_widths
                ~candidates i with
        | None -> acc
        | Some child ->
            (* Was this instance still around (acc may have lost it
             * via earlier folds), and did inline_instance remove it?
             * Use membership, not length — instances list grows when
             * a non-empty child.instances gets migrated. *)
            let was_present =
              List.exists (fun x -> x.inst_name = i.inst_name) acc.instances in
            let acc' = inline_instance ~force_ff ~child acc i in
            let still_present =
              List.exists (fun x -> x.inst_name = i.inst_name) acc'.instances in
            if was_present && not still_present then
              inlined_any := true;
            acc'
      ) p p.instances in
      if !inlined_any then loop (depth + 1) p' else p'
    end
  in
  loop 0 parent

let flatten_program ?(force_ff=false) (p : bprogram) : bprogram =
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
  let result = { p with modules = List.map (flatten_module ~force_ff ~by_base ~by_name) p.modules } in
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
