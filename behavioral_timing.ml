(* Analytical timing pass.
 *
 * Walks a flat BIR (post Behavioral_hier.flatten_for_z3) computing
 * per-signal arrival times via longest-path on the dataflow DAG.
 * Per-operator delay comes from a small analytical model; per-instance
 * delay comes from the architecture attribute (sv_decomp_adder /
 * sv_decomp_mul) on the target module, falling back to a default
 * architecture per op when no attribute is present.
 *
 * The output is a slack report: for each path that exceeds a target
 * depth, list the operators along it. Combined with the cert-gated
 * substitution pass (Behavioral_arch_subst), this drives an
 * optimisation loop where each transformation is provably equivalent.
 *
 * Units: "logic levels" (gate stages). Multiply by τ to get seconds.
 * No physical placement here — that's the LEF/DEF accuracy boost. *)

open Behavioral_ir

(* ──────────────────────────────────────────────────────────────────
 * Per-architecture analytical depth model.
 *
 * All numbers are gate-equivalent depth (NAND-2 stages). They match
 * the textbook complexity of each architecture. Constants chosen for
 * the asymptotic shape; per-tech calibration goes through Liberty
 * cell_rise LUTs once the parser knows them. *)

let log2_ceil w =
  let rec aux n p = if p >= w then n else aux (n + 1) (p * 2) in
  if w <= 1 then 0 else aux 0 1

let adder_depth ~arch ~width =
  match arch with
  | "ripple"      -> width
  | "sklansky"    -> log2_ceil width
  | "brent_kung"  -> max 1 (2 * log2_ceil width - 1)
  | "kogge_stone" -> log2_ceil width
  | _ ->
      (* Unknown arch: be conservative, treat as ripple. *)
      width

let mul_depth ~arch ~width =
  match arch with
  | "ripple"  -> width * 2                     (* shift-add *)
  | "wallace" -> 2 * log2_ceil width + log2_ceil (2 * width)
  | "dadda"   -> 2 * log2_ceil width + log2_ceil (2 * width)
  | _ -> width * 2

(* Default architecture per op when no attribute is set. Conservative
 * (slowest reasonable) so unannotated designs don't accidentally
 * benefit from architectures they didn't ask for. *)
let default_adder_arch = "ripple"
let default_mul_arch   = "ripple"

(* Map a BIR operator to its analytical delay given operand width.
 * Comparison / shift / boolean ops use coarse constants; the path
 * analyser cares mostly about adders and multipliers. *)
let op_depth ~arch_adder ~arch_mul = function
  | BAdd | BSub -> fun w -> adder_depth ~arch:arch_adder ~width:w
  | BMul        -> fun w -> mul_depth   ~arch:arch_mul   ~width:w
  | BDiv | BMod -> fun w -> w * w                 (* coarse — division is expensive *)
  | BAnd | BOr  | BXor -> fun _ -> 1
  | BShl | BShr | BAshr -> fun w -> log2_ceil w   (* barrel shifter *)
  | BEq  | BNe         -> fun w -> log2_ceil w + 1   (* tree compare + reduce *)
  | BLt  | BLe | BGt | BGe ->
      fun w -> adder_depth ~arch:arch_adder ~width:w (* subtract + sign *)

let unop_depth op w =
  match op with
  | BNot | BNeg -> 1
  | BRedAnd | BRedOr | BRedXor -> log2_ceil w

(* Width of a bexpr in the parent module's signal context. Same idea
 * as Z3_miter.width_of_expr_ctx but simpler since we only need a
 * positive integer; falls back to 32 when the expression is opaque. *)
let rec width_of widths = function
  | BVar n ->
      (try Hashtbl.find widths n with Not_found -> 32)
  | BConst { width; _ } -> width
  | BBinOp { op = (BEq|BNe|BLt|BLe|BGt|BGe); _ } -> 1
  | BBinOp { lhs; rhs; _ } -> max (width_of widths lhs) (width_of widths rhs)
  | BUnOp { op = (BRedAnd|BRedOr|BRedXor); _ } -> 1
  | BUnOp { operand; _ } -> width_of widths operand
  | BSelect _ -> 1
  | BSlice { msb; lsb; _ } -> abs (msb - lsb) + 1
  | BConcat es -> List.fold_left (fun a e -> a + width_of widths e) 0 es
  | BReplicate { count; value } -> count * width_of widths value
  | BCond { then_val; _ } -> width_of widths then_val
  | BCall _ -> 32

(* ──────────────────────────────────────────────────────────────────
 * Critical-path walker.
 *
 * Build a per-signal arrival-time table by topologically processing
 * BAssign statements: arrival(lhs) = max over RHS leaves of
 * arrival(leaf) + delay(operator chain to leaf).
 *
 * Inputs and FF outputs (post-ffrip these are <Q>__Q) anchor at 0.
 * BSequential bodies anchor their LHS at 0 too — Behavioral_ffrip
 * normalises this for us when invoked via the miter, but for a
 * standalone timing pass we treat any BSequential lhs as a "register
 * boundary" with arrival 0. *)

type op_record = {
  op_name : string;       (* e.g. "BAdd width=32 arch=brent_kung" *)
  op_depth : int;
}

type arrival_node = {
  signal : string;
  arrival : int;
  driver_ops : op_record list;
  source : arrival_source;
}
and arrival_source =
  | InputPort
  | FfBoundary
  | DerivedFrom of string list      (* upstream signals *)

let signal_widths_of (m : bmodule) : (string, int) Hashtbl.t =
  let h = Hashtbl.create 64 in
  List.iter (fun (s : bsignal) ->
    let w = match s.stype with
      | BInt { width; _ } -> width
      | BArray { size; element = BInt { width; _ }; _ } -> size * width
      | _ -> 1 in
    Hashtbl.replace h s.name w
  ) m.signals;
  h

(* Per-signal architecture lookup: read the attribute from the
 * destination signal (the one being assigned) so a path's local
 * choice of architecture matters. Falls back to a module-wide
 * default if the signal carries no `sv_decomp_*` attr. *)
let signal_attrs_of (m : bmodule) : (string, (string * string) list) Hashtbl.t =
  let h = Hashtbl.create 64 in
  List.iter (fun (s : bsignal) ->
    if s.attrs <> [] then Hashtbl.replace h s.name s.attrs
  ) m.signals;
  h

let arch_for_signal sig_attrs default_arch lhs key =
  match Hashtbl.find_opt sig_attrs lhs with
  | Some attrs ->
      (try List.assoc key attrs with Not_found -> default_arch)
  | None -> default_arch

(* Enumerate (lhs, rhs) pairs from all combinational processes in a
 * flat module. BSequential lhses are emitted as "register boundary"
 * nodes (arrival 0) — the miter's FF-rip would already have hoisted
 * them, but for a standalone pre-encode walk we model them here. *)
let collect_assigns (m : bmodule) =
  let combs = ref [] in
  let regs  = ref [] in
  let rec walk_stmt env = function
    | BAssign { lhs; rhs } ->
        env := (lhs, rhs) :: !env
    | BIf { then_stmts; else_stmts; _ } ->
        List.iter (walk_stmt env) then_stmts;
        List.iter (walk_stmt env) else_stmts
    | BCase { cases; default; _ } ->
        List.iter (fun (_, body) ->
          List.iter (walk_stmt env) body) cases;
        List.iter (walk_stmt env) default
    | BWhile { body; _ } | BFor { body; _ } | BBlock body ->
        List.iter (walk_stmt env) body
    | _ -> ()
  in
  List.iter (function
    | BCombinational { body; _ } ->
        List.iter (walk_stmt combs) body
    | BSequential   { body; _ } ->
        List.iter (walk_stmt regs) body
  ) m.processes;
  (List.rev !combs, List.rev !regs)

(* Compute arrivals via a fixed-point iteration. Memoised over signal
 * names; combinational cycles cap at max_iters and emit a warning
 * (the BIR shouldn't have such cycles after ffrip but leaf cells
 * sometimes look like them via expand_program's BCond chain). *)
let compute_arrivals
    ?(arch_adder = default_adder_arch)
    ?(arch_mul   = default_mul_arch)
    (m : bmodule) : (string, arrival_node) Hashtbl.t =
  let widths   = signal_widths_of m in
  let sig_attrs = signal_attrs_of m in
  let arrivals : (string, arrival_node) Hashtbl.t = Hashtbl.create 64 in
  let combs, regs = collect_assigns m in
  let comb_drivers : (string, bexpr) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun (lhs, rhs) ->
    Hashtbl.replace comb_drivers lhs rhs
  ) combs;
  let reg_lhs = List.map fst regs in
  let input_names = List.filter_map (fun (s : bsignal) ->
    match s.direction with `Input -> Some s.name | _ -> None
  ) m.signals in
  (* Anchor inputs and FF boundaries at arrival 0. *)
  List.iter (fun n ->
    Hashtbl.replace arrivals n
      { signal = n; arrival = 0; driver_ops = [];
        source = InputPort }
  ) input_names;
  List.iter (fun n ->
    Hashtbl.replace arrivals n
      { signal = n; arrival = 0; driver_ops = [];
        source = FfBoundary }
  ) reg_lhs;
  (* Recursive arrival of a bexpr. Memoise per BVar lookup. Operator
   * delay adds to the max of operand arrivals. The `lhs_signal`
   * argument carries the destination signal name (when known) so
   * BBinOp / BUnOp can read per-signal `sv_decomp_*` attrs. *)
  let rec arr_of_expr ?lhs_signal in_progress = function
    | BConst _ -> (0, [])
    | BVar n ->
        if List.mem n in_progress then (0, [])    (* cycle break *)
        else begin
          (match Hashtbl.find_opt arrivals n with
           | Some a -> (a.arrival, [])
           | None ->
               match Hashtbl.find_opt comb_drivers n with
               | None ->
                   Hashtbl.replace arrivals n
                     { signal = n; arrival = 0; driver_ops = [];
                       source = InputPort };
                   (0, [])
               | Some rhs ->
                   let (a, ops) =
                     arr_of_expr ~lhs_signal:n
                       (n :: in_progress) rhs in
                   Hashtbl.replace arrivals n
                     { signal = n; arrival = a;
                       driver_ops = ops;
                       source = DerivedFrom (collect_vars rhs) };
                   (a, ops))
        end
    | BBinOp { op; lhs; rhs; _ } ->
        let (la, lops) = arr_of_expr ?lhs_signal in_progress lhs in
        let (ra, rops) = arr_of_expr ?lhs_signal in_progress rhs in
        let w = max (width_of widths lhs) (width_of widths rhs) in
        (* Per-signal architecture lookup: a wire annotated
         * `(* sv_decomp_adder = "brent_kung" *)` uses brent_kung
         * depth, regardless of the module-wide default. *)
        let local_adder = match lhs_signal with
          | Some n -> arch_for_signal sig_attrs arch_adder n "sv_decomp_adder"
          | None -> arch_adder in
        let local_mul = match lhs_signal with
          | Some n -> arch_for_signal sig_attrs arch_mul n "sv_decomp_mul"
          | None -> arch_mul in
        let d = op_depth ~arch_adder:local_adder ~arch_mul:local_mul op w in
        let arch_tag = match op with
          | BAdd | BSub -> local_adder
          | BMul        -> local_mul
          | _ -> "-" in
        let oprec = {
          op_name = Printf.sprintf "%s w=%d arch=%s"
                      (string_of_binop op) w arch_tag;
          op_depth = d;
        } in
        (max la ra + d, oprec :: (lops @ rops))
    | BUnOp { op; operand; _ } ->
        let (oa, oops) = arr_of_expr in_progress operand in
        let w = width_of widths operand in
        let d = unop_depth op w in
        let oprec = {
          op_name = Printf.sprintf "%s w=%d" (string_of_unop op) w;
          op_depth = d;
        } in
        (oa + d, oprec :: oops)
    | BCond { condition; then_val; else_val } ->
        let (ca, cops) = arr_of_expr in_progress condition in
        let (ta, tops) = arr_of_expr in_progress then_val in
        let (ea, eops) = arr_of_expr in_progress else_val in
        (max ca (max ta ea) + 1,
         { op_name = "mux"; op_depth = 1 } :: (cops @ tops @ eops))
    | BSelect { array; index } ->
        let (aa, aops) = arr_of_expr in_progress array in
        let (ia, iops) = arr_of_expr in_progress index in
        (max aa ia + 1, { op_name = "sel"; op_depth = 1 }
                       :: (aops @ iops))
    | BSlice { signal; _ } -> arr_of_expr in_progress signal
    | BConcat es ->
        let arrs_ops = List.map (arr_of_expr in_progress) es in
        let max_a = List.fold_left max 0 (List.map fst arrs_ops) in
        let all_ops = List.concat_map snd arrs_ops in
        (max_a, all_ops)
    | BReplicate { value; _ } -> arr_of_expr in_progress value
    | BCall _ -> (0, [])

  and collect_vars expr =
    let acc = ref [] in
    let rec go = function
      | BVar n -> acc := n :: !acc
      | BConst _ -> ()
      | BBinOp { lhs; rhs; _ } -> go lhs; go rhs
      | BUnOp { operand; _ } -> go operand
      | BCond { condition; then_val; else_val } ->
          go condition; go then_val; go else_val
      | BSelect { array; index } -> go array; go index
      | BSlice { signal; _ } -> go signal
      | BConcat es -> List.iter go es
      | BReplicate { value; _ } -> go value
      | BCall { args; _ } -> List.iter go args
    in
    go expr; !acc
  in
  (* Force evaluation of every comb LHS so the table is populated.
   * Passing [] as in_progress (not [n]) — the cycle-break marker is
   * added by arr_of_expr itself when it descends into the LHS's
   * driver. *)
  List.iter (fun (n, _) ->
    if not (Hashtbl.mem arrivals n) then
      ignore (arr_of_expr [] (BVar n))
  ) combs;
  (* Also evaluate FF inputs (the rhs of register-driving assigns) so
   * we know how much delay each register sees on its D pin. *)
  List.iter (fun (q, rhs) ->
    let (a, ops) = arr_of_expr [] rhs in
    Hashtbl.replace arrivals (q ^ "__D")
      { signal = q ^ "__D"; arrival = a;
        driver_ops = ops;
        source = DerivedFrom (collect_vars rhs) }
  ) regs;
  arrivals

(* ──────────────────────────────────────────────────────────────────
 * Reporting *)

type path_report = {
  endpoint    : string;
  arrival     : int;
  ops         : op_record list;
  source      : arrival_source;
}

let endpoint_paths arrivals m =
  let outputs = List.filter_map (fun (s : bsignal) ->
    match s.direction with `Output -> Some s.name | _ -> None
  ) m.signals in
  let ff_d_names =
    Hashtbl.fold (fun k _ acc ->
      let n = String.length k in
      if n > 3 && String.sub k (n - 3) 3 = "__D" then k :: acc
      else acc
    ) arrivals [] in
  let pick name =
    match Hashtbl.find_opt arrivals name with
    | Some (a : arrival_node) ->
        Some {
          endpoint = name;
          arrival  = a.arrival;
          ops      = a.driver_ops;
          source   = a.source;
        }
    | None -> None
  in
  let out_paths = List.filter_map pick outputs in
  let ff_paths  = List.filter_map pick ff_d_names in
  out_paths @ ff_paths
  |> List.sort (fun a b -> compare b.arrival a.arrival)

let format_path p =
  let ops_summary =
    if p.ops = [] then "(direct)"
    else
      String.concat " → " (List.map (fun o ->
        Printf.sprintf "%s[%d]" o.op_name o.op_depth) p.ops)
  in
  Printf.sprintf "  %-40s arrival=%d  %s"
    p.endpoint p.arrival ops_summary

let report ?(target_depth = max_int) (paths : path_report list) =
  let buf = Buffer.create 1024 in
  let n_paths = List.length paths in
  Buffer.add_string buf
    (Printf.sprintf "═══════════════════════════════════════════════════════\n");
  Buffer.add_string buf
    (Printf.sprintf "  Critical-path report (%d endpoints)\n" n_paths);
  Buffer.add_string buf
    (Printf.sprintf "  Target depth: %s\n"
       (if target_depth = max_int then "—"
        else string_of_int target_depth));
  Buffer.add_string buf
    "═══════════════════════════════════════════════════════\n\n";
  let pass = List.filter (fun p -> p.arrival <= target_depth) paths in
  let fail = List.filter (fun p -> p.arrival >  target_depth) paths in
  if fail <> [] then begin
    Buffer.add_string buf
      (Printf.sprintf "Negative-slack paths (%d):\n" (List.length fail));
    List.iter (fun p ->
      Buffer.add_string buf (format_path p);
      Buffer.add_char buf '\n'
    ) fail;
    Buffer.add_char buf '\n'
  end;
  if pass <> [] then begin
    let top = List.filteri (fun i _ -> i < 10) pass in
    Buffer.add_string buf
      (Printf.sprintf "Top %d paths within budget:\n" (List.length top));
    List.iter (fun p ->
      Buffer.add_string buf (format_path p);
      Buffer.add_char buf '\n'
    ) top
  end;
  Buffer.contents buf

(* ──────────────────────────────────────────────────────────────────
 * Cert-gated optimisation loop.
 *
 * For each negative-slack path, walk its operator list looking for
 * BAdd/BMul stages whose width has a faster certified architecture.
 * Returns a list of suggested upgrades (signal_name × kind × arch ×
 * width). The caller can apply them by mutating the source SV's
 * `(* sv_decomp_<kind> *)` attributes (or by swapping in
 * `emit-arch`-produced modules at instance sites). *)

type upgrade_suggestion = {
  endpoint   : string;
  kind       : string;       (* "adder" / "mul" *)
  from_arch  : string;
  to_arch    : string;
  op_width   : int;
  delay_drop : int;          (* old depth − new depth *)
}

(* Architecture-rank lookup: for a given kind+width, return the
 * fastest certified arch (via $HOME/.cache/sv_suite/arch). *)
let cert_dir () =
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  home ^ "/.cache/sv_suite/arch"

let cert_exists ~kind ~arch ~width =
  let p = Printf.sprintf "%s/%s_%s_%d.proven"
            (cert_dir ()) kind arch width in
  Sys.file_exists p

let archs_for_kind = function
  | "adder" -> ["ripple"; "sklansky"; "brent_kung"; "kogge_stone"]
  | "mul"   -> ["ripple"; "wallace"; "dadda"]
  | _ -> []

let depth_for ~kind ~arch ~width =
  match kind with
  | "adder" -> adder_depth ~arch ~width
  | "mul"   -> mul_depth   ~arch ~width
  | _ -> 0

let fastest_certified ~kind ~width ~current_arch =
  let cur_d = depth_for ~kind ~arch:current_arch ~width in
  let candidates = archs_for_kind kind |> List.filter_map (fun a ->
    if a = current_arch then None
    else if not (cert_exists ~kind ~arch:a ~width) then None
    else
      let d = depth_for ~kind ~arch:a ~width in
      if d < cur_d then Some (d, a) else None
  ) in
  match List.sort compare candidates with
  | (d, a) :: _ -> Some (a, cur_d - d)
  | [] -> None

let suggest_upgrades
    ?(arch_adder = default_adder_arch)
    ?(arch_mul   = default_mul_arch)
    (paths : path_report list) : upgrade_suggestion list =
  List.concat_map (fun (p : path_report) ->
    List.filter_map (fun (op : op_record) ->
      (* op_name shape: `<binop_str> w=<W> arch=<A>`. *)
      let m = Str.regexp
        "^\\(.\\)\\(.*\\) w=\\([0-9]+\\) arch=\\([a-z_]+\\)$" in
      if not (Str.string_match m op.op_name 0) then None
      else
        let op_str = Str.matched_group 1 op.op_name
                   ^ Str.matched_group 2 op.op_name in
        let w = int_of_string (Str.matched_group 3 op.op_name) in
        let local_arch = Str.matched_group 4 op.op_name in
        let kind = match op_str with
          | "+" | "-" -> Some "adder"
          | "*"       -> Some "mul"
          | _         -> None in
        match kind with
        | None -> None
        | Some k ->
            match fastest_certified ~kind:k ~width:w
                    ~current_arch:local_arch with
            | None -> None
            | Some (to_arch, delta) ->
                Some {
                  endpoint = p.endpoint;
                  kind = k;
                  from_arch = local_arch;
                  to_arch;
                  op_width = w;
                  delay_drop = delta;
                }
    ) p.ops
  ) paths

let format_upgrades (us : upgrade_suggestion list) =
  if us = [] then "No certified upgrades available.\n"
  else
    let buf = Buffer.create 256 in
    Buffer.add_string buf "Suggested cert-gated upgrades:\n";
    List.iter (fun u ->
      Buffer.add_string buf
        (Printf.sprintf
           "  %-40s %s/%d : %s → %s  (-%d levels)\n"
           u.endpoint u.kind u.op_width u.from_arch u.to_arch u.delay_drop)
    ) us;
    Buffer.contents buf
