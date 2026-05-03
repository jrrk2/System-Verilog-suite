(* Brain-dead semantic sanity checks on a converted Behavioral_ir.bmodule.
 *
 * The four front-ends in this project are PARSERS — they happily
 * accept SV that is syntactically valid but semantically wrong (e.g.
 * `assign v = 1; assign v = 2;` — multi-driver). To match what a
 * synthesiser like Vivado catches at elaboration we run a few
 * structural checks over the BIR after conversion. Anything that
 * trips a check is reported; downstream tooling decides whether to
 * treat it as a hard fail.
 *
 * Currently checks:
 *   - duplicate signal declarations (`reg v; wire v;`)
 *   - multiple continuous assigns to the same lhs
 *   - mixed continuous + procedural drive of the same lhs
 *
 * NOT yet checked (each is its own line item if needed):
 *   - undefined typedef references
 *   - void function returning a value
 *   - stream concat width mismatch
 *   - enum value type-checking *)

open Behavioral_ir

type sanity_error =
  | Duplicate_signal of string
  | Multiple_continuous_drivers of string * int
  | Mixed_proc_and_continuous of string

let string_of_error = function
  | Duplicate_signal n ->
      Printf.sprintf "signal %s redeclared" n
  | Multiple_continuous_drivers (n, k) ->
      Printf.sprintf "signal %s has %d continuous assigns (multi-driver)" n k
  | Mixed_proc_and_continuous n ->
      Printf.sprintf "signal %s driven by both `assign` and an `always` block" n

(* ─── Helpers ────────────────────────────────────────────────────── *)

(* Walk a stmt tree, collecting every `BAssign.lhs` it contains.
 * Used for both procedural (always) and continuous (BCombinational
 * named `assign_*`) bodies. *)
let rec collect_lhs_in_stmt = function
  | BAssign { lhs; _ } -> [lhs]
  | BIf { then_stmts; else_stmts; _ } ->
      List.concat_map collect_lhs_in_stmt then_stmts
      @ List.concat_map collect_lhs_in_stmt else_stmts
  | BCase { cases; default; _ } ->
      List.concat_map (fun (_, ss) ->
        List.concat_map collect_lhs_in_stmt ss) cases
      @ List.concat_map collect_lhs_in_stmt default
  | BBlock ss -> List.concat_map collect_lhs_in_stmt ss
  | BWhile { body; _ } | BFor { body; _ } ->
      List.concat_map collect_lhs_in_stmt body
  | _ -> []

(* A BCombinational block whose name starts with `assign_` was emitted
 * by the converter from a `cont_assign1` continuous assign. Anything
 * else (including BSequential) counts as procedural. *)
let is_continuous_proc = function
  | BCombinational { name; _ }
    when String.length name >= 7
      && String.sub name 0 7 = "assign_" -> true
  | _ -> false

let proc_lhs_names = function
  | BCombinational { body; _ } | BSequential { body; _ } ->
      List.concat_map collect_lhs_in_stmt body

(* ─── Checks ─────────────────────────────────────────────────────── *)

let check_duplicate_signals (m : bmodule) =
  let seen = Hashtbl.create 16 in
  List.filter_map (fun (s : bsignal) ->
    if Hashtbl.mem seen s.name then Some (Duplicate_signal s.name)
    else (Hashtbl.add seen s.name (); None)
  ) m.signals

let check_multi_drivers (m : bmodule) =
  (* Tally how many times each signal is driven, separately by
   * continuous and procedural processes. *)
  let cont = Hashtbl.create 16 in
  let proc = Hashtbl.create 16 in
  List.iter (fun p ->
    let dst = if is_continuous_proc p then cont else proc in
    List.iter (fun lhs ->
      let prev = try Hashtbl.find dst lhs with Not_found -> 0 in
      Hashtbl.replace dst lhs (prev + 1)
    ) (proc_lhs_names p)
  ) m.processes;
  (* Multi-driver: more than one continuous assign to same lhs. *)
  let multi = Hashtbl.fold (fun lhs n acc ->
    if n > 1 then Multiple_continuous_drivers (lhs, n) :: acc
    else acc
  ) cont [] in
  (* Mixed: same lhs has BOTH continuous AND procedural drive. *)
  let mixed = Hashtbl.fold (fun lhs _ acc ->
    if Hashtbl.mem proc lhs then Mixed_proc_and_continuous lhs :: acc
    else acc
  ) cont [] in
  multi @ mixed

(* ─── Top-level ──────────────────────────────────────────────────── *)

let check_module (m : bmodule) : sanity_error list =
  check_duplicate_signals m
  @ check_multi_drivers m

let check_program (p : bprogram) : (string * sanity_error list) list =
  List.map (fun (m : bmodule) -> (m.name, check_module m)) p.modules

(* Convenience predicate: any module has at least one error. *)
let has_errors (p : bprogram) =
  List.exists (fun (m : bmodule) -> check_module m <> []) p.modules

(* Print a one-line diagnostic per error to stderr. Returns the
 * total error count so the caller can decide on exit code. *)
let report (p : bprogram) =
  let n = ref 0 in
  List.iter (fun (m : bmodule) ->
    let errs = check_module m in
    List.iter (fun e ->
      Printf.eprintf "[sanity] %s: %s\n" m.name (string_of_error e);
      incr n
    ) errs
  ) p.modules;
  !n
