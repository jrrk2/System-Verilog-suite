(* equiv_core.ml — engine behind the Z3 equivalence workbench.
 *
 * The workbench is a 3-step flow:
 *   1. load two designs (RTL or netlist), each through its own frontend,
 *      with an optional synthesis/lowering pass;
 *   2. match the two register spaces (name → canonical name → simulation
 *      signature → manual override), because the miter ties state BY NAME
 *      and a synthesised netlist does not keep the RTL's names;
 *   3. build the miter, run it, and debug a counterexample on the cone of
 *      influence behind the failing output.
 *
 * EVERYTHING lives here rather than in the GTK layer, so any verdict the GUI
 * shows can be reproduced headless (`sv_suite equiv …`).  That is deliberate:
 * an equivalence checker whose answers exist only inside a window cannot be
 * checked by anyone but the person at the keyboard.
 *
 * The failure mode this module is written against is NOT "says DIFFER when it
 * should say EQUIVALENT" — it is the VACUOUS PASS: a miter that matched no
 * registers, compared no outputs, or constrained no inputs will happily report
 * equivalence.  So every verdict is accompanied by a census of what was
 * actually proved over, and [verdict_is_trustworthy] downgrades a pass whose
 * coverage is too thin to mean anything.
 *)

open Behavioral_ir
module BI = Behavioral_initeval

(* ────────────────────────────────────────────────────────────────
 * Side loading
 * ──────────────────────────────────────────────────────────────── *)

type side_spec = {
  s_tag      : string;        (* "A" (spec/reference) or "B" (impl) *)
  s_frontend : string;        (* verible | slang | yosys | verilator | vhdl | … *)
  s_files    : string list;
  s_top      : string;
  s_post     : string;        (* "none" | "optimize" | "flatten" *)
}

let default_spec tag = {
  s_tag = tag; s_frontend = "verible"; s_files = []; s_top = ""; s_post = "none";
}

type side = {
  sp      : side_spec;
  prog    : bprogram;
  picked  : bmodule;          (* top module as the frontend produced it *)
  prepped : bmodule;          (* prep_for_z3: hierarchy flattened, always-blocks lowered *)
  ripped  : bmodule;          (* ssa → ffrip → share: EXACTLY what Z3 sees *)
}

(* The frontend list is DISCOVERED, not declared: `Tool_scan` finds each
   external tool (or the user's selection of it) and only the ones that can
   actually run are offered.  A menu entry for a tool that is not installed is
   a trap — the user picks it and gets a failure from three layers down. *)
let frontends ?(rescan = false) () = Tool_scan.available_frontends ~rescan ()

let post_passes = [ "none"; "optimize"; "flatten" ]

(* What Z3 actually encodes.  Mirrors check_miter_equivalence's own prologue —
   if that changes, this must change with it, or the census below describes a
   different circuit from the one being proved. *)
let rip (m : bmodule) : bmodule =
  m |> Behavioral_ssa.module_to_ssa
    |> Behavioral_ffrip.rip_module
    |> Behavioral_share.share_module

let pick_top (p : bprogram) (top : string) : bmodule =
  match List.find_opt (fun (m : bmodule) -> m.name = top) p.modules with
  | Some m -> m
  | None ->
      (* Parameter specialisation renames the module (`foo` → `foo__W8`);
         accept a UNIQUE specialisation, list them when it is ambiguous. *)
      let pfx = top ^ "__" in
      let pl = String.length pfx in
      let cands = List.filter (fun (m : bmodule) ->
        String.length m.name > pl && String.sub m.name 0 pl = pfx) p.modules in
      (match cands with
       | [ m ] -> m
       | [] ->
           failwith (Printf.sprintf "no module '%s' (have: %s)" top
                       (String.concat ", "
                          (List.map (fun (m : bmodule) -> m.name) p.modules)))
       | ms ->
           failwith (Printf.sprintf
             "module '%s' is ambiguous — %d specialisations: %s" top
             (List.length ms)
             (String.concat ", " (List.map (fun (m : bmodule) -> m.name) ms))))

let load_side (sp : side_spec) : side =
  if sp.s_files = [] then failwith (sp.s_tag ^ ": no source files");
  if sp.s_top = "" then failwith (sp.s_tag ^ ": no top module");
  (* Fail here, where the message can name the fix, rather than inside the
     frontend where it reads as a parse failure. *)
  (match Tool_scan.ensure sp.s_frontend with
   | Ok () -> ()
   | Error e -> failwith (sp.s_tag ^ ": " ^ e));
  let prog = Sv_lua.load_frontend ~frontend:sp.s_frontend ~top:sp.s_top
               ~files:sp.s_files in
  let prog = match sp.s_post with
    | "optimize" -> fst (Behavioral_optimize.optimize_custom
                           { Behavioral_optimize.default_config with verbose = false } prog)
    | _ -> prog in
  let picked = pick_top prog sp.s_top in
  let prepped =
    match sp.s_post with
    | "flatten" -> Behavioral_hier.flatten_for_z3 prog ~top:picked.name
    | _ -> Sv_lua.prep_for_z3 picked prog in
  { sp; prog; picked; prepped; ripped = rip prepped }

(* ────────────────────────────────────────────────────────────────
 * Register space
 * ──────────────────────────────────────────────────────────────── *)

let sig_width (s : bsignal) = match s.stype with
  | BInt { width; _ } -> width
  | BBool -> 1
  | BArray { size; element = BInt { width; _ } } -> size * width
  | _ -> 1

let has_suffix suf n =
  let l = String.length suf and k = String.length n in
  k >= l && String.sub n (k - l) l = suf

(* A "register" of a ripped module = a `<base>__D` next-state output.  This is
   the same definition the miter compares over, so counting anything else here
   would describe a circuit nobody is proving. *)
type reg = { r_base : string; r_width : int; r_qin : string }

let regs_of (rm : bmodule) : reg list =
  let inset = Hashtbl.create 128 in
  List.iter (fun (s : bsignal) ->
    if is_input_dir s.direction then Hashtbl.replace inset s.name ()) rm.signals;
  List.filter_map (fun (s : bsignal) ->
    if s.direction = `Output && has_suffix "__D" s.name then
      let base = String.sub s.name 0 (String.length s.name - 3) in
      Some { r_base = base; r_width = sig_width s;
             r_qin = if Hashtbl.mem inset (base ^ "__Q") then base ^ "__Q" else base }
    else None) rm.signals

(* Canonical register name for cheap cross-flow matching.  Synthesis decorates:
   Vivado appends `_reg`, yosys splits a bus into `foo[3]`, frontends differ on
   the `/` vs `__` hierarchy separator.  canon_sep_name (sv_lua) already handles
   the separator and bit-index forms; strip the synthesis suffixes on top. *)
let canon_reg (n : string) : string =
  let n = Sv_lua.canon_sep_name n in
  let strip suf s =
    if has_suffix suf s && String.length s > String.length suf
    then String.sub s 0 (String.length s - String.length suf) else s in
  n |> strip "__Q" |> strip "_reg" |> strip "_ff" |> strip "_q"
    |> String.lowercase_ascii

type meth = Exact | Canon | Sim | Manual | Forced_unmatched | Unmatched

let meth_str = function
  | Exact -> "name" | Canon -> "canonical" | Sim -> "simulation"
  | Manual -> "manual" | Forced_unmatched -> "manual (unmatched)"
  | Unmatched -> "UNMATCHED"

type pair = {
  p_a    : string;            (* A-side register base *)
  p_a_w  : int;
  p_b    : string option;     (* B-side register base, None = unmatched *)
  p_b_w  : int;
  p_meth : meth;
}

(* Manual decisions, keyed by A-side register.  [Some b] pins the pair,
   [None] forces "leave this one unmatched".  Persisted in the project file:
   a matching you have to redo by hand on every run is a matching nobody
   will use twice. *)
type overrides = (string * string option) list

(* Simulation-signature matching, via sv_lua's partition refinement.  It
   returns rename pairs (target → reference) on the PREPPED modules, including
   `__D`, state-input and base entries; the `__D` entries are the ones that
   name a register unambiguously. *)
let sim_pairs (a : side) (b : side) : (string * string) list =
  let m = try Sv_lua.reg_correspond a.prepped b.prepped with _ -> [] in
  List.filter_map (fun (t, r) ->
    if has_suffix "__D" t && has_suffix "__D" r then
      Some (String.sub t 0 (String.length t - 3),
            String.sub r 0 (String.length r - 3))
    else None) m

(* The matching pipeline.  Stages run in increasing cost and decreasing
   confidence; each stage only looks at what is still unmatched. *)
(* [stale] collects overrides that could not be applied: the register they
   name is gone (renamed by a pass, or the design changed since the project
   was saved).  Dropping them silently is the "looks fine, has no effect"
   failure — the user pinned a pair and got automatic matching instead. *)
let match_registers ?(use_sim = true) ?(overrides : overrides = [])
    (a : side) (b : side) : pair list * reg list * (string * string) list =
  let aregs = regs_of a.ripped and bregs = regs_of b.ripped in
  let stale = ref [] in
  let bw = Hashtbl.create 128 in
  List.iter (fun r -> Hashtbl.replace bw r.r_base r.r_width) bregs;
  let b_taken : (string, unit) Hashtbl.t = Hashtbl.create 128 in
  let result : (string, string option * meth) Hashtbl.t = Hashtbl.create 128 in

  (* 0. manual overrides win, and are applied first so they also remove their
        B-side register from the pool the automatic stages draw from. *)
  List.iter (fun (an, bo) ->
    if not (List.exists (fun r -> r.r_base = an) aregs) then
      stale := (an, (match bo with Some b -> b | None -> "(unmatched)")) :: !stale
    else
      match bo with
      | Some bn when Hashtbl.mem bw bn ->
          Hashtbl.replace result an (Some bn, Manual);
          Hashtbl.replace b_taken bn ()
      | Some bn -> stale := (an, bn) :: !stale
      | None -> Hashtbl.replace result an (None, Forced_unmatched)) overrides;

  let free_a () =
    List.filter (fun r -> not (Hashtbl.mem result r.r_base)) aregs in
  let claim an bn m =
    if not (Hashtbl.mem b_taken bn) then begin
      Hashtbl.replace result an (Some bn, m);
      Hashtbl.replace b_taken bn ()
    end in

  (* 1. exact name *)
  List.iter (fun r ->
    if Hashtbl.mem bw r.r_base then claim r.r_base r.r_base Exact) (free_a ());

  (* 2. canonical name, only where BOTH sides are unambiguous — an ambiguous
        canonical key is a real choice, not something to guess at. *)
  let group l key =
    let h = Hashtbl.create 128 in
    List.iter (fun r ->
      let k = key r in
      match Hashtbl.find_opt h k with
      | Some acc -> acc := r :: !acc
      | None -> Hashtbl.add h k (ref [ r ])) l;
    h in
  let bfree = List.filter (fun r -> not (Hashtbl.mem b_taken r.r_base)) bregs in
  let bcanon = group bfree (fun r -> canon_reg r.r_base) in
  List.iter (fun r ->
    match Hashtbl.find_opt bcanon (canon_reg r.r_base) with
    | Some { contents = [ br ] } -> claim r.r_base br.r_base Canon
    | _ -> ()) (free_a ());

  (* 3. simulation signature for the residue *)
  if use_sim && free_a () <> [] then begin
    let sp = sim_pairs a b in
    List.iter (fun (bn, an) ->
      if not (Hashtbl.mem result an) && Hashtbl.mem bw bn then
        claim an bn Sim) sp
  end;

  let pairs = List.map (fun r ->
    match Hashtbl.find_opt result r.r_base with
    | Some (Some bn, m) ->
        { p_a = r.r_base; p_a_w = r.r_width; p_b = Some bn;
          p_b_w = (try Hashtbl.find bw bn with Not_found -> 0); p_meth = m }
    | Some (None, m) ->
        { p_a = r.r_base; p_a_w = r.r_width; p_b = None; p_b_w = 0; p_meth = m }
    | None ->
        { p_a = r.r_base; p_a_w = r.r_width; p_b = None; p_b_w = 0;
          p_meth = Unmatched }) aregs in
  let b_left = List.filter (fun r -> not (Hashtbl.mem b_taken r.r_base)) bregs in
  (pairs, b_left, List.rev !stale)

(* Renames to apply to B's PREPPED module so the miter ties state by name.
   Renaming the base propagates: ffrip derives `<base>__D` / `<base>__Q` from
   the signal, so one entry per pair is enough (and does not risk renaming a
   `__D` while leaving its `__Q` behind). *)
let renames_of_pairs (pairs : pair list) : (string * string) list =
  List.filter_map (fun p ->
    match p.p_b with
    | Some b when b <> p.p_a -> Some (b, p.p_a)
    | _ -> None) pairs

let apply_renames (renames : (string * string) list) (m : bmodule) : bmodule =
  if renames = [] then m
  else
    let h = Hashtbl.create (List.length renames * 2) in
    List.iter (fun (f, t) -> Hashtbl.replace h f t) renames;
    Sv_lua.rename_module
      (fun x -> match Hashtbl.find_opt h x with Some t -> t | None -> x) m

(* ────────────────────────────────────────────────────────────────
 * Census: what a verdict is actually a statement about
 * ──────────────────────────────────────────────────────────────── *)

type census = {
  c_a_regs      : int;
  c_b_regs      : int;
  c_matched     : int;
  c_a_in        : int;
  c_b_in        : int;
  c_common_in   : int;
  c_a_out       : int;
  c_b_out       : int;
  c_common_out  : int;
  c_state_cones : int;   (* of the common outputs, how many are `__D` cones *)
  c_prim_cones  : int;   (* … and how many are primary outputs *)
}

let census (a_ripped : bmodule) (b_ripped : bmodule) (pairs : pair list) : census =
  let ain = Z3_miter.get_input_signals a_ripped
  and bin = Z3_miter.get_input_signals b_ripped
  and aout = Z3_miter.get_output_signals a_ripped
  and bout = Z3_miter.get_output_signals b_ripped in
  let names l = List.map fst l in
  let inter x y = List.filter (fun n -> List.mem n (names y)) (names x) in
  let common_out = inter aout bout in
  let state = List.filter (has_suffix "__D") common_out in
  { c_a_regs = List.length (regs_of a_ripped);
    c_b_regs = List.length (regs_of b_ripped);
    c_matched = List.length (List.filter (fun p -> p.p_b <> None) pairs);
    c_a_in = List.length ain; c_b_in = List.length bin;
    c_common_in = List.length (inter ain bin);
    c_a_out = List.length aout; c_b_out = List.length bout;
    c_common_out = List.length common_out;
    c_state_cones = List.length state;
    c_prim_cones = List.length common_out - List.length state }

(* An EQUIVALENT verdict is only worth as much as its coverage.  Returns
   [None] when the proof looks solid, [Some why] when it does not — the GUI
   shows that alongside the verdict rather than a bare green tick. *)
let coverage_warnings (c : census) : string list =
  let w = ref [] in
  let add s = w := s :: !w in
  if c.c_common_in = 0 && (c.c_a_in > 0 || c.c_b_in > 0) then
    add "NO input is shared: every input constraint is vacuous";
  if c.c_common_out = 0 then
    add "NO output is compared: the miter asserts nothing";
  if c.c_a_regs > 0 && c.c_matched = 0 then
    add (Printf.sprintf "NO register matched (A has %d, B has %d): state is \
                         tied by name and no name corresponds"
           c.c_a_regs c.c_b_regs);
  if c.c_a_regs > 0 && c.c_matched > 0 && c.c_matched * 2 < c.c_a_regs then
    add (Printf.sprintf "only %d of %d A-registers matched (%d%%): most of the \
                         state is compared against nothing"
           c.c_matched c.c_a_regs (100 * c.c_matched / c.c_a_regs));
  if c.c_b_regs > c.c_matched && c.c_matched > 0 then
    add (Printf.sprintf "%d B-registers are unmatched — their next-state cones \
                         are outside the proof"
           (c.c_b_regs - c.c_matched));
  if c.c_common_in * 2 < c.c_a_in then
    add (Printf.sprintf "only %d of %d A-inputs are constrained: the rest are \
                         free on each side independently"
           c.c_common_in c.c_a_in);
  List.rev !w

let string_of_census (c : census) : string =
  Printf.sprintf
    "Proved over\n\
    \  inputs constrained : %d common  (A %d, B %d)\n\
    \  outputs compared   : %d common  (A %d, B %d)  = %d next-state cones + %d primary\n\
    \  registers matched  : %d  (A %d, B %d)\n"
    c.c_common_in c.c_a_in c.c_b_in
    c.c_common_out c.c_a_out c.c_b_out c.c_state_cones c.c_prim_cones
    c.c_matched c.c_a_regs c.c_b_regs

(* ────────────────────────────────────────────────────────────────
 * Running the miter
 * ──────────────────────────────────────────────────────────────── *)

type mode = Flat | Per_cone | Hierarchical

let mode_str = function
  | Flat -> "flat" | Per_cone -> "per-cone" | Hierarchical -> "hierarchical (bottom-up)"

(* stdout/stderr capture — the engine's report IS its evidence, and it prints.
   Losing it to the terminal behind the window would leave the GUI asserting a
   verdict with nothing to back it. *)
let capture (f : unit -> 'a) : ('a, exn) result * string =
  let tmp = Filename.temp_file "svs_equiv" ".log" in
  let fd = Unix.openfile tmp [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600 in
  flush stdout; flush stderr;
  let so = Unix.dup Unix.stdout and se = Unix.dup Unix.stderr in
  Unix.dup2 fd Unix.stdout; Unix.dup2 fd Unix.stderr;
  let r = (try Ok (f ()) with e -> Error e) in
  flush stdout; flush stderr;
  Unix.dup2 so Unix.stdout; Unix.dup2 se Unix.stderr;
  Unix.close so; Unix.close se; Unix.close fd;
  let ic = open_in_bin tmp in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  (try Sys.remove tmp with _ -> ());
  (r, s)

let set_timeout ms =
  Unix.putenv "Z3_MITER_TIMEOUT_MS" (string_of_int ms);
  (* The context was built at module-load time from the env var, so the env
     alone would only affect the NEXT process.  Update the live context too. *)
  (try Z3.Params.update_param_value Z3_miter.ctx "timeout" (string_of_int ms)
   with _ -> ())

type run_result = {
  rr_verdict  : string;                    (* EQUIVALENT / DIFFER / INCONCLUSIVE / … *)
  rr_census   : census;
  rr_warnings : string list;
  rr_cones    : string list;               (* differing cones, per-cone mode *)
  rr_modules  : (string * string) list;    (* per-module verdicts, hierarchical mode *)
  rr_log      : string;
  rr_seconds  : float;
}

(* 0 = proved, 1 = a real difference, 2 = anything else.  An INCONCLUSIVE must
   never reach a Makefile looking like a pass. *)
let exit_code_of_verdict (v : string) : int =
  let starts p = String.length v >= String.length p
                 && String.sub v 0 (String.length p) = p in
  if starts "EQUIVALENT" then (if starts "EQUIVALENT (WEAK" then 2 else 0)
  else if starts "DIFFER" then 1
  else 2

(* The bottom-up walk reports per module and ends with a HIER-SUMMARY line.
   Fold that into one verdict — a multi-line report where a verdict is
   expected reads as an error to every caller downstream. *)
let parse_hier (report : string) : string * (string * string) list =
  let lines = String.split_on_char '\n' report in
  let mods = List.filter_map (fun l ->
    if String.length l > 5 && String.sub l 0 5 = "HIER " then
      match String.split_on_char ' ' (String.sub l 5 (String.length l - 5))
            |> List.filter (fun x -> x <> "") with
      | n :: v :: _ -> Some (n, v)
      | _ -> None
    else None) lines in
  let summary = List.find_opt (fun l ->
    String.length l > 13 && String.sub l 0 13 = "HIER-SUMMARY ") lines in
  let verdict = match summary with
    | None -> "INCONCLUSIVE — hierarchical walk produced no summary"
    | Some sl ->
        let has sub = try ignore (Str.search_forward (Str.regexp_string sub) sl 0); true
                      with Not_found -> false in
        if has "whole design EQUIVALENT" then
          "EQUIVALENT (hierarchical, assume-guarantee)"
        else
          let first_diff = List.find_opt (fun (_, v) -> v <> "EQUIVALENT") mods in
          (match first_diff with
           | Some (n, v) when String.length v >= 6 && String.sub v 0 6 = "DIFFER" ->
               Printf.sprintf "DIFFER (hierarchical; first divergent module: %s)" n
           | Some (n, v) -> Printf.sprintf "%s (module %s)" v n
           | None -> "INCONCLUSIVE — " ^ sl) in
  (verdict, mods)

(* "first differing cones: a, b, c" — the per-cone report's worklist. *)
let parse_cone_list (log : string) : string list =
  let re = Str.regexp "first differing cones: \\(.*\\)" in
  try
    let _ = Str.search_forward re log 0 in
    Str.matched_group 1 log
    |> String.split_on_char ','
    |> List.map String.trim
    |> List.filter (fun s -> s <> "")
  with Not_found -> []

let run_miter ?(mode = Flat) ?(timeout_ms = 30000) (a : side) (b : side)
    (pairs : pair list) : run_result =
  set_timeout timeout_ms;
  let renames = renames_of_pairs pairs in
  let mb = apply_renames renames b.prepped in
  let cen = census a.ripped (rip mb) pairs in
  let t0 = Unix.gettimeofday () in
  let (res, log) = capture (fun () ->
    match mode with
    | Hierarchical ->
        (* Bottom-up, leaves first, children black-boxed as uninterpreted
           functions: the capacity-friendly route, and the one that localises
           a divergence to the smallest module that has it. *)
        let ha = Sv_lua.hadd (Sv_lua.Prog (a.sp.s_top, a.prog)) in
        let hb = Sv_lua.hadd (Sv_lua.Prog (b.sp.s_top, b.prog)) in
        Sv_lua.lmiter_hier ha hb a.picked.name
    | Per_cone | Flat ->
        (if mode = Per_cone then Unix.putenv "Z3_MITER_PER_CONE" "1"
         else (try Unix.putenv "Z3_MITER_PER_CONE" "" with _ -> ()));
        (try
           if Z3_miter.check_miter_equivalence a.prepped mb
           then "EQUIVALENT" else "DIFFER"
         with
         | Z3_miter.Solver_unknown why -> "INCONCLUSIVE — solver UNKNOWN: " ^ why
         | Z3_miter.Vacuous_comparison why -> "UNCOMPARABLE — " ^ why)) in
  let t1 = Unix.gettimeofday () in
  let raw = match res with
    | Ok v -> v
    | Error e -> "ERROR — " ^ Printexc.to_string e in
  let (verdict, modules) =
    match mode, res with
    | Hierarchical, Ok report -> parse_hier report
    | _ -> (raw, []) in
  let warnings = coverage_warnings cen in
  (* A pass with no coverage is not a pass.  Say so in the verdict itself:
     the summary line is what gets quoted, and "EQUIVALENT" quoted out of a
     vacuous run is exactly how a checker starts lying. *)
  let verdict =
    if verdict = "EQUIVALENT" && warnings <> []
    then "EQUIVALENT (WEAK — see coverage warnings)" else verdict in
  { rr_verdict = verdict; rr_census = cen; rr_warnings = warnings;
    rr_cones = (if mode = Per_cone then parse_cone_list log else []);
    rr_modules = modules;
    rr_log = (if mode = Hierarchical then log ^ "\n" ^ raw else log);
    rr_seconds = t1 -. t0 }

(* ────────────────────────────────────────────────────────────────
 * Counterexample: solve one cone, then debug it on the partial netlist
 * ──────────────────────────────────────────────────────────────── *)

let z_of_z3 (e : Z3.Expr.expr) : Z.t =
  let s = Z3.Expr.to_string e in
  let n = String.length s in
  try
    if n > 2 && s.[0] = '#' && s.[1] = 'x' then
      Z.of_string ("0x" ^ String.sub s 2 (n - 2))
    else if n > 2 && s.[0] = '#' && s.[1] = 'b' then
      Z.of_string ("0b" ^ String.sub s 2 (n - 2))
    else if n > 5 && String.sub s 0 5 = "(_ bv" then
      (* (_ bvNNN W) *)
      let rest = String.sub s 5 (n - 5) in
      let i = try String.index rest ' ' with Not_found -> String.length rest in
      Z.of_string (String.sub rest 0 i)
    else Z.of_string s
  with _ -> Z.zero

(* The signals a ripped module offers a simulator: primary inputs plus the
   lifted state inputs.  Everything else is derived. *)
let free_inputs (rm : bmodule) : (string * int) list =
  List.filter_map (fun (s : bsignal) ->
    if is_input_dir s.direction then Some (s.name, sig_width s) else None)
    rm.signals

(* Combinational evaluation of a ripped module under an input assignment.
   Same technique reg_correspond uses for its signatures: iterate the
   combinational statements to a fixpoint and read the scalar environment. *)
let simulate (rm : bmodule) (assign : (string * Z.t) list)
  : (string, Z.t) Hashtbl.t * int =
  let widths = Hashtbl.create 512 in
  List.iter (fun (s : bsignal) -> Hashtbl.replace widths s.name (sig_width s))
    rm.signals;
  let comb = List.filter_map (function
    | BCombinational r -> Some r.body | _ -> None) rm.processes in
  let env = { BI.widths; arrays = Hashtbl.create 8; elemw = Hashtbl.create 8;
              scalars = Hashtbl.create 512; awrites = Hashtbl.create 8 } in
  List.iter (fun (n, v) -> BI.set_scalar env n v) assign;
  let outs = List.filter_map (fun (s : bsignal) ->
    if s.direction = `Output then Some s.name else None) rm.signals in
  let chk () = List.fold_left (fun a n ->
    Z.add a (try Hashtbl.find env.BI.scalars n with Not_found -> Z.zero))
    Z.zero outs in
  let cap = 4 + List.length rm.signals in
  let failed = ref 0 and prev = ref (chk ()) and i = ref 0 and stop = ref false in
  while not !stop && !i < cap do
    BI.fuel := 0;
    List.iter (List.iter (fun st ->
      try BI.exec env st with _ -> incr failed)) comb;
    incr i;
    let c = chk () in
    if Z.equal c !prev && !i > 1 then stop := true else prev := c
  done;
  (env.BI.scalars, !failed)

(* name → defining expression, for the backward walk. *)
let def_map (rm : bmodule) : (string, bexpr) Hashtbl.t =
  let h = Hashtbl.create 512 in
  let rec walk = function
    | BAssign { lhs; rhs } -> if not (Hashtbl.mem h lhs) then Hashtbl.add h lhs rhs
    | BBlock l -> List.iter walk l
    | BIf { then_stmts; else_stmts; _ } ->
        List.iter walk then_stmts; List.iter walk else_stmts
    | BCase { cases; default; _ } ->
        List.iter (fun (_, l) -> List.iter walk l) cases; List.iter walk default
    | BWhile { body; _ } -> List.iter walk body
    | BFor { body; _ } -> List.iter walk body
    | BCallStmt _ | BReturn _ -> () in
  List.iter (function
    | BCombinational r -> List.iter walk r.body
    | BSequential r -> List.iter walk r.body) rm.processes;
  h

let rec vars_of (e : bexpr) (acc : string list ref) : unit =
  match e with
  | BVar n -> acc := n :: !acc
  | BConst _ -> ()
  | BBinOp { lhs; rhs; _ } -> vars_of lhs acc; vars_of rhs acc
  | BUnOp { operand; _ } -> vars_of operand acc
  | BSelect { array; index } -> vars_of array acc; vars_of index acc
  | BSlice { signal; _ } -> vars_of signal acc
  | BConcat l -> List.iter (fun e -> vars_of e acc) l
  | BReplicate { value; _ } -> vars_of value acc
  | BCond { condition; then_val; else_val } ->
      vars_of condition acc; vars_of then_val acc; vars_of else_val acc
  | BCall { args; _ } -> List.iter (fun e -> vars_of e acc) args

(* Transitive fanin of [root] on one side, stopping at signals that exist on
   BOTH sides (the common frontier) and at leaves with no definition.  This is
   the "partial gate-level netlist" the debug view works on: the cone behind
   one failing output, not the whole design. *)
let support (defs : (string, bexpr) Hashtbl.t) (common : (string, unit) Hashtbl.t)
    (root : string) : string list * int =
  let seen = Hashtbl.create 64 and sup = Hashtbl.create 32 and n = ref 0 in
  let rec go name =
    if not (Hashtbl.mem seen name) then begin
      Hashtbl.add seen name (); incr n;
      match Hashtbl.find_opt defs name with
      | None -> Hashtbl.replace sup name ()      (* leaf: input or state *)
      | Some e ->
          let acc = ref [] in
          vars_of e acc;
          List.iter (fun v ->
            if Hashtbl.mem common v then Hashtbl.replace sup v () else go v) !acc
    end in
  (match Hashtbl.find_opt defs root with
   | None -> Hashtbl.replace sup root ()
   | Some e ->
       Hashtbl.add seen root (); incr n;
       let acc = ref [] in
       vars_of e acc;
       List.iter (fun v ->
         if Hashtbl.mem common v then Hashtbl.replace sup v () else go v) !acc);
  (Hashtbl.fold (fun k () l -> k :: l) sup [], !n)

type divergence = {
  d_signal  : string;
  d_a       : Z.t;
  d_b       : Z.t;
  d_support : string list;    (* common signals it depends on — all AGREEING *)
  d_frontier : bool;          (* true = first divergence (all support agrees) *)
}

type ce = {
  ce_cone      : string;               (* the output that differs *)
  ce_assign    : (string * Z.t) list;  (* the counterexample stimulus *)
  ce_a_val     : Z.t;
  ce_b_val     : Z.t;
  ce_reproduced: bool;                 (* simulation agrees with Z3 *)
  ce_cone_size : int * int;            (* A-side, B-side signals in the cone *)
  ce_divs      : divergence list;      (* frontier first *)
  ce_a_def     : string;               (* defining expression, A side *)
  ce_b_def     : string;
  ce_note      : string;
}

(* Build a miter solver for the ripped pair: both encodings plus common-input
   equality.  Deliberately lighter than check_miter_equivalence (no constant-
   state sweep), so every counterexample it produces is REPLAYED through the
   simulator before it is believed — see [ce_reproduced]. *)
let cone_solver (ra : bmodule) (rb : bmodule) =
  Z3_miter.clear_miter_caches ();
  let (s1, _) = Z3_miter.encode_module ra "_d1" in
  let a1 = Z3.Solver.get_assertions s1 in
  Z3_miter.clear_cache ();
  let (s2, _) = Z3_miter.encode_module rb "_d2" in
  let a2 = Z3.Solver.get_assertions s2 in
  let ms = Z3.Solver.mk_simple_solver Z3_miter.ctx in
  Z3.Solver.add ms a1; Z3.Solver.add ms a2;
  let in1 = Z3_miter.get_input_signals ra and in2 = Z3_miter.get_input_signals rb in
  let sz z = Z3.BitVector.get_size (Z3.Expr.get_sort z) in
  List.iter (fun (n, w) ->
    match List.assoc_opt n in2 with
    | None -> ()
    | Some w2 ->
        let v1 = Z3_miter.bv_var n w "_d1" and v2 = Z3_miter.bv_var n w2 "_d2" in
        let w = min (sz v1) (sz v2) in
        let lo z = if sz z > w then Z3.BitVector.mk_extract Z3_miter.ctx (w - 1) 0 z else z in
        Z3.Solver.add ms [ Z3.Boolean.mk_eq Z3_miter.ctx (lo v1) (lo v2) ]) in1;
  ms

let common_outputs (ra : bmodule) (rb : bmodule) : (string * int) list =
  let o2 = Z3_miter.get_output_signals rb in
  List.filter (fun (n, _) -> List.mem_assoc n o2) (Z3_miter.get_output_signals ra)

let mask w v = if w <= 0 then v else Z.logand v (Z.pred (Z.shift_left Z.one w))

(* Solve one cone and turn SAT into an explanation.  The chain is:
     Z3 model  →  stimulus  →  simulate BOTH sides  →  value per signal
              →  signals present on both sides that DISAGREE
              →  of those, the ones whose whole common support AGREES.
   That last set is the first divergence: everything feeding it matches, its
   own value does not, so the bug is in the logic between them. *)
let explain_cone (a : side) (b : side) (pairs : pair list) (cone : string)
  : (ce, string) result =
  let ra = a.ripped in
  let rb = rip (apply_renames (renames_of_pairs pairs) b.prepped) in
  let outs1 = Z3_miter.get_output_signals ra
  and outs2 = Z3_miter.get_output_signals rb in
  match List.assoc_opt cone outs1, List.assoc_opt cone outs2 with
  | None, _ -> Error (Printf.sprintf "'%s' is not an output of A" cone)
  | _, None -> Error (Printf.sprintf "'%s' is not an output of B" cone)
  | Some w1, Some w2 ->
      let ms = cone_solver ra rb in
      let o1 = Z3_miter.bv_var cone w1 "_d1" and o2 = Z3_miter.bv_var cone w2 "_d2" in
      let sz z = Z3.BitVector.get_size (Z3.Expr.get_sort z) in
      let w = max (sz o1) (sz o2) in
      let ext z = if sz z < w then Z3.BitVector.mk_zero_ext Z3_miter.ctx (w - sz z) z else z in
      let x = Z3.BitVector.mk_xor Z3_miter.ctx (ext o1) (ext o2) in
      Z3.Solver.add ms
        [ Z3.Boolean.mk_not Z3_miter.ctx
            (Z3.Boolean.mk_eq Z3_miter.ctx x
               (Z3.BitVector.mk_numeral Z3_miter.ctx "0" w)) ];
      (match Z3.Solver.check ms [] with
       | Z3.Solver.UNSATISFIABLE ->
           Error (Printf.sprintf "cone '%s' is EQUIVALENT (no counterexample)" cone)
       | Z3.Solver.UNKNOWN ->
           Error (Printf.sprintf "cone '%s': solver returned UNKNOWN (%s)" cone
                    (Z3.Solver.get_reason_unknown ms))
       | Z3.Solver.SATISFIABLE ->
         match Z3.Solver.get_model ms with
         | None -> Error "SAT but no model"
         | Some model ->
           let ins_a = free_inputs ra and ins_b = free_inputs rb in
           let value_of nm w sfx =
             match Z3.Model.eval model (Z3_miter.bv_var nm w sfx) true with
             | Some v -> mask w (z_of_z3 v)
             | None -> Z.zero in
           let assign_a = List.map (fun (n, w) -> (n, value_of n w "_d1")) ins_a in
           (* B's free inputs that A also has take A's value (the miter ties
              them); B-only inputs take B's own model value. *)
           let assign_b = List.map (fun (n, w) ->
             match List.assoc_opt n assign_a with
             | Some v -> (n, mask w v)
             | None -> (n, value_of n w "_d2")) ins_b in
           let (va, fa) = simulate ra assign_a in
           let (vb, fb) = simulate rb assign_b in
           let get t n = try Some (Hashtbl.find t n) with Not_found -> None in
           let a_out = (match get va cone with Some v -> mask w1 v | None -> Z.zero)
           and b_out = (match get vb cone with Some v -> mask w2 v | None -> Z.zero) in
           let reproduced = not (Z.equal a_out b_out) in
           (* signals declared on BOTH sides — the only ones a value
              comparison means anything for *)
           let decl m = let h = Hashtbl.create 512 in
             List.iter (fun (s : bsignal) -> Hashtbl.replace h s.name (sig_width s))
               m.signals; h in
           let da = decl ra and db = decl rb in
           let common = Hashtbl.create 512 in
           Hashtbl.iter (fun n _ -> if Hashtbl.mem db n then Hashtbl.replace common n ()) da;
           let defs_a = def_map ra and defs_b = def_map rb in
           (* restrict to the cone: the union of both sides' fanin of [cone] *)
           let (sup_a, na) = support defs_a common cone in
           let (sup_b, nb) = support defs_b common cone in
           let in_cone = Hashtbl.create 256 in
           let rec mark defs name depth =
             if depth < 4096 && not (Hashtbl.mem in_cone name) then begin
               Hashtbl.replace in_cone name ();
               match Hashtbl.find_opt defs name with
               | None -> ()
               | Some e ->
                   let acc = ref [] in vars_of e acc;
                   List.iter (fun v -> mark defs v (depth + 1)) !acc
             end in
           mark defs_a cone 0; mark defs_b cone 0;
           ignore sup_a; ignore sup_b;
           let differs n =
             match get va n, get vb n with
             | Some x, Some y ->
                 let w = min (try Hashtbl.find da n with Not_found -> 64)
                             (try Hashtbl.find db n with Not_found -> 64) in
                 not (Z.equal (mask w x) (mask w y))
             | _ -> false in
           let diverging = Hashtbl.fold (fun n () acc ->
             if Hashtbl.mem in_cone n && differs n then n :: acc else acc) common [] in
           let divs = List.map (fun n ->
             let (spa, _) = support defs_a common n
             and (spb, _) = support defs_b common n in
             let sup = List.sort_uniq compare (spa @ spb) in
             let sup = List.filter (fun s -> s <> n) sup in
             { d_signal = n;
               d_a = (match get va n with Some v -> v | None -> Z.zero);
               d_b = (match get vb n with Some v -> v | None -> Z.zero);
               d_support = sup;
               d_frontier = not (List.exists differs sup) }) diverging in
           (* frontier first, then by support size: the smallest explanation
              at the top *)
           let divs = List.sort (fun x y ->
             match compare y.d_frontier x.d_frontier with
             | 0 -> compare (List.length x.d_support) (List.length y.d_support)
             | c -> c) divs in
           let show defs n = match Hashtbl.find_opt defs n with
             | Some e -> string_of_bexpr e
             | None -> "(primary input / lifted state)" in
           let note =
             let parts = ref [] in
             if not reproduced then
               parts := "the counterexample does NOT reproduce in simulation — \
                         treat it as an encoding artefact, not a design bug"
                        :: !parts;
             if fa + fb > 0 then
               parts := Printf.sprintf
                 "%d statement(s) could not be evaluated by the simulator \
                  (values below may be incomplete)" (fa + fb) :: !parts;
             if divs <> [] && not (List.exists (fun d -> d.d_frontier) divs) then
               parts := "no first-divergence frontier: every diverging signal \
                         depends on another one (the two netlists share no \
                         internal names in this cone)" :: !parts;
             parts := Printf.sprintf
               "state inputs are FREE in a combinational miter, so the state \
                shown may be unreachable in operation" :: !parts;
             String.concat "\n" (List.rev !parts) in
           Ok { ce_cone = cone;
                ce_assign = List.sort compare assign_a;
                ce_a_val = a_out; ce_b_val = b_out;
                ce_reproduced = reproduced;
                ce_cone_size = (na, nb);
                ce_divs = divs;
                ce_a_def = show defs_a cone;
                ce_b_def = show defs_b cone;
                ce_note = note })

(* Which cones differ, solved directly (so the GUI can offer a worklist even
   when the run was flat).  [limit] bounds the work: on a big design the first
   few differing cones are the ones you debug. *)
let differing_cones ?(limit = 40) (a : side) (b : side) (pairs : pair list)
  : string list * string list =
  let ra = a.ripped in
  let rb = rip (apply_renames (renames_of_pairs pairs) b.prepped) in
  let ms = cone_solver ra rb in
  let outs2 = Z3_miter.get_output_signals rb in
  let diff = ref [] and unknown = ref [] and n = ref 0 in
  List.iter (fun (name, w1) ->
    if !n < limit then
      match List.assoc_opt name outs2 with
      | None -> ()
      | Some w2 ->
          let o1 = Z3_miter.bv_var name w1 "_d1" and o2 = Z3_miter.bv_var name w2 "_d2" in
          let sz z = Z3.BitVector.get_size (Z3.Expr.get_sort z) in
          let w = max (sz o1) (sz o2) in
          let ext z = if sz z < w then Z3.BitVector.mk_zero_ext Z3_miter.ctx (w - sz z) z else z in
          Z3.Solver.push ms;
          Z3.Solver.add ms
            [ Z3.Boolean.mk_not Z3_miter.ctx
                (Z3.Boolean.mk_eq Z3_miter.ctx
                   (Z3.BitVector.mk_xor Z3_miter.ctx (ext o1) (ext o2))
                   (Z3.BitVector.mk_numeral Z3_miter.ctx "0" w)) ];
          (match Z3.Solver.check ms [] with
           | Z3.Solver.UNSATISFIABLE -> ()
           | Z3.Solver.SATISFIABLE -> incr n; diff := name :: !diff
           | Z3.Solver.UNKNOWN -> unknown := name :: !unknown);
          Z3.Solver.pop ms 1)
    (Z3_miter.get_output_signals ra);
  (List.rev !diff, List.rev !unknown)

(* ────────────────────────────────────────────────────────────────
 * Project file — a matching you cannot save is a matching you redo
 * ──────────────────────────────────────────────────────────────── *)

let json_of_spec (s : side_spec) : Yojson.Safe.t =
  `Assoc [ "frontend", `String s.s_frontend;
           "top", `String s.s_top;
           "post", `String s.s_post;
           "files", `List (List.map (fun f -> `String f) s.s_files) ]

let spec_of_json tag (j : Yojson.Safe.t) : side_spec =
  let str k d = match j with
    | `Assoc l -> (match List.assoc_opt k l with Some (`String s) -> s | _ -> d)
    | _ -> d in
  let files = match j with
    | `Assoc l -> (match List.assoc_opt "files" l with
        | Some (`List fl) -> List.filter_map (function `String s -> Some s | _ -> None) fl
        | _ -> [])
    | _ -> [] in
  { s_tag = tag; s_frontend = str "frontend" "verible"; s_top = str "top" "";
    s_post = str "post" "none"; s_files = files }

let save_project path (a : side_spec) (b : side_spec) (ov : overrides)
    (mode : mode) (timeout_ms : int) : unit =
  let j = `Assoc [
    "version", `Int 1;
    "a", json_of_spec a;
    "b", json_of_spec b;
    "mode", `String (match mode with
      | Flat -> "flat" | Per_cone -> "per-cone" | Hierarchical -> "hier");
    "timeout_ms", `Int timeout_ms;
    "overrides", `List (List.map (fun (an, bo) ->
      `Assoc [ "a", `String an;
               "b", (match bo with Some b -> `String b | None -> `Null) ]) ov) ] in
  let oc = open_out path in
  output_string oc (Yojson.Safe.pretty_to_string j);
  output_char oc '\n';
  close_out oc

let load_project path : side_spec * side_spec * overrides * mode * int =
  let j = Yojson.Safe.from_file path in
  let get k = match j with
    | `Assoc l -> (match List.assoc_opt k l with Some v -> v | None -> `Null)
    | _ -> `Null in
  let a = spec_of_json "A" (get "a") and b = spec_of_json "B" (get "b") in
  let ov = match get "overrides" with
    | `List l -> List.filter_map (function
        | `Assoc e ->
            (match List.assoc_opt "a" e with
             | Some (`String an) ->
                 Some (an, (match List.assoc_opt "b" e with
                            | Some (`String bn) -> Some bn | _ -> None))
             | _ -> None)
        | _ -> None) l
    | _ -> [] in
  let mode = match get "mode" with
    | `String "per-cone" -> Per_cone
    | `String "hier" -> Hierarchical
    | _ -> Flat in
  let timeout = match get "timeout_ms" with `Int n -> n | _ -> 30000 in
  (a, b, ov, mode, timeout)

(* ────────────────────────────────────────────────────────────────
 * Headless report — what `sv_suite equiv` prints, and what the GUI
 * writes when you ask it to save the run.
 * ──────────────────────────────────────────────────────────────── *)

let hex v = "0x" ^ Z.format "%x" v

let report_run (a : side) (b : side) (pairs : pair list) (unmatched_b : reg list)
    (r : run_result) : string =
  let buf = Buffer.create 4096 in
  let p fmt = Printf.ksprintf (Buffer.add_string buf) fmt in
  p "══════════════════════════════════════════════════════════════\n";
  p "  Equivalence: %s ↔ %s\n" a.picked.name b.picked.name;
  p "══════════════════════════════════════════════════════════════\n\n";
  p "A : %s   [%s%s]  %s\n" a.picked.name a.sp.s_frontend
    (if a.sp.s_post = "none" then "" else " + " ^ a.sp.s_post)
    (String.concat " " a.sp.s_files);
  p "B : %s   [%s%s]  %s\n\n" b.picked.name b.sp.s_frontend
    (if b.sp.s_post = "none" then "" else " + " ^ b.sp.s_post)
    (String.concat " " b.sp.s_files);
  p "%s" (string_of_census r.rr_census);
  if r.rr_modules <> [] then
    p "  (census above is the TOP module's flat surface; the hierarchical run \n\
      \   proves each module separately with its children abstracted)\n";
  p "\n";
  let by m = List.length (List.filter (fun x -> x.p_meth = m) pairs) in
  p "Register matching: %d by name, %d canonical, %d by simulation, %d manual, \
     %d unmatched (A) / %d unmatched (B)\n\n"
    (by Exact) (by Canon) (by Sim) (by Manual)
    (List.length (List.filter (fun x -> x.p_b = None) pairs))
    (List.length unmatched_b);
  if r.rr_modules <> [] then begin
    p "Per-module verdicts (leaves first; a parent's pass assumes its \
       children's):\n";
    List.iter (fun (n, v) -> p "  %-38s %s\n" n v) r.rr_modules;
    p "\n"
  end;
  p "VERDICT: %s   (%.2f s)\n\n" r.rr_verdict r.rr_seconds;
  if r.rr_warnings <> [] then begin
    p "Coverage warnings — the verdict is only as good as these:\n";
    List.iter (fun w -> p "  ⚠ %s\n" w) r.rr_warnings;
    p "\n"
  end;
  if r.rr_cones <> [] then
    p "Differing cones: %s\n\n" (String.concat ", " r.rr_cones);
  Buffer.contents buf

(* The register-matching table, exactly as page 2 of the workbench shows it.
   Printing it is how a manual override gets written correctly: the names it
   pins are these names, not the RTL's. *)
let report_registers ?(stale : (string * string) list = [])
    (pairs : pair list) (unmatched_b : reg list) : string =
  let buf = Buffer.create 4096 in
  let p fmt = Printf.ksprintf (Buffer.add_string buf) fmt in
  p "Register matching (%d A-registers)\n" (List.length pairs);
  p "  %-40s %4s  %-40s %4s  %s\n" "A register" "w" "B register" "w" "matched by";
  List.iter (fun x ->
    p "  %-40s %4d  %-40s %4s  %s\n" x.p_a x.p_a_w
      (match x.p_b with Some b -> b | None -> "—")
      (if x.p_b = None then "" else string_of_int x.p_b_w)
      (meth_str x.p_meth)) pairs;
  if unmatched_b <> [] then begin
    p "\nUnmatched B registers (%d):\n" (List.length unmatched_b);
    List.iter (fun (r : reg) -> p "  %-40s %4d\n" r.r_base r.r_width) unmatched_b
  end;
  if stale <> [] then begin
    p "\n⚠ %d manual override(s) COULD NOT BE APPLIED — the register they name \
       no longer exists:\n" (List.length stale);
    List.iter (fun (an, bn) -> p "  %s → %s\n" an bn) stale
  end;
  p "\n";
  Buffer.contents buf

let report_ce (c : ce) : string =
  let buf = Buffer.create 4096 in
  let p fmt = Printf.ksprintf (Buffer.add_string buf) fmt in
  p "Counterexample on cone %s\n" c.ce_cone;
  p "──────────────────────────────────────────────────────────────\n";
  p "  A = %s   B = %s   %s\n" (hex c.ce_a_val) (hex c.ce_b_val)
    (if c.ce_reproduced then "(reproduced in simulation)"
     else "(NOT reproduced in simulation)");
  p "  cone: %d signals (A) / %d signals (B)\n\n" (fst c.ce_cone_size) (snd c.ce_cone_size);
  p "Stimulus (%d free inputs; only non-zero shown):\n" (List.length c.ce_assign);
  List.iter (fun (n, v) ->
    if not (Z.equal v Z.zero) then p "  %-40s = %s\n" n (hex v)) c.ce_assign;
  p "\nFirst divergence (all inputs to these signals AGREE):\n";
  let front = List.filter (fun d -> d.d_frontier) c.ce_divs in
  if front = [] then p "  (none — see note)\n"
  else List.iter (fun d ->
    p "  %-40s A=%s  B=%s   [%d common inputs, all equal]\n"
      d.d_signal (hex d.d_a) (hex d.d_b) (List.length d.d_support)) front;
  let rest = List.filter (fun d -> not d.d_frontier) c.ce_divs in
  if rest <> [] then begin
    p "\nDownstream divergences (%d):\n" (List.length rest);
    List.iter (fun d -> p "  %-40s A=%s  B=%s\n" d.d_signal (hex d.d_a) (hex d.d_b))
      (List.filteri (fun i _ -> i < 20) rest)
  end;
  p "\nDefining expression\n  A: %s\n  B: %s\n" c.ce_a_def c.ce_b_def;
  p "\nNote:\n%s\n" c.ce_note;
  Buffer.contents buf
