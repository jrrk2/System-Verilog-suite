(* Stuck-at fault simulator + ATPG random-pattern coverage report.

   For every cell output net in the compiled netlist, enumerate two
   faults: stuck-at-0 and stuck-at-1.  For each fault, re-run the
   bit-parallel combinational simulator with that net forced to the
   stuck value, compare every PO and every FF.D net to the golden
   (fault-free) result, and call the fault "detected" if any output
   differs in any pattern.

   Stage 1 is the simple all-cells-all-faults loop.  Optimisations
   that will land in Stage 2:
     * cone-of-influence pruning (re-eval only cells downstream of
       the fault)
     * parallel-fault simulation (pack 64 faults into one re-eval)
     * fault dropping (skip already-detected faults on later patterns)

   Random patterns come from a seeded Random.State so runs are
   reproducible.                                                      *)

open Lib_map
open Gate_sim

(* ── Fault enumeration ──────────────────────────────────────────── *)

type fault = {
  f_net   : net_id;
  f_stuck : int;       (* 0 or 1 *)
}

let enumerate (c : compiled) : fault list =
  (* Stuck-at at every cell output net.  Could extend to per-input-pin
     faults (more thorough but ~4× the list); per-output is the
     standard collapsed fault list. *)
  let acc = ref [] in
  Array.iter (fun (_cell, _ins, out) ->
    if out >= 0 then begin
      acc := { f_net = out; f_stuck = 0 } :: !acc;
      acc := { f_net = out; f_stuck = 1 } :: !acc
    end
  ) c.c_evals;
  List.rev !acc

(* ── Pattern bench-vector ───────────────────────────────────────── *)

(* Build a closure that maps net-name → 64-bit pattern word.  PIs +
   FF.Q nets get one fresh random word each; everything else returns 0
   (never queried, but defensive). *)
let make_random_patterns ~seed (c : compiled) =
  let rng = Random.State.make [| seed |] in
  let by_name : (string, int) Hashtbl.t = Hashtbl.create 1024 in
  let alloc_word name =
    let hi = Random.State.bits rng in
    let lo = Random.State.bits rng in
    let mid = Random.State.bits rng in
    (* 3 × 30-bit chunks → 90 bits; mask to 64. *)
    let w = (hi lsl 30) lor (mid lsl 60) lor lo in
    Hashtbl.replace by_name name w in
  List.iter (fun nid -> alloc_word c.c_name_of_net.(nid)) c.c_pi_nets;
  List.iter (fun nid -> alloc_word c.c_name_of_net.(nid)) c.c_ff_q_nets;
  fun name ->
    try Hashtbl.find by_name name with Not_found -> 0

(* ── Coverage report ────────────────────────────────────────────── *)

type report = {
  r_module       : string;
  r_total_faults : int;
  r_detected     : int;
  r_patterns     : int;
}

let render_report (r : report) =
  let pct =
    if r.r_total_faults = 0 then 0.
    else 100. *. float_of_int r.r_detected /. float_of_int r.r_total_faults in
  Printf.sprintf
    "ATPG random-pattern coverage report\n\
     ───────────────────────────────────\n\
     Module     : %s\n\
     Patterns   : %d  (one int = 64 patterns; bit-parallel)\n\
     Faults     : %d  (stuck-at-{0,1} at every cell output)\n\
     Detected   : %d  (%.2f %%)\n\
     Undetected : %d  (%.2f %%)\n"
    r.r_module r.r_patterns r.r_total_faults r.r_detected pct
    (r.r_total_faults - r.r_detected) (100. -. pct)

(* Simulate one fault: re-run the netlist with the fault net forced
   to the stuck-at value, compare against [golden] at every observable
   net (POs + FF.D nets).  Returns [true] if any pattern differs.    *)
let detected_with ~c ~golden ~fault ~input_pat : bool =
  let fault_fn out_net raw =
    if out_net = fault.f_net then
      (if fault.f_stuck = 0 then 0 else -1)
    else raw in
  let faulty = run ~fault:fault_fn c ~input_pat in
  (* Compare every PO and every FF.D net pattern-word: detected if
     any bit differs. *)
  let mismatch = ref false in
  List.iter (fun nid ->
    if not !mismatch && golden.(nid) lxor faulty.(nid) <> 0 then
      mismatch := true) c.c_po_nets;
  if not !mismatch then
    List.iter (fun nid ->
      if not !mismatch && golden.(nid) lxor faulty.(nid) <> 0 then
        mismatch := true) c.c_ff_d_nets;
  !mismatch

(* ── Top-level entry ────────────────────────────────────────────── *)

let run_atpg
    ?(seed = 0xC0DE)
    ?(n_pattern_words = 16)   (* 16 × 64 = 1024 patterns *)
    ~module_name
    (nl : netlist) : report =
  let c = compile nl in
  let faults = enumerate c in
  let n_faults = List.length faults in
  let detected_table = Array.make n_faults false in

  let debug = Sys.getenv_opt "ATPG_DEBUG" = Some "1" in
  if debug then
    Printf.eprintf "[atpg] %s: %d cells, %d faults, %d obs nets (%d POs + %d FF.D)\n%!"
      module_name (Array.length c.c_evals) n_faults
      (List.length c.c_po_nets + List.length c.c_ff_d_nets)
      (List.length c.c_po_nets) (List.length c.c_ff_d_nets);
  for word = 0 to n_pattern_words - 1 do
    let input_pat = make_random_patterns ~seed:(seed + word) c in
    let golden = run c ~input_pat in
    List.iteri (fun idx fault ->
      if not detected_table.(idx) then begin
        if detected_with ~c ~golden ~fault ~input_pat then
          detected_table.(idx) <- true
      end
    ) faults
  done;

  let n_detected =
    Array.fold_left (fun acc b -> if b then acc + 1 else acc) 0 detected_table in
  { r_module       = module_name;
    r_total_faults = n_faults;
    r_detected     = n_detected;
    r_patterns     = n_pattern_words * 64 }
