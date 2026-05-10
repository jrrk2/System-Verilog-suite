(* Pre-emptive replacement of OpenROAD's [replace_arith_modules] —
   the floorplan-stage transform that swaps yosys's ripple-carry
   chains for prefix-sum adders (Sklansky / Brent_kung / Kogge_stone)
   and array multipliers for Wallace / Dadda trees.

   Why move it here, before OpenROAD reads our Verilog
   ===================================================
   OpenROAD's transform is structural — it pattern-matches an
   arithmetic block in the read-in ODB and re-emits gates.  Doing the
   same swap *during* synth has three concrete advantages:

   1. We know the block kind, signal name, and width without pattern-
      matching: [Block_tag] recorded {kind=ADD, signal, width, arch}
      when the block was emitted.  Selection is exact, not
      heuristic.
   2. We can certify the swap (Z3 boundary miter against a behavioural
      `a + b`) and cache the proof — ORD sees a netlist whose every
      arithmetic block is already proven against its spec.  Their
      `replace_arith_modules` does not.
   3. The swap fires on every block above the width threshold, not
      only the ones that survive WNS-driven repair budgets.

   Implementation strategy
   =======================
   This is the predict-only flavour: walks the Block_tag block list,
   scores each block (very crude delay model: ripple O(W); brent-kung
   2*ceil(log2 W)+1; ditto for sklansky), and emits a report of which
   blocks WOULD be swapped under [SV_DECOMP_ARCH_SWAP=1].  The actual
   in-place swap (regenerate the block via [Arch_verify.emit_adder_verilog]
   + cert-gate via [Arch_verify.verify_adder]) is the next commit; this
   one ships the analysis so the call site is wired up safely.        *)

(* ── Crude delay estimates (FO4-equivalents, 45-nm-ish) ─────────── *)

let ripple_stages w =
  (* Each FA = 2 * XOR + 2 * AND + OR ≈ 5 cell stages on the carry
     path.  But practical libraries collapse a few of those, so use
     ~3 stages per bit for the carry-out. *)
  3 * w

let prefix_stages w =
  (* Sklansky / Brent_kung / Kogge_stone all have O(log W) carry-tree
     depth.  Brent_kung has 2*log W − 1 stages of [g/p] gates plus a
     final XOR; ≈ 2 * ceil(log2 W) + 1.                              *)
  let rec log2_ceil n = if n <= 1 then 0 else 1 + log2_ceil ((n + 1) / 2) in
  2 * log2_ceil w + 1

let array_mul_stages aw bw =
  (* Each row of partial-product rippling: bw rows * (carry-prop ≈ aw). *)
  bw * aw

let tree_mul_stages aw bw =
  (* Wallace/Dadda compress bw partial products in O(log bw) levels. *)
  let rec log32_ceil n = if n <= 1 then 0 else 1 + log32_ceil ((2 * n) / 3) in
  log32_ceil bw + ripple_stages aw

(* ── Scoring + report ──────────────────────────────────────────── *)

type swap_proposal = {
  block_id  : int;
  module_   : string;
  signal    : string;
  width     : int;
  from_arch : string;
  to_arch   : string;
  before    : int;        (* current stage estimate *)
  after     : int;        (* proposed stage estimate *)
}

(* Width threshold below which the swap isn't worth it: prefix-sum
   adders win at ≥8 bits in 45 nm; below that the ripple is shorter
   in stages and lower in area.  Override via SV_DECOMP_ARCH_MIN_W. *)
let min_width_default = 8
let min_width () =
  match Sys.getenv_opt "SV_DECOMP_ARCH_MIN_W" with
  | Some s -> (try int_of_string s with _ -> min_width_default)
  | None -> min_width_default

let propose_for_block (b : Block_tag.block_record) : swap_proposal option =
  let w = b.br_width in
  if w < min_width () then None
  else
    let mk to_arch before after =
      Some { block_id = b.br_id; module_ = b.br_module;
             signal = b.br_signal; width = w;
             from_arch = b.br_arch; to_arch; before; after } in
    match b.br_kind, b.br_arch with
    | Block_tag.ADD, "ripple" ->
        mk "brent_kung" (ripple_stages w) (prefix_stages w)
    | Block_tag.SUB, "ripple" ->
        (* sub = a + ~b + 1 → same prefix-sum infra *)
        mk "brent_kung" (ripple_stages w) (prefix_stages w + 2)
    | Block_tag.MUL, "array" ->
        mk "wallace"
          (array_mul_stages (w / 2) (w / 2))
          (tree_mul_stages (w / 2) (w / 2))
    | _ -> None

let propose_all () : swap_proposal list =
  Block_tag.with_blocks (fun blocks ->
    List.filter_map propose_for_block blocks)

(* ── Reporter ──────────────────────────────────────────────────── *)

let report_to oc =
  let proposals = propose_all () in
  if proposals = [] then
    Printf.fprintf oc "[arch_swap] no swap candidates (all blocks below width threshold of %d)\n"
      (min_width ())
  else begin
    Printf.fprintf oc
      "[arch_swap] %d swap candidate(s) (set SV_DECOMP_ARCH_SWAP=1 to apply, predict-only otherwise):\n"
      (List.length proposals);
    let total_saved = ref 0 in
    List.iter (fun p ->
      let saved = p.before - p.after in
      total_saved := !total_saved + saved;
      Printf.fprintf oc
        "  B%d  %s/%s[%d] : %s → %s   stages %d → %d  (Δ%+d)\n"
        p.block_id p.module_ p.signal p.width
        p.from_arch p.to_arch p.before p.after (-saved)
    ) proposals;
    Printf.fprintf oc "[arch_swap] total stage savings: %d\n" !total_saved
  end

let report () = report_to stderr
