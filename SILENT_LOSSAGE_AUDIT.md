# Silent-lossage audit (SVS synthesis pipeline)

Places where structural logic (instances / processes / assigns / ports / nets)
can be dropped WITHOUT an error. Principle: SVS must never silently discard
logic — it must either preserve it or bomb loudly naming what was lost.

## P1 — CONFIRMED functional bug
1. **`behavioral_hier_struct.ml` `flatten_structural` drops ALL `m.processes`.**
   Keeps binstances, discards every continuous assign — constant ties
   `assign x = <const0>`, net aliases `assign a = b`, any un-lowered comb logic.
   Safe for a FULLY gate-mapped module; FATAL for a primitive netlist.
   Dropped pcs_pma_flat.v's 1467 `<const0>/<const1>` ties in the PCS pass-through
   → driverless outputs → P&R trimmed downstream wrapper LUTs (opt LUT2 error).
   STATUS: **FIXED by tweaking the existing pass.** `flatten_structural` now
   RESOLVES simple continuous assigns (aliases / constant ties / bit-remaps)
   into cell-pin connectivity — `collect_resolvable_assigns` gathers them per
   module (rewritten into flat scope), and a read of an assigned net is
   replaced by its RHS. It still bombs loudly (`process_is_resolvable` = false)
   on genuinely un-structural processes (BIf/BCase/clocked) that must be
   gate-mapped first. Verified: pass-through arp flattens with no error;
   selftest 20/0; normal gate-mapped flow unaffected.

## P1b — CONFIRMED second bug (undriven cell input from gate_map)
1b. **SVS `gate_map` emits a cell input net with NO driver.**
    `sgmii_soc`'s `the_LUT2_361` (INIT 4'b0010) reads net `n_13503`, which
    appears exactly once in the EDIF — nothing drives it. Present in BOTH
    arp_svs.edf and the pass-through, but latent in arp_svs (its output was
    dead → opt trimmed it silently); the pass-through makes it live → hard
    opt error. This is why Hybrid A (SVS wrappers) also failed — the wrappers
    carry an undriven-net gate_map bug independent of the PCS.
    GUARD IMPLEMENTED: **ERC (electrical rule check) in `bir_to_edif`** — every
    net read by a cell INPUT pin must be driven by a cell OUTPUT, a top input,
    or a constant, else FAIL loudly naming net+readers (SVS_ERC=0 disables).
    Validated: fires on the pass-through (18 nets) and, crucially, on the
    NORMAL full-gate-map flow finds EXACTLY 1 undriven net — `mmcm_locked` in
    the shipping arp_svs.edf (not over-firing). Also caught the `C_GT_LOOPBACK`
    parameter LEAKING as an undriven net into the GT.
    Two follow-on fixes landed: (a) `behavioral_to_hardcaml` `__keep_` retention
    extended from FF-clock nets to FF-reset nets (`reset_names`) + FF-primitive
    async control pins (R/S/CLR/PRE) — retains async reset/set box outputs (the
    set-to-1 case is covered because reset_names captures the reset SIGNAL
    regardless of value). BUT this did NOT fix `mmcm_locked`: the ERC proved
    `__keep_mmcm_locked` IS emitted (retention works) — the fault is a DRIVER
    CONNECTIVITY BREAK: the PCS's `obuf_mmcm_locked_out` output does not connect
    to sgmii_soc's `mmcm_locked` net across the flatten boundary. Real bug,
    likely contributes to silicon failure; needs targeted flatten-boundary work.

## P1c — CONFIRMED + FIXED (parameter net leak, via existing elaboration)
1c. **A module parameter used as a NET value leaked as an undriven net.**
    `pcs_pma_flat.v`'s `gig_ethernet_pcs_pma_0_support` referenced the GTXE2
    `.LOOPBACK(C_GT_LOOPBACK)` where `C_GT_LOOPBACK` is the PARENT module's
    parameter — but `_support` neither declared it nor received it (our own
    hand-parameterization threaded the param onto the top only).  The existing
    hierarchical elaboration (`Verible_elaborate.specialise_design` +
    `convert_files`' inline `expr_to_bexpr ~params` substitution) resolves any
    PROPERLY-scoped parameter to a constant — the defect was a malformed source
    (undeclared cross-scope reference), NOT a missing pass.
    FIX: thread the parameter through the hierarchy in the source — declare
    `parameter [2:0] C_GT_LOOPBACK` on `_support` and pass `.C_GT_LOOPBACK(...)`
    from `gig_ethernet_pcs_pma_0`.  Elaboration then specialises
    `gig_ethernet_pcs_pma_0_support__CGL0` and folds the constant into the pin;
    the net vanishes.  ERC dropped 17→16.

## P1d — CONFIRMED + FIXED (concat-LHS continuous assign dropped)
1d. **`collect_resolvable_assigns` DROPPED an output-port assign whose port
    maps to a bit-split concat.**  A continuous-assign-driven output port
    (`assign status_vector = {…}` via the `merged_array` process) whose
    instantiation binds it bit-split — `.status_vector({_n_16,…,_n_1})` — has
    `rewrite_bexpr (BVar lhs)` return a `BConcat`; the old `| BVar nm -> … | _ ->
    None` silently discarded it, leaving all 16 parent nets driverless.  (A
    port driven directly by a primitive output — `gmii_rxd` — was unaffected,
    which is why ONLY `pcspma_status[15:0]` fired.)
    FIX: decompose a concat-LHS assign into one per-bit assign, extracting each
    RHS bit with a new width-aware `bexpr_bit` (the old `bit_select` wrongly
    assumed 1-bit concat elements, breaking on `{const0, ^sv[13:9], ^sv[7:0]}`).
    ERC dropped 16→0; pass-through EDIF is now driver-clean.

## P1e — CONFIRMED + FIXED (miter reader: bus-bit primitive output dropped)
1e. **`behavioral_hier.ml` `inline_instance` DROPPED a primitive output pin
    wired to a single BUS BIT.**  `lhs_subst` only mapped whole-net (`BVar`)
    output actuals; `.O(y[3])` (a `BSlice`/`BSelect`) hit the "bit-blast not yet
    supported" branch → `None`, and `fanout_procs` only handled `BConcat`.  So
    when the Z3 miter reads a SYNTHESISED netlist (every LUT/FF drives one bit of
    an output bus), each output bus collapsed to 0 → spurious DIFFER on almost
    every design (found via the sv-tests/yosys Vivado-vs-SVS cross-flow sweep:
    combinational `carryadd`'s Vivado netlist mis-read as 0).
    FIX: drive the per-bit net `obuf_<bus>_<i>__O` (declared as a signal) which
    `resolve_input_bitbus`'s existing output-assembly (`out_drivers`) rebuilds
    into the port.  Verified: `a+b` oracle vs Vivado `carryadd` netlist now
    EQUIVALENT; selftest 20/0.  NOTE this is a MITER-side (verification) fix, not
    a synthesis fix — but it made the cross-flow valid and immediately surfaced a
    real SVS FRONTEND bug: cross-generate-instance hierarchical refs
    (`STAGE[i-1].C` in carryadd) are DROPPED (treated as 0) → `y = a^b` not `a+b`
    (counterexample a=b=0xff → SVS 0x00 vs 0xfe).  Self-miters are BLIND to this
    (both sides share the mis-elaboration); the Vivado cross-flow catches it.
    FIXED (`verible_elaborate.ml namespace_genblk_locals`): the loop_generate
    unroll now namespaces each iteration's block-local decls/refs (`C`->`L__v__C`)
    and rewrites hierarchical refs (`L[k].C`->`L__k__C`).  Fires only for a LABELED
    block with >=1 local decl (local-free blocks untouched -> no regression).
    carryadd cross-flow EQUIVALENT; generate/hierarchy still EQUIV; selftest 20/0.

## P1f — CONFIRMED + FIXED (dynamic-shift amount silently WRAPPED)
1f. **`behavioral_to_hardcaml.ml` `log_shift_clamped` truncated a dynamic shift
    amount to clog2(W) bits** — `x >> 8` on an 8-bit x wrapped to `x >> 0` = x
    (correct result 0).  Found by the Vivado↔SVS cross-flow miter (rand_2);
    self-miter ALSO differed (gate_map vs behavioral spec) → real wrong-netlist
    bug for any dynamic shift whose RHS can represent values ≥ W.
    FIX: keep the low clog2(W) bits for log_shift (whose stages compose
    correctly, flushing naturally for in-range amounts ≥ W), and OR-reduce the
    DROPPED high bits into an overflow mux → flush value (zeros for sll/srl,
    sign-replicate for sra).  rand_2 self + cross-flow EQUIVALENT; selftest 20/0.

## P1g — CONFIRMED real bugs, NOT YET FIXED (found by Vivado cross-flow miter)
1g. **Signedness dropped in width extension.**  `reg [1:0] reg10 <= wire4` with
    `input signed wire4` must SIGN-extend (reg10 = {wire4,wire4}); SVS
    zero-extends ({1'b0,wire4}).  CONFIRMED by RTL-vs-RTL oracle miter (no
    netlist involved) on yosys dff_init.v (dff_test_997).
    **FIXED** (`verible_to_behavioral.ml` convert_module tail): every decl
    site hardcoded `signed = Unsigned`, so the qualifier was LOST at parse.
    Recovery pass scans decl CST nodes (module_port/port/net/data/tf_port
    declarations) for the `Signed` token, marks the signals' stype, and
    normalises assignments whose RHS is a signed expr narrower than the LHS
    by MATERIALISING the extension at BIR level ({{n{x[msb]}},x} via
    Behavioral_const.sign_extend), with the context width pushed INTO
    arith/bitwise binop operands (extending a narrower op's result ≠
    extending its operands — carries).  Encoder-agnostic: fixes the Z3 miter
    AND the gate_map netlists in one place.  Gated on ≥1 signed decl → no
    effect on unsigned designs.  Verified: sign oracle EQUIVALENT; dff_init
    cross-flow EQUIVALENT; selftest 20/0; random sweep 40/40; eth wrappers
    2/2.  STILL OPEN in this area: signed comparisons (`<` should be slt),
    signed div/mod, signed shifts-right (>>> of signed), and signed
    PORT-connection coercion across module boundaries — the normalisation
    covers the assignment context only.
1h. **`wand`/`wor` wired-logic nets: extra drivers SILENTLY DROPPED.**  Multiple
    continuous assigns to a wand/wor net must resolve by wired-AND/OR; SVS
    keeps one assign and drops the rest (yosys wandwor.v: inputs A,B,D vanish
    from the converted module).  Must at least fail loudly.

## P2 — silent drops to guard with an error
2. **`behavioral_to_hardcaml.ml:~632` `@slice_write` on a flat BInt dropped**
   (TODO #117). A sliced-LHS bus write (read-modify-write) returns `alw`
   unchanged → the write vanishes. Add error naming the signal.
3. **`behavioral_inline.ml:~193` uninlinable function-case dropped** (`| None ->
   acc  (* skip uninlinable case *)`). A case that can't be inlined disappears
   from the generated mux tree. Add error.
4. **`fpga_synth/bir_to_aig.ml:~104` ripple-carry add drops carry-out.**
   Documented for low-N adds; verify carry-out is genuinely unused everywhere it
   is called, else the high bit is lost.
5. **`fpga_synth/fpga_map.ml:~55` CE defaults to vdd when the enable net is
   lost** (driverless top output). A dropped enable silently becomes
   always-enabled. Guard / error.

## P3 — swallowed exceptions / silent defaults (lower risk)
6. `behavioral_hier_struct.ml:~38` width parse `with _ -> 0` (silent width-0).
7. `verible_to_behavioral.ml` number parsing `try ... with _ -> None/0` — silent
   parse-failure defaults; warn on unexpected input.

## Already guarded (audit trail exists — no action)
- `flatten_for_z3` (`behavioral_hier.ml`) drops unresolved primitives but calls
  `unresolved_register` → miter reports "INCONCLUSIVE — N primitive bodies".
- `verible_to_behavioral.ml:~4471` — external instances always KEPT now (the
  keep_external fix); unknown names bomb at the gate_map port-dir check.
- `sv_lua.ml:~906-928` — gate_map bombs loudly on unresolved primitive port dirs.
- `bir_to_edif.ml:~190-206` — based-literal INIT with no digits fails loudly.
