# Bottom-up cva6 miter

Per-module formal equivalence: Verilator's parse of the cva6 RTL on
one side, Vivado's `synth_design -rtl` elaboration of the same RTL
on the other, mitered with Z3 module-by-module.

## Inputs

| File | Producer |
|---|---|
| `cva6_elab.vhd` | `elab_cva6.tcl` (Vivado RTL elaboration of the cva6 core, dumped via `write_vhdl`) |
| `cva6_verilate.json.dir/Vcva6.tree.json` | `verilate_cva6.sh` (Verilator `--json-only` on the same source set cva6's `make verilate` uses, top = `cva6`) |

The two flows see the same SV: `verilate_cva6.sh` reuses
`core/Flist.cva6` plus the `ariane_pkg` and global headers from the
Makefile, mirroring `make verilate` exactly except that we ask for
the parse-tree JSON instead of building the simulator binary.

## Running

```bash
# 1. Vivado side (≈5 min, only re-run when sources change):
cd test/cva6_ram
TOP=cva6 OUT_BASE=cva6_elab vivado -mode batch -source elab_cva6.tcl

# 2. Verilator side (≈10s):
RISCV=/usr/bin CVA6_REPO_DIR=$HOME/cva6 bash verilate_cva6.sh

# 3. Per-module miter, filtered to a few small leaves:
$repo/_build/default/test_cva6_bottom_up.exe \
    cva6_verilate.json.dir/Vcva6.tree.json \
    cva6_elab.vhd \
    gated_clk_cell ct_vfdsu_ff1 lfsr exp_backoff rstgen_bypass

# 4. Whole-design sweep (no filter — every module that exists on
#    both sides):
$repo/_build/default/test_cva6_bottom_up.exe \
    cva6_verilate.json.dir/Vcva6.tree.json cva6_elab.vhd
```

The driver (`test_cva6_bottom_up.ml`) does:

1. Parse Verilator JSON → BIR via `Verilator_to_behavioral`
2. Parse Vivado VHDL → BIR via `Vhdl_to_ver_front`
3. Apply the four BIR transformations on the SV side:
   `Behavioral_unroll → Behavioral_inline → Behavioral_iflift → Behavioral_meminfer`
   so loops are unrolled, function/task calls inlined, if/else lifted
   to ternary, and case-with-constants converted to ROM lookups
4. Pair every SV-side module with the matching Vivado entity by name
5. Run `Z3_miter.check_miter_equivalence` per pair
6. Print a per-module verdict and a summary table

Filters can list multiple substrings — a module passes if any matches.

## Why bottom-up

`cva6` as one big top is too large for a single Z3 miter run, and the
hierarchy obscures where mismatches actually live. Running each leaf
separately:

- **Localises mismatches** — a counter-example points at one entity
- **Surfaces converter gaps incrementally** — fix the smallest leaf
  that fails, re-run, the next one becomes tractable
- **Parallelisable** — different filters can run on different cores

The top-level `cva6` entity is naturally the last thing to verify, and
only after every leaf in its hierarchy passes.

## Status of representative leaves (this round)

The driver runs to completion on the full 30 MB Verilator JSON and
the 52 MB Vivado VHDL. With the BSlice clamp (`z3_miter.ml` —
gracefully handles `mk_extract` requests beyond the source signal's
width), the standalone-elab leaf list is now:

```
✅ gated_clk_cell, ct_vfdsu_ff1, ct_vfdsu_pack
✅ exp_backoff, rstgen_bypass, clk_div, mv_filter, sync
✅ lfsr_8bit, lfsr_16bit
✅ delta_counter, shift_reg
✅ binary_to_gray, onehot_to_bin, cc_onehot
✅ lzc                      (BSlice clamp unblocked it)
✅ stream_filter
❌ popcount                 (recursive self-instantiation)
❌ gray_to_binary           (Vivado N=-1 default → empty netlist)
```

That's **17 cva6 leaf modules** formally equivalent.

## Fuzzy pairing across parameterizations

Verilator names parameterized modules with hash-style suffixes
(`popcount__I2`, `lzc__W40_M1`, `cva6_fifo_v3__pi19`); Vivado names
them `<base>__parameterizedN`. The driver pairs first by exact name,
then by *base name* (everything before `__`), and when multiple
parameterizations exist on the Vivado side picks the one whose
input/output port shape matches.

A whole-design sweep filtered to a few base names paired ~42
modules; verdicts depend on whether the matched parameterizations
agree functionally (different generic values would give different
outputs, so a mismatch surfaces as an honest counter-example).
