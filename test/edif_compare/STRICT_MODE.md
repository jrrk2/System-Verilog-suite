# MITER_STRICT — surfacing converter gaps

By default the BIR converters are permissive: when they hit an
unrecognised VHDL/SV/cell shape, they fall back to a placeholder
(e.g. `BConst 0`, `BBlock []`, `EMPTY`, or `cell_to_bprocess` returning
`None`). This keeps the pipeline running on real designs but means a
"FORMALLY EQUIVALENT" verdict can be **vacuously true** — gaps in
extraction simply don't constrain the search.

`MITER_STRICT=1` flips silent fallbacks into hard failures. Run any
test under it and the first unhandled pattern raises with a
constructor-level diagnostic, telling you exactly what to teach the
converter next.

```bash
# Permissive (default) — formal pass, but possibly partial coverage:
bash compare.sh apb_uart xilinx_rtl
# → ✅ FORMALLY EQUIVALENT

# Strict — exposes the gap that the formal pass was hiding:
MITER_STRICT=1 bash compare.sh apb_uart xilinx_rtl
# → Fatal error: exception Failure("[vhdl_to_ver_front] unrecognised
#    actual_part in actual_to_vparser: Double (Vhd?<int=40>)")
```

## Where the strict checks live

* **`verilator_to_behavioral.expr_to_bexpr` and `stmt_to_bstmt`** —
  bail when an SV expression / statement constructor isn't in the
  dispatch. Diagnostic includes the constructor name (e.g. `BinaryOp`,
  `Concat`, `Begin`).
* **`vhdl_to_ver_front.actual_to_vparser`** — bails on unrecognised
  VHDL `actual_part` shapes. Diagnostic includes outer constructor
  (`Double`, `Triple`, ...) plus inner Vhd-tag, falling back to the
  runtime tag number when the constructor isn't in the named-tag list.
* **`ver_front_to_behavioral.cell_to_bprocess`** — bails when the
  RTL_*/cell type isn't in the dispatch table. Diagnostic includes
  cell type name and instance name.

In permissive mode all three behave as before. With `MITER_VERBOSE=1`
you can also get the same warnings logged without the failure — handy
for cataloguing what's missing without losing tests.

## Coverage added in this round

Starting from the gaps strict mode revealed, added:

* **`Double (VhdCharPrimary, Char c)`** in `actual_to_vparser` — VHDL
  character literals `'0'`/`'1'` Vivado uses for tied-low/tied-high
  pins (e.g. `RTL_ARSHIFT.I2 => '1'` for the signedness flag).
* **`VhdIntPrimary`, `VhdOperatorString`, `VhdAggregatePrimary`,
  `VhdActualOpen`** — additional VHDL literal/aggregate forms.
* **`VCC` / `GND`** in `cell_to_bprocess` — exact 1/0 constants.
* **`RTL_BMERGE`, `RTL_LATCH`, `RTL_ROM`** — approximate encodings
  documented inline (they emit a deterministic but not-functionally-
  exact constraint, so any test exercising them will counter-example
  rather than vacuously pass).
* **User-module instances** (anything that doesn't look like a
  Xilinx primitive) now skip silently in `cell_to_bprocess` — they're
  hierarchical references the miter handles by walking their entities,
  not behavioural cells.
* **`Var`/`Var'` declarations**, **`For`/`For'` loops**, **`Initial`/
  `Final`/`Display`/`Stop`** in `verilator_to_behavioral.stmt_to_bstmt`
   — Var decls and simulation-only constructs cleanly skipped, for-loop
  body walked once.
* **`Sel`/`ArraySel` LHSs** in Assign — recover the base name, emit a
  whole-vector assign (lossy approximation, surfaced via
  counter-examples).
* **Constructor-name diagnostics** — `vhd_inner_kind` now uses a named
  pattern match instead of `Obj.magic` runtime tags. Fragile coupling
  to declaration order removed.

## State after the round

| Test | Strict verdict | Notes |
|---|---|---|
| `inverter`, `and2`, `mux2`, `reg1`, `add4` | ✅ | unchanged |
| `mul_unsigned`, `mul_mixed`, `redand4`, `redor4`, `xnor2`, `neg4` | ✅ | unchanged |
| `ashr4` | ✅ | `VhdCharPrimary` case unblocked it |
| `blocking`, `blocking_chain` | ✅ | `Var`/`Sel`-LHS handlers unblocked |
| `apb_uart` | ✅ | **previously vacuous — now genuine.** RTL_BMERGE / RTL_ROM / Sel-LHS additions made the encoding complete |
| `mul_signed`, `concat2`, `bitsel4`, `blocking_observed`, `sr_ff8` | ❌ counterexample | real semantic gaps, *not* converter limits |
| `picorv32_regs`, `picorv32` (whole CPU) | ✅ parses through (RTL_RAM approximate) | RAM model is XOR(WD, RA) — counter-examples on actual reads |
| `picorv32_pcpi_div`, `picorv32_pcpi_fast_mul` | ✅ | clean |
| `picorv32_pcpi_mul` | Z3 sort error | for-loop unrolling → bad slice indices on the orig SV side |

## Round 2 — full cva6 design (144 entities)

Driving the converter against `test/cva6_ram/cva6_elab.vhd` (52 MB,
elaborated whole-core dump) under strict mode surfaced four more gaps,
each of which now has a converter case:

* **Escaped-name parity** — VHDL escaped identifiers `\foo__parameterizedN\`
  weren't being unescaped on the entity-declaration side, so 55 of the
  144 entities silently failed the entity↔architecture join. Single
  `unescape` call in `collect_entity` brings the count to 144/144.
* **`Double (Vhdwaveform_element, e)`** in `actual_to_vparser` — Vivado
  emits these for default-driven concurrent assignments. Recurse into
  the inner expression.
* **`RTL_BSEL`** (dynamic bit-select) — exact handler:
  `O = (I >> S) & 1`.
* **`RTL_RAM`** (distributed-RAM inference) — approximate handler:
  `RO1 = WD2 XOR RA1`. Same shape as `RTL_ROM` — strict-mode passes,
  any test exercising the RAM counter-examples (honest reporting).

Combined: `MITER_STRICT=1 test_cva6_sweep cva6_elab.vhd` runs to
completion across all 144 entities. Driver lives at
`test_cva6_sweep.ml`.

## cva6 leaf modules through formal miter

These cva6 sub-modules now pass `compare.sh <top>` end-to-end:

| Module | Source | Notes |
|---|---|---|
| `gated_clk_cell` | `core/cvfpu/vendor/openc910/.../gated_clk_cell.v` | trivial pass-through |
| `exp_backoff` | `vendor/pulp-platform/common_cells/src/exp_backoff.sv` | LFSR backoff timer; bit-blasted regs differ in count |
| `ct_vfdsu_ff1` | `core/cvfpu/vendor/openc910/.../ct_vfdsu_ff1.v` | priority encoder |
| `ct_vfdsu_pack` | `core/cvfpu/vendor/openc910/.../ct_vfdsu_pack.v` | sign/exp packer |
| `rstgen_bypass` | `vendor/pulp-platform/common_cells/src/rstgen_bypass.sv` | reset synchroniser bypass |

Modules that fail the formal miter (real gaps, not converter):

| Module | Reason |
|---|---|
| `popcount` | recursive self-instantiation — miter only encodes the top |
| `lzc` | nested `if/else` generate blocks — verilator → BIR drops a branch |
| `onehot_to_bin`, `binary_to_gray`, `gray_to_binary` | `genvar for` loops collapse to 0–1 constraints on the SV side |
| `delta_counter` | depends on `counter.sv`; standalone elab uses default params (mismatch) |
| `shift_reg` | internal generate-block FF state not encoded |
