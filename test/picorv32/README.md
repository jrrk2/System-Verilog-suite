# picorv32 Equivalence Regression

[picorv32](https://github.com/YosysHQ/picorv32) is a small RISC-V CPU
that fits in a single Verilog file, with no package dependencies. That
makes it a good real-CPU regression target without the parametrization
hurdles cva6 has.

`picorv32.v` defines several modules, each tested separately:

| Module | What it teases out |
|---|---|
| `picorv32` | Full CPU — instruction decode, ALU, branch logic, memory IF, IRQ |
| `picorv32_regs` | 31×32 register file — RAM inference target |
| `picorv32_pcpi_mul` | RISC-V `mul`/`mulh`/`mulhu`/`mulhsu` (signed/unsigned variants) |
| `picorv32_pcpi_fast_mul` | Single-cycle multiplier alternative |
| `picorv32_pcpi_div` | `div`/`divu`/`rem`/`remu` |

## Setup

`<top>.srcs` files just point at `../../../picorv32/picorv32.v`.
`compare.sh`, `elab.tcl`, and `stubs.sv` are copies of the ones from
`test/edif_compare/` — no picorv32-specific changes needed.

```bash
bash compare.sh picorv32 xilinx_rtl       # full CPU
bash compare.sh picorv32_pcpi_mul xilinx_rtl
# ...
```

## Current results

| Test | Z3 miter | Notes |
|---|---|---|
| `picorv32` | ✅ FORMALLY EQUIVALENT | 11 shared cell families; `only in sv_main: <none>` |
| `picorv32_pcpi_mul` | ✅ FORMALLY EQUIVALENT | Multiplier coprocessor — signed and unsigned variants |
| `picorv32_pcpi_fast_mul` | ✅ FORMALLY EQUIVALENT | Single-cycle variant |
| `picorv32_pcpi_div` | ✅ FORMALLY EQUIVALENT | RISC-V divider |
| `picorv32_regs` | ❌ | RTL_RAM inference for the regfile (unsupported in converter — this is the same gap that affects cva6 RAM modules) |

## What this told us

The full picorv32 CPU elaborates cleanly via Vivado RTL elaboration
and our pipeline produces a formally-equivalent verdict against the
original SV. Vivado's elaborated form uses **20 distinct primitive
families** for picorv32; 11 are shared with the orig-SV side, 9 are
"only in Vivado". The "only in Vivado" families (`RTL_ARSHIFT`,
`RTL_GEQ`, `RTL_NEQ`, `RTL_RAM`, `RTL_REDUCTION_*`, `RTL_REG_SYNC`,
`RTL_ROM`, `RTL_RSHIFT`) are real outputs Vivado emits while
`verilator_to_behavioral` doesn't yet emit equivalent BIR for the
same source SV — these are gaps on the original-side converter, not
in the Vivado-side converter.

A few converter improvements landed during this regression:

* `z3_miter` now auto-extends operand widths in `BBinOp` (zero-extend
  smaller to match larger) — without this, large-width arithmetic in
  `picorv32_pcpi_mul` (where the accumulator is 64 bits but inputs
  are 32) hits Z3 sort errors on every `bvadd`. The fix is correct
  for unsigned bitvector ops; for signed ops the converter would need
  explicit sign-info propagation, but that's a separate problem (see
  `MULTIPLIER_NOTES.md`).
* `BCond` likewise zero-extends mismatched then/else widths and uses
  a same-width zero for the condition compare. Previously the fixed
  1-bit zero failed when the condition was a wider expression.

## Important caveat: verdict completeness

The picorv32 CPU's "✅ FORMALLY EQUIVALENT" verdict comes with 609
encoded constraints on the Vivado side vs 320 on the orig-SV side —
the orig side has roughly half the constraints because
`verilator_to_behavioral` doesn't extract every assign/always
sub-statement. So the verdict is a *partial* equivalence: every
output-driving relationship that IS encoded matches, but constraints
that aren't encoded on the orig side don't constrain the search.
This is a soundness gap on the orig-SV side, not a Vivado-side
issue.

The clean small-test verdicts (`mul_unsigned`, `add4`, `reg1`, etc.
in `test/edif_compare/`) are full equivalence; the picorv32 CPU
verdict is partial. Closing this requires extending
`verilator_to_behavioral` to capture more sub-statements (already-
known issue, see the existing blocking-semantics gap).
