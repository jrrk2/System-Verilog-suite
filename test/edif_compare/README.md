# EDIF Match Tests

Tests that compare `sv_main_unified` netlist output against Vivado's RTL
elaboration EDIF using **two layers of comparison**:

1. **Z3 formal miter** (`test_xilinx_rtl_miter.exe`) — primary check.
   The two sides take *different* front-ends:

   * **Vivado side** (`<top>_elab.v` from `write_verilog` after
     `synth_design -rtl`) goes through **`ver_front`**, the in-tree
     OCaml Verilog parser in `vhd_libs/`. Its parse tree exposes
     bit-select pin connections (`TRIPLE(BITSEL, ID q, INT 2)`) as
     first-class structure, which lets `ver_front_to_behavioral`
     recombine Vivado's per-bit register cells (e.g. `q_reg[0]`,
     `q_reg[1]`, ...) into a single multi-bit `BSequential` process.
     This was the missing piece that the Verilator path couldn't bridge.
   * **Original SV side** still goes through `verilator --json-only` →
     `verilator_to_behavioral`. Always blocks, continuous assigns, and
     `RTL_*` cell instantiations are converted to BIR.

   `Z3_miter.check_miter_equivalence` is UNSAT iff the two are formally
   equivalent for all inputs. SAT prints a concrete counterexample.

   Cell-type normalisation (`RTL_REG_SYNC__BREG_4` → `RTL_REG_SYNC`,
   `RTL_OR4` → `RTL_OR`) lives inside `ver_front_to_behavioral`, so
   `compare.sh` no longer needs a sed pass.
2. **Cell-family count table** — secondary diagnostic. Useful when the
   formal check fails (or a converter doesn't yet handle the design)
   to see *where* structural mismatches sit. Per-instance numeric
   suffixes are normalised (`RTL_ADD0` → `RTL_ADD`).

When the converters can't encode a design (e.g. RTL_ADD with vector
ports isn't yet folded back into a vector expression in
`edif_to_behavioral`), the miter prints `⚠ converter limitation` and the
cell table provides the fallback view.

## Why elaboration, not synthesis?

`synth_design -rtl` (Vivado) performs RTL elaboration only and emits a netlist
of *generic* primitives (`RTL_INV`, `RTL_AND`, `RTL_MUX`, `RTL_ADD`,
`RTL_REG_SYNC__BREG_*`, `RTL_REG_ASYNC__BREG_*`, ...). Full `synth_design`
would map down to Xilinx technology cells (`LUT*`, `FDRE`, `CARRY4`, ...) which
is a much harder match target. Generic RTL is the right intermediate to aim for.

## Clock/reset classification

Clock and reset detection is purely structural — the classifier in
`sv_tran_struct.ml:classify_always_block` looks only at:

* **Sensitivity-list shape.** N edges → one is the clock, the other
  N−1 are async controls. The clock is the unique sensitivity-list
  signal not referenced as a reset condition in the body.
* **Body's top-level if/else-if chain (or folded ternary chain).**
  Each link of the chain whose condition is a single-signal predicate
  (`v`, `!v`, `v == K`, `v != K`) and whose then-branch assigns only
  constants identifies `v` as an async control. The first non-matching
  arm terminates the chain (it's the "real logic" branch).
* **Verilator may fold 1-bit if/cond chains into pure boolean logic,**
  which destroys the structural cue. Multi-bit registers preserve the
  chain — see `sr_ff8.sv`. The 1-bit set/reset case currently produces
  an Unsynthesizable warning with a pointer to widen the register.

Signal names are never inspected — patterns like `iRST`, `aresetn`,
`arst_b`, `n_clk`, `preset`, `clear`, etc. are classified the same as
conventionally-named signals.

Three-edge always blocks (set/reset flip-flops with both async preset
and async clear) are recognised; the structural backend currently emits
the *primary* async control (the first one in the if/cond chain) as the
register's RST input and warns that the secondary control isn't wired
into the cell.

## Blocking vs non-blocking semantics

Both `=` and `<=` inside an always block produce flip-flops by default —
the textbook "blocking → combinational" rule is wrong for synthesisable
code. The only semantic difference is **read-resolution within the
block**: after `x = expr`, reads of `x` inside the same block resolve
to the just-computed input wire (the FF's D side), not to the FF's Q
output. After `x <= expr`, reads of `x` continue to see the previous Q.

The single exception that eliminates a flip-flop is when the LHS of a
blocking assignment is **purely an alias** — every read of it gets
substituted away, and it's not observed outside the block. The DCE pass
in `sv_tran_struct.ml:module_dce` removes the resulting dead FFs to a
fixpoint.

This is implemented as:

* A `block_substitutions` map in the structural-context wiped at every
  sequential always-block boundary.
* `structural_expr` consults the map for VarRef reads while in
  sequential context.
* `structural_assign` updates the map after a blocking DFF emission;
  non-blocking emissions don't update it.
* `module_dce` post-pass drops register cells whose Q wire is never
  consumed (read), and prunes orphaned wire/var declarations.

See `blocking.sv`, `blocking_chain.sv`, `blocking_observed.sv` for tests
that lock in correctness for the alias case, the chained-substitution
case, and the externally-observed case respectively.

## Backends compared

* `xilinx_rtl` (new) — emits `RTL_INV`, `RTL_AND`, `RTL_OR`, `RTL_XOR`,
  `RTL_MUX`, `RTL_ADD`, `RTL_SUB`, `RTL_EQ`, `RTL_REG_SYNC__BREG_<W>`. It
  reuses the structural backend's transformations and renames cells + pins
  via `sv_gen_xilinx_rtl.ml`. A pre-rename fusion pass collapses the
  structural backend's `(~rst & d) → dff_en` chain into a single
  `RTL_REG_SYNC`, matching Vivado's representation of the same source SV.
* `structural` (existing) — generic primitives from `structural_primitives.sv`
  (`bitwise_not`, `bitwise_and`, `dff_en`, ...). Useful for seeing the
  pre-rename gap.

## Layout

```
test/edif_compare/
├── elab.tcl          # Vivado RTL elaboration TCL (writes <top>_elab.edf and <top>_elab.v)
├── compare.sh        # Run both flows for one test, print family table
├── run_all.sh        # Run compare.sh over the test set
├── stubs.sv          # Blackbox stubs for Xilinx primitives that the apb_uart
│                     # sources directly instantiate (e.g. RAMB16_S9_S9)
├── inverter.sv       # ~ -> RTL_INV
├── and2.sv           # & -> RTL_AND
├── mux2.sv           # ?: -> RTL_MUX
├── reg1.sv           # always_ff w/ sync reset -> RTL_REG_SYNC__BREG_1
├── add4.sv           # + -> RTL_ADD
├── sr_ff8.sv         # 8-bit set/reset FF (3-edge always) -> RTL_REG_ASYNC
├── blocking.sv          # tmp = a+b; q <= tmp -> RTL_ADD + RTL_REG, no FF for tmp
├── blocking_chain.sv    # chained = then <= -> aliases DCE'd
├── blocking_observed.sv # blocking LHS is a port -> FF kept (not pure alias)
└── apb_uart.srcs     # File list for the UART top (paths relative to here)
```

## Running

Requires Vivado at `/NFS/apps/Xilinx/Vivado/2020.1/bin/vivado` and a build of
`sv_main_unified` (run `dune build` from the repo root first).

```bash
./run_all.sh                 # all tests, xilinx_rtl backend
./run_all.sh structural      # all tests, structural backend (shows pre-rename gap)
./compare.sh inverter        # single test, xilinx_rtl
./compare.sh apb_uart        # the larger UART comparison
```

The first run on each design takes ~20–60s because Vivado has to elaborate;
the EDIF is cached and reused on subsequent runs unless any source changes.

## Output format

`compare.sh` prints the formal miter result first, then the family-level
cell-count table:

```
=== inverter (sv_main backend: xilinx_rtl) ===
Z3 miter:   ✅ FORMALLY EQUIVALENT (Vivado EDIF ≡ original SV)

cell family               | Vivado     | sv_main    | delta
--------------------------+------------+------------+-----------
RTL_INV                   | 1          | 1          | =
```

Possible miter verdicts:

* `✅ FORMALLY EQUIVALENT` — Z3 proved no input distinguishes the designs.
* `❌ NOT EQUIVALENT` — Z3 found a concrete counterexample (`<top>_miter.log`
  has the input/output values; the script also prints a one-line summary).
* `⚠ converter limitation` — one of the converters (EDIF→BIR or
  Verilator→BIR) couldn't encode this design. The cell-count table is the
  fallback signal in that case.

Vivado disambiguates per-instance for its viewer (`RTL_ADD0`, `RTL_MUX103`,
`RTL_REG_ASYNC__BREG_31`); these are normalised to family names before
counting in the table.

## Vivado-side front-end: VHDL → ver_front tree

The Vivado side prefers `write_vhdl` output (`<top>_elab.vhd`), routing
it through a thin translator (`vhdl_to_ver_front.ml`) that converts
Vivado's structural VHDL tree into the same `Vparser.token` shape that
ver_front produces for Verilog. The result is inserted into
`Globals.modprims` and processed by `Ver_front_to_behavioral.module_to_bmodule`
unchanged. Best of both worlds:

* **VHDL preserves vector ports** (`PRDATA : out STD_LOGIC_VECTOR (31 downto 0)`)
  — no bit-blasted `\PRDATA[N]` outputs, no `.NAME({...})` named-port
  shorthand. The apb_uart entity has 9 logical outputs, matching the
  orig SV.
* **All the per-bit register grouping** (`\q_reg[0]\` ... `\q_reg[3]\`
  → one multi-bit `BSequential`), `RTL_*` mapping, and bit-select
  handling already in `ver_front_to_behavioral` is reused without
  modification.
* The fallback `.v` path stays available — if `<top>_elab.vhd` is
  missing, `compare.sh` falls back to `<top>_elab.v` automatically.

The translator covers:
* Entity port declarations (with widths from `STD_LOGIC` /
  `STD_LOGIC_VECTOR(M downto L)` subtype indications).
* Architecture signal declarations.
* Architecture component instantiations (label, cell type, port map).
* Bit-select pin connections (`q(0)` → `BITSEL`).
* VHDL escaped identifiers (`\q_reg[0]\`) — backslashes stripped at
  identifier extraction so downstream sees plain names.
* `Rewrite.abstraction` is applied first to normalise the parse tree.

A `.tcl` change ships `write_vhdl -force <top>_elab.vhd` alongside
`write_edif` and `write_verilog`. In `vhd_libs/` only one small
addition was needed beyond the original tree types: a fix to
`semantics.ml` (extending the port-list handler with `DOUBLE(DOT, ID id)`
and `TRIPLE(DOT, ID id, _)` for the Verilog-2001 named-port shorthand,
which only matters when the .v fallback is in play; the VHDL form
doesn't use it).

## Alternative front-end: unisims library

For the *full* `synth_design` flow (no `-rtl`), Vivado emits
technology-mapped LUTs/FFs/DSPs/etc. Behavioural simulation models for
all 361 of those primitives live at
`/NFS/apps/Xilinx/Vivado/2020.1/data/verilog/src/unisims/`. Pointing
`verilator` at that directory (e.g. with `-y .../unisims +libext+.v`)
and feeding it Vivado's tech-mapped Verilog would produce a fully-
elaboratable netlist with no stubs needed. Trade-off: this exercises
synthesis-output equivalence, not the RTL-elaboration equivalence the
existing tests target — different scope, different gap to close.

## Current converter limitations

* **Vivado bit-blasted ports**: for output ports it can't keep
  vector-clean, Vivado emits one `\PRDATA[N]` port per bit alongside a
  Verilog-2001 `.PRDATA({...})` named-port binding in the module port
  list. The named-port shorthand itself is now handled (extension to
  `vhd_libs/semantics.ml`'s port-list handler accepting
  `DOUBLE(DOT, ID id)`), so `apb_uart` parses end-to-end. The remaining
  mismatch is structural — `apb_uart` shows 33 outputs in the Vivado
  side vs 9 in the orig — which the converter needs to consolidate by
  recognising `\PRDATA[i]` per-bit ports as bits of a single vector.
* **`verilator_to_behavioral` doesn't implement blocking semantics**
  the way `sv_tran_struct` does — within an always block,
  `lhs = expr` should make subsequent reads of `lhs` resolve to `expr`
  (not the FF's Q output), and `lhs` should still get a register unless
  it's a pure alias. The orig-SV side of `blocking_observed` exposes
  this gap.
* **Z3 shift encoding** in `z3_miter.encode_stmt` doesn't pad shift
  amounts to the data-path width — a 5-bit shift by a 1-bit constant
  fails sort-checking. Affects `blocking_chain`.
* **Verilator-folded 1-bit set/reset FFs**: covered by the classifier
  documentation; affects `sr_ff8`.

The big win from switching the Vivado side to `ver_front` is that
multi-bit registers now compose correctly — `blocking` (which expanded
to 4 per-bit cells in Vivado's output) now proves formally equivalent.

## Current results (snapshot)

| Test | Z3 miter | notes |
|---|---|---|
| inverter | ✅ formally equivalent | via VHDL path |
| and2 | ✅ formally equivalent | via VHDL path |
| mux2 | ✅ formally equivalent | via VHDL path |
| reg1 | ✅ formally equivalent | via VHDL path |
| add4 | ✅ formally equivalent | via VHDL path |
| blocking | ✅ formally equivalent | per-bit registers recombined |
| sr_ff8 | ❌ | original SV side: Verilator folds the 1-bit if/cond chain into pure boolean, erasing the structural cue |
| blocking_observed | ❌ | original SV side: `verilator_to_behavioral` doesn't yet implement the blocking-vs-non-blocking substitution semantics from `sv_tran_struct` |
| blocking_chain | ⚠ converter limit | Z3 `bvshl` shift-width sort mismatch when the shift amount is a 1-bit constant against a 5-bit data path |
| apb_uart | ⚠ converter limit | VHDL → ver_front tree → BIR all succeed end-to-end (entity has 9 logical ports, no `\PRDATA[N]` mismatch). Z3 encoding then throws a `bvand` sort mismatch (32-bit signal vs 64-bit RTL_* operation) — same family of width issue as `blocking_chain`'s `bvshl` |

6/10 formally equivalent end-to-end. The remaining 4 are real, distinct
gaps — three on the original-SV side, one in z3_miter's encoder.

Where Z3 reports `❌` it's overwhelmingly because of converter
under-constraint, not actual semantic divergence — the cell-count table
shows the same families on both sides for every small test. The framework
is honest about this: the formal verdict is the truth once the converters
catch up.

The UART (`apb_uart` + 11 dependency modules) closes a substantial portion
of the gap and surfaces the remaining real work:

| family | Vivado | sv_main | status |
|---|---|---|---|
| RTL_AND | 171 | 71 | shared (sv_main produces fewer; some `&` patterns inlined) |
| RTL_INV | 2 | 22 | shared (sv_main over-emits inverters) |
| RTL_MUX | 210 | 81 | shared |
| RTL_OR | 51 | 30 | shared |
| RTL_XOR | 16 | 15 | shared (essentially matched) |
| RTL_REG_ASYNC | 760 | 68 | shared family — count differs because Vivado expands per-bit; sv_main keeps multi-bit registers as one cell. The drop from 74→68 is DCE removing dead alias FFs. |
| RTL_EQ | 48 | 28 | shared |
| RTL_NEQ | 9 | 4 | shared |
| RTL_ADD | 15 | 0 | only in Vivado — structural backend's `adder` not firing on UART patterns |
| RTL_SUB | 6 | 0 | only in Vivado — same as ADD |
| RTL_GEQ | 1 | 0 | only in Vivado — `>=` not yet hit |
| RTL_BMERGE | 2 | 0 | only in Vivado — bit-merge concat not modeled |
| RTL_LATCH | 1 | 0 | only in Vivado — latch detection |
| RTL_ROM | 70 | 0 | only in Vivado — constant ROMs not detected |

`only in sv_main: <none>` — every cell family the xilinx_rtl backend emits
has a Vivado counterpart.

The 760 vs 74 register-count discrepancy reflects Vivado expanding multi-bit
registers into one cell per bit, while sv_main keeps each multi-bit register
as a single cell. A bit-aware comparator would normalize this.

## How the comparator works

For each test, `compare.sh`:

1. Resolves source files (from `<top>.srcs` if present, else `<top>.sv`).
2. Runs Vivado `synth_design -rtl` via `elab.tcl`, producing `<top>_elab.edf`.
3. Runs `verilator --json-only` (with `stubs.sv` for any directly-instantiated
   Xilinx primitives), then `sv_main_unified file <backend> ...`, producing
   `<top>_<backend>.v`.
4. Greps both files for primitive cells, normalizes per-instance suffixes,
   and prints a family-level count table.

Generated artefacts (logs, `obj_dir/`, `_elab.edf`, `_*.v`, `*_proj/`) are
not checked in.

## Extending

Add a small test: drop `mytest.sv` here and append `mytest` to the list in
`run_all.sh`.

Add a multi-file test: drop `mytop.srcs` (one path per line, relative to this
directory) and append `mytop` to `run_all.sh`.

Close part of the apb_uart gap: extend `sv_gen_xilinx_rtl.ml`. The map of
structural-primitive → RTL_* lives in `map_primitive`; closing the
`RTL_REG_ASYNC` gap requires plumbing async-reset edge information from
`sv_tran_struct.gen_dff_en` through to a new register variant.
