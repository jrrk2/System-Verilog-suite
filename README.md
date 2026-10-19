# System-Verilog-suite

A multi-frontend SystemVerilog elaborator, **proof-driven synthesiser**, and
equivalence-checking pipeline written in OCaml. Pulls SV/Verilog/RTLIL/UHDM
and Vivado's post-elaboration structural VHDL through five distinct parsers
into a common Behavioral IR (BIR), runs SV-elaboration and post-elaboration
optimisation passes on it, lowers BIR through a hardcaml + Liberty-driven
tech mapper to ORFS-compatible cell-mapped Verilog, and proves equivalence
between any two BIR modules via Z3 SMT — including against its *own*
gate-level output and after cell-level architecture swaps in the placed
netlist.

Originally built to recover behavioural Verilog from synthesised netlists;
grown into a side-by-side correctness checker for SV elaboration, with
Vivado's elaborated form as the oracle; now also a verification-first
alternative to yosys+ABC inside OpenROAD-flow-scripts. The headline
result: a 24-bit MAC built from formally-verified 8-bit multiplier and
48-bit adder leaves verifies end-to-end against its cell-mapped output in
~5 min, where the monolithic 24-bit equivalence check doesn't converge.
On a smaller MAC laid out through ORFS, an analytical predict-swap
recommendation (ripple → sklansky adder, leaf-cert-gated) lands within
0.005 % of the measured post-CTS arrival.

Vivado is an SV consumer in this pipeline, not a VHDL consumer: we
feed it SystemVerilog, run `synth_design -rtl`, and use the
structural VHDL it emits (RTL_REG_*, RTL_AND, RTL_MUX, …) as a
post-elaboration intermediate language we can re-parse. So the
"Vivado VHDL frontend" reads Vivado's *output*, not the project's
*input*.

The project is structured around the BIR as the lingua franca: every
frontend's job is to land a `bprogram`; every pass operates on `bmodule`s;
every backend reads `bmodule`s.

## Frontends

| Frontend | Module | Reads | Notes |
|---|---|---|---|
| Verible | `verible_to_behavioral.ml` + `verible_elaborate.ml` | SV source via OCaml port of Verible's grammar | Full elaboration: parameter overrides, generate unrolling, const-fn evaluation, struct-typed parameters. `\`include` resolution is in-OCaml via `Sv_preproc` (no shell-out to verilator -E) |
| Verilator JSON | `verilator_to_behavioral.ml` | `verilator --json-only` AST dump | Already-elaborated, monomorphic. Walks procedural-scope `Var` declarations (named-begin / `unnamedblkN`) so locals carry source-stated widths into the BIR. Generic submodule `Cell`s become `binstance`s (`cell_to_binstance`) so the hierarchy survives into the BIR; Vivado RTL_* primitive cells still lower to processes |
| Slang | `slang_to_behavioral.ml` | `slang --ast-json` | Independent SV elaborator, parses CVA6 (handles type parameters Surelog can't); first-class Z3 oracle peer. Enum-typed signals are sized from their `EnumType` base via `build_enum_table` (keyed by module-qualified name so same-named typedefs in different modules don't collide) |
| sv-parser | `sv_parser_to_behavioral.ml` | dalance/sv-parser CST dump (`parse_sv -t`) | Independent CST parser as a third SV oracle. Extracts module instances (named + positional port maps → `binstance`), enum typedefs, and resolves positional connections to named form before flattening. Param specialisation across instances is still TODO |
| Vivado RTL VHDL | `vhdl_to_ver_front.ml` → `ver_front_to_behavioral.ml` | Structural VHDL emitted by Vivado's `synth_design -rtl` (after Vivado has parsed and elaborated the original SV source) | Treats RTL_REG_SYNC/ASYNC/_CE cells as `BSequential` processes; the structural oracle |
| GRLIB / VHDL native | `vhdl_to_behavioral.ml` | VHDL source via the `vhd_front` parser | Records, enums, conditional concurrent assigns. Emits one bmodule per entity and parses a file-list together (`convert_vhdl_files_to_behavioral`), so a top architecture's component instantiations resolve against sibling entities and flatten. 1072/1105 GRLIB files (97%) emit clean BIR; first Z3 ✅ EQUIVALENT on `eth_rstgen` against ghdl-synth (verilator/slang oracles) |
| Yosys RTLIL | `rtlil_to_behavioral.ml` | `read_verilog`-then-`write_rtlil` | Used for the four-way miter |
| Surelog UHDM | `surelog_to_behavioral.ml` | UHDM dump from Surelog | **Parked at port-surface only** — slang covers a strict superset (CVA6's type parameters work) and is already a Z3 oracle. Use slang for new oracle work |

Each frontend produces a `bprogram = { modules: bmodule list; library_cells: ... }`.
A `bmodule` is `{ name; params; signals; processes; instances; funcs; mems }`
where `processes` is a list of `BCombinational` / `BSequential` blocks
holding `bstmt` trees over `bexpr` trees.

**Uniform hierarchy contract.** Every frontend records the module
hierarchy as `binstance`s (instance name, child module, port
connections) rather than pre-inlining it — verible, slang, sv-parser,
verilator and native VHDL all populate `instances` on hierarchical
designs. Flattening is *not* destructive: `prep_for_z3` runs
`Behavioral_arch_subst.substitute_program` (the operator-architecture
swap — see below) on the recorded hierarchy, then
`Behavioral_hier.flatten_for_z3` produces a *transient* flat module for
the Z3 leaf check while the hierarchical `bprogram` is left intact. So
operator-architecture substitution (`sv_decomp_adder`/`sv_decomp_mul`
instances → abstract `BBinOp`) slots in at the instance boundary on the
recorded representation, ahead of and independent of the flatten.

## Verible elaborator (`verible_elaborate.ml`)

Handles SystemVerilog elaboration semantics that Verible itself wouldn't
get to without a separate elaborator front-end. Implemented as a series
of passes over the parsed token tree.

### Constant expression evaluator (`Eval`)

Recursive-descent integer evaluator over `deep_string_of_token`'s output.
Supports:

- Arithmetic: `+ - * / % << >>`
- Comparisons: `== != < <= > >=`
- Logical: `&& || !`
- Ternary: `? :`
- System functions: `$clog2`, `$bits`, `$signed`, `$unsigned`
- Sized literals: `<width>'<base><digits>` and bare `'0` / `'1`

Returns `int option` — `None` is the lattice top (don't know).
Operates on the deep-string form, so any token-tree shape that
`deep_string_of_token` can flatten is fair game.

### Constant function evaluator (`eval_function`)

Verilator-V3Const-style symbolic execution of pure functions returning
structs. Walks the function body collecting `<local>.<field> = <expr>`
assignments into an accumulating `SVStruct` value, with arg substitution
into the body's expression scope. Handles:

- Plain field assigns to a return-type local
- Function arg references inside the body (struct-typed args go through
  the `struct_table` redirect; int-typed args go into the local int scope)
- Nested struct literals as field values
- Cross-package localparam references via `lookup_pkg_localparam`

`sv_value` lattice domain: `SVInt | SVStruct | SVArray | SVUnknown`.
`SVUnknown` propagates: any unresolvable sub-expression makes the
enclosing field unknown but doesn't poison the whole struct.

### Module specialisation (`specialise_design`)

Walks from a chosen top, follows every `instantiation_base`, computes a
unique parameter value set per call site, and yields a flat list of
monomorphic `specialised` modules with deterministic suffix-encoded
names (`lzc__W4`, `popcount__INPUT_WIDTH8`).

For each visited module: builds an int-valued local scope (overrides +
folded localparams) and uses it to evaluate the if-condition of each
`conditional_generate_construct` via `walk_live`, recursing only into
the live branch. Records `(parent_specname, inst_label) → child_specname`
in `inst_specialised` so the BIR-level converter can rewrite each
`binstance.module_name` from the base to the specialised sibling.

### Generate-block pruning (`prune_dead_generates`)

Tree transformer: for each `conditional_generate_construct` whose
condition reduces in the current scope, replaces the dead branch(es)
with `EMPTY_TOKEN`. Run by `verible_to_behavioral.convert_module`
before extracting signals/assigns/processes, so a `popcount__W2`
specialisation doesn't carry assigns from the W==1 / W>=3 branches that
fight the W==2 branch. Default-on (set `DISABLE_GEN_PRUNE=1` to bypass).

## BIR-level passes

Each pass takes a `bprogram` and returns a `bprogram`. The full pipeline
applied by the comparator and miter drivers is:

```
unroll → inline → iflift → blocking_subst → meminfer → flatten
```

| Pass | Module | Purpose |
|---|---|---|
| `Behavioral_unroll` | `behavioral_unroll.ml` | Unroll `BFor` / `BWhile` loops with constant bounds; substitute the iteration variable into the body N times |
| `Behavioral_inline` | `behavioral_inline.ml` | Replace every `BCall` (function-call expression) and `BCallStmt` (task-call statement) with the corresponding `bfunc` body, formals→actuals substituted. Recursive functions guarded by depth limit |
| `Behavioral_iflift` | `behavioral_iflift.ml` | Lift `BIf` inside an always-block to dataflow form: `if (c) lhs <= a; else lhs <= b;` becomes `lhs <= c ? a : b;` |
| `Behavioral_blocking_subst` | `behavioral_blocking_subst.ml` | Forward-substitute single-write blocking-LHS variables within a `BSequential` body so the surviving non-blocking RHS is purely a function of FF outputs + primary inputs |
| `Behavioral_meminfer` | `behavioral_meminfer.ml` | Recognise array-as-RAM and array-as-ROM patterns, replace with `BMem` declarations |
| `Behavioral_flatten` | `behavioral_flatten.ml` | Inline combinational module instances into their parent: copy child's signals (renamed) and processes (port-substituted) into the parent, drop the instance |
| `Behavioral_share` | `behavioral_share.ml` | Post-FF-rip register sharing — collapse pairs of FFs whose D-cones are structurally identical |
| `Behavioral_ffrip` | `behavioral_ffrip.ml` | Convert each `BSequential` Q to a primary input `<Q>__Q`; emit each Q's next-state expression as a fresh primary output `<Q>__D`. Reduces equivalence checking to a combinational problem |
| `Behavioral_sanity` | `behavioral_sanity.ml` | Brain-dead semantic checks: duplicate signal declarations, multi-driver assigns, mixed proc/cont drivers |

## VHDL ↔ (System)Verilog cross-translate

The pipeline is symmetric: any frontend lands `bprogram`, any backend
reads `bprogram`. With `behavioral_to_vhdl.ml` mirroring
`behavioral_to_verilog.ml`, the same BIR drives both directions.

`convert_hdl.exe input.{vhd,v,sv} [-o output] [--top NAME]`:

- Auto-detects source / dest by extension; if `-o` is omitted, the
  output's language is the opposite of the input's.
- Preserves the leading copyright/SPDX/license comment block and
  re-prefixes its markers for the target (`-- ` ↔ `// `). Crucial when
  GRLIB / OpenCores / IETF-licensed sources are translated for use in
  toolchains that consume the other HDL.
- High-level processes survive intact: `BSequential` →
  `process(clk, rst); if rst='1' then RESET; elsif rising_edge(clk)
  then BODY end if;` (canonical async-reset idiom);  `BCombinational` →
  `process(all)` (VHDL-2008). `BIf` / `BCase` / `BFor` / `BWhile`
  preserved inside the process body — no flattening to nets.
- Verilog-side arithmetic operands cast to `(un)signed` for VHDL math,
  result cast back to `std_logic_vector` — the canonical synthesizable
  idiom that `ghdl-synth` / Vivado / DC accept.

Same operations are reachable from Lua via `svd.emit_verilog(h)`,
`svd.emit_vhdl(h)`, `svd.write_verilog(h, path)`, `svd.write_vhdl(h, path)`,
`svd.convert_hdl(in_path, out_path)`. Demo:
`test/lua/hdl_convert_demo.lua` exercises every binding end-to-end.

## Z3 miter (`z3_miter.ml`)

`check_miter_equivalence : bmodule -> bmodule -> bool`

Encodes both modules as bit-vector functions of their primary inputs
(after `ffrip`+`share`), checks that for every input both produce
identical outputs (and identical D-pin next states for every shared FF
name). Fast path for combinational designs; for sequential designs the
state correspondence is via Q-name matching — a precondition the FF-set
comparator (#63) is responsible for verifying first.

`Z3_MITER_TIMEOUT_MS=<ms>` overrides the default 30 s budget. Set
higher when checking parent modules with deep boundary cones.

Memories: an unpacked array register's next state is ffrip's
`@mem_store(mem, addr, data)` (each write port, enable-guarded), so write
ports are compared, not just reads.  Arrays of at least
`Z3_MITER_ARRAY_BITS` bits (default 4096; 0 = all) are Z3 arrays — reads
are `select`, writes `store`, the next-state compare is extensional — and
the miter then skips the CaDiCaL bit-blast and lets Z3 decide.  Smaller
arrays stay flat bitvectors with a per-word write mux.  A 32K x 36 RAM
(`ram32k_hi`) checks in ~20 s / 200 MB; flat it bit-blasted past 27 GB.

`SVS_MAX_MEM_MB=<n>` caps `sv_suite`: resident memory is checked after
every major GC (exit 3 with the call stack) and Z3 gets the same
`memory_max_size`, which a miter reports as INCONCLUSIVE.  Set it on large
or parallel runs.

### Compositional / hierarchical miter

`behavioral_boundary.ml` lets a parent module's miter substitute every
child instance with an uninterpreted-function `BCall`:
`BCall { func = "<child_module>__<port>"; args = input_ports }`. Both
sides of the parent miter encode the same Z3 `FuncDecl` (same name +
same args → same value), so the child becomes opaque without losing
soundness — the leaf certificate (proved separately) carries the
correctness obligation. Sequential children's outputs are promoted to
primary inputs instead, matching across designs by name (depth-1
sequential check).

This is what makes wide multipliers tractable. Z3 hits a wall around
10-bit raw multiplier equivalence (8-bit ≈ 144 s, 10-bit no-converge).
A 24-bit MAC built from 9 × `mul8` + 8 × `add48` + accumulator
verifies in ~5 min — each leaf is independently Z3-tractable, and
the parent miter only handles the surrounding bookkeeping.

The construction requires a buffering convention: every off-module
wire (instance output, instance input) goes through an explicit
single-driver `assign` so `hier_synth`'s phantom-IO promotion sees
unambiguous direction per net. See `test/cascade/cascade_mac.sv` for
the worked example.

## Synth pipeline (BIR → cell-mapped Verilog)

Driven by `synth_pipeline.ml` — the same code path is called both
from `synth_orfs_shim.exe` at ORFS make-time and from `orfs_prep.exe`
at pre-flight.  Stages run in this order:

| Stage | Module | Purpose |
|---|---|---|
| `Behavioral_to_hardcaml.create_circuit` | `behavioral_to_hardcaml.ml` | Lower a `bmodule` to a hardcaml `Circuit.t` — operators map to `Signal` ops, `BSequential` blocks to `Reg_spec`-clocked registers |
| `Lib_map.map_circuit` | `lib_map.ml` | Tech-map hardcaml's `Comb_op` / `Mux` / `Eq` / `Add` / `Sub` / `Mul` / `Lt` nodes onto Liberty cells. Bit-blasts adders/subtractors as ripple-carry (or Brent-Kung under `SV_DECOMP_ARCH_SWAP=1`), multipliers as Array (b_w iterations of `gen_add`), gates as `AND2_X1`/`OR2_X1`/`XOR2_X1`/`INV_X1`/`MUX2_X1`. Netlist DCE is on by default (`SV_DECOMP_NO_DCE=1` skips) |
| `Hier_synth.synth_program` | `hier_synth.ml` | Per-module gate-level synth in dependency order. Children get phantom IO at their parent so each module synthesises standalone, while the cell-Verilog emit step splices each instance line back in alongside the parent's cells. Preserves both hier (instance lines) and flat (gate cells) views.  `HIER_SYNTH_TRACE=1` logs which module is being lowered (useful for localising upstream BIR bugs) |
| `Mux_chain_flatten` (opt) | `mux_chain_flatten.ml` | Priority MUX2 chains → balanced one-hot AND-OR (depth O(log N)).  Off by default; flip on with `SV_DECOMP_MUX_FLATTEN=1`.  Constants routed through tie cells to keep DRT-0305 (POWER-net) clean |
| `Kary_merge` | `kary_merge.ml` | Collapse same-family AND2/OR2 chains and trees into AND3/AND4/OR3/OR4.  *Duplicating* absorb — a high-fanout 2-input driver is inlined into every consumer, then `dce_module` sweeps it if all consumers absorbed it.  Iterates to fixed point (cap 16, override `SV_DECOMP_KARY_MAX_PASSES`).  Slack-aware: `SV_DECOMP_TIMING_REF=<6_finish.rpt>` falls back to single-fanout absorb on cells that were on the prior run's worst path, so widening a chain head from `OR2_X4` (40 ps) to `OR4_X4` (72 ps) only happens off the critical path.  `SV_DECOMP_NO_KARY_MERGE=1` skips entirely |
| `Tie_fanout` | `tie_fanout.ml` | Splits over-loaded `LOGIC0_X1` / `LOGIC1_X1` cells before OpenROAD's `repair_tie_fanout` does.  `SV_DECOMP_TIE_FANOUT_MAX=N` (default 16); `SV_DECOMP_NO_TIE_FAN=1` skips |
| `Lib_size` | `lib_size.ml` | Liberty-driven drive-strength sizing using the same NLDM tables OpenSTA consults at signoff.  `SV_DECOMP_NO_SIZE=1` skips |
| `Mem_boundary_scan` (opt) | `mem_boundary_scan.ml` | Wrap black-box memory macros (and optionally every hierarchical child) with bit-level DFFs so ATPG can observe driver-side faults and control consumer-side logic across an unmodelled child. `SV_DECOMP_MEM_BSR=1` (macros only) / `SV_DECOMP_HIER_BSR=1` (all children). Port direction comes from each child's `mn_real_inputs`/`mn_real_outputs` |
| `Scan_insert` (opt) | `scan_insert.ml` | Swap `DFF_X1` → `SDFF_X1` (and `DFFR` / `DFFS` / `DFFRS` variants) and stitch every flip-flop in emission order into a single scan chain. Adds `scan_en`, `scan_in` inputs and `scan_out` output. `SV_DECOMP_SCAN=1` enables |
| `Jtag_tap_insert` (opt) | `jtag_tap_insert.ml` | IEEE 1149.1 TAP + boundary-scan cells on the chip top. Adds `jtag_tck`/`tms`/`tdi`/`trst_n` inputs and `jtag_tdo` output; wraps every other top-level IO with a BC\_1-style BSC (shift-mux + shift-FF + update-FF + output-mux, all from lib\_map primitives); appends the TAP RTL body to the output Verilog; writes a STD\_1149\_1\_2001-conformant BSDL file to `<out>.bsd`. `SV_DECOMP_JTAG=1` enables; `SV_DECOMP_JTAG_IDCODE=32'h...` overrides the default IDCODE |
| `Block_arch_swap.report` | `block_arch_swap.ml` | Predict-only report on which arith blocks would be candidates for an arch swap; actual swap is `SV_DECOMP_ARCH_SWAP=1` gated and cert-checked |
| `Cell_verilog_emit` | `cell_verilog_emit.ml` | Write OpenROAD-readable structural Verilog: bus reconstruction emitted as `assign bus = {bit_w-1, …, 1, 0};` so OpenSTA can trace each gate's full bus driver |

### Slack-feedback loop (`SV_DECOMP_TIMING_REF`)

Each structural pass uses the same Liberty NLDM tables OpenSTA uses,
so per-cell delay estimates agree at signoff.  Where the passes
diverge from STA is in *decision aggressiveness* — `kary_merge` will
otherwise greedily promote a chain head to OR4, even when the wider
cell's per-cell delay (~1.4× OR2) costs more than the eliminated
hop saves.

`SV_DECOMP_TIMING_REF=<path/to/6_finish.rpt>` closes that loop.  It
parses the worst-slack max-path block from a prior run, indexes
every hop's inst path (both full and leaf forms), and feeds the set
to `kary_merge` so cells *on that path* fall back to single-fanout
absorb.  Off-path cells keep the aggressive dup behaviour (area win
+ neutral-or-better depth).

Picosoc through nangate45 ORFS, 1.1 ns SDC:

| Variant | WNS | TNS | placements |
|---|---:|---:|---:|
| baseline (`SV_DECOMP_NO_KARY_MERGE=1`)            | -2.26 ns | -38.68 ns | 5817 |
| `kary_merge` on, no slack gate                    | -0.58 ns |  -4.62 ns | 4388 |
| `kary_merge` on, gate = baseline rpt              | -0.58 ns |  -4.62 ns | (same) |

picosoc's critical path is essentially single-fanout end-to-end, so
the gate is a functional no-op for this design (it matches 56/102
baseline leaves but they're already chain heads with single-fanout
drivers).  The gate's purpose is to keep the algorithm honest on
designs with shared-fanout subexpressions on the critical path
(CVA6, smollm); on picosoc we get the +1.68 ns WNS improvement from
the rewrite alone.

### `synth_orfs_shim.exe`

Drop-in replacement for the yosys+ABC step in OpenROAD-flow-scripts.
The patched ORFS Makefile (`test/orfs/decomp_synth.patch` — apply via
`test/orfs/install.sh`) routes `do-yosys` and `do-yosys-canonicalize`
to our shim when `USE_DECOMP_SYNTH=1`. No silent fallback to yosys —
on any failure the make target fails, and yosys remains available
only for explicit oracle/comparison runs (#101 in progress).

```sh
test/orfs/install.sh                                  # patch ORFS Makefile
cd $HOME/OpenROAD-flow-scripts/flow
USE_DECOMP_SYNTH=1 make DESIGN_CONFIG=designs/nangate45/gcd/config.mk
```

The companion `mac_synth_shim.exe` reads `MAC_WIDTH` / `MAC_MUL` /
`MAC_ADD` env vars and emits a `Synth_mac` netlist (used for the
multiplier-architecture demos — Wallace, Dadda, Brent-Kung,
Sklansky, Kogge-Stone alternates). All five arches sit on top of
the same bit-blasting primitives in `lib_map`; per-(arch, width)
correctness is proved standalone via `verify-arch`.

## FPGA synth (BIR → LUT/FF netlist → nextpnr → bitstream)

A second, independent back end in `fpga_synth/` (its own dune library
with `ppx_hardcaml`) targets Xilinx 7-series FPGAs through the **open**
nextpnr-xilinx + prjxray flow — no yosys, no VPR, no Vivado anywhere in
the path.  Where the ASIC pipeline above tech-maps to Liberty std cells,
this maps to LUTs + flip-flops.

| Stage | Module | Purpose |
|---|---|---|
| `Bir_to_aig.lower_circuit` | `fpga_synth/bir_to_aig.ml` | Bit-blast a hardcaml `Circuit.t` (from `behavioral_to_hardcaml`) into a 2-input And-Inverter graph: bitwise / add / sub / eq / lt / mul / mux / cat / select lowered to AND2 + edge inversions, hash-consed. Registers are boundaries — a FF's Q becomes an AIG primary input, its D-cone an AIG primary output — with clock + async-reset nets returned for later stitching |
| `Lut_cover.cover` / `map_to_luts` | `fpga_synth/lut_cover.ml` | FlowMap / ABC-`if`-style LUT covering specialised to k≤6 (a truth table is one `Int64`): k-feasible cut enumeration (dominance prune + 8-cut priority cap), per-cut truth-table eval via elementary masks (= the `LUTk` INIT), area-flow cut selection |
| `Fpga_map.map_lowered` | `fpga_synth/fpga_map.ml` | Stitch the LUT-mapped combinational logic + one FF per register bit (FDRE, or FDCE when the boundary carried an async reset) into a single `Circuit.t`, closing register feedback through wires. Folds `~`-inversions on outputs / FF-D into the driving LUT's INIT when fanout-safe. `~io` wraps top pads in IBUF / OBUF (clock through IBUF + BUFG) |
| `Xil_prim` | `fpga_synth/xil_prim.ml` | Black-box Xilinx-primitive instantiation helpers (LUT1-6 with INIT, FDRE / FDCE, IBUF / OBUF / BUFG) via the hardcaml v0.17.1 `Instantiation.create_with_interface` idiom |
| `Fpga_emit.write_yosys_json` | `fpga_synth/fpga_emit.ml` | Direct `Circuit.t` → yosys-`write_json` netlist for nextpnr, with **no yosys process**: walks the graph, allocates net ids, emits each primitive as a cell (INIT etc. serialised from `Parameter.Value`), residual inverters as `LUT1 #(.INIT(2'b01))`. Native Verilog emit (`emit_verilog`) also available |

End to end, no proprietary or third-party synth tool in the loop:

```
SystemVerilog/BIR → hardcaml → AIG → lut_cover (+inverter fold)
  → LUT+FF Circuit → write_yosys_json → nextpnr-xilinx → FASM
  → prjxray (fasm2frames + xc7frames2bit) → .bit [→ uf2conv → board]
```

`fpga_synth/nextpnr_validate.sh` drives it (env-parameterised
`CHIPDB`/`DEVICE`/`JSON`/`XDC`; `MAKE_BIT=1` adds the prjxray FASM→bit
stage, `UF2=1` packages a Sonata UF2).  The nextpnr chipdb is built once
from the bundled prjxray-db (`bbaexport.py` → `bbasm`).  nextpnr-xilinx
is the flow that consumes a fresh prjxray DB *directly*, so it is the one
aligned with in-house virtex7 fuzzing — the f4pga/VPR eblif path was
evaluated and dropped.

**Validated** on xc7z020 (`test_artyz7`) and xc7a50t (`test_sonata`):
nextpnr places & routes at 250 MHz, and the xc7a50t bitstream is
**hardware-confirmed on a Sonata board** (a 4-bit ~1 Hz LED counter).
Scope boundary: `bir_to_aig` currently raises on memories / submodule
instances, so block-RAM / DSP / hierarchy inference is the next step
before a full SoC (picosoc) rides this path.

### FPGA flow defaults, QoR harness and correctness gates (2026-10)

**One profile record.** Every knob of the FPGA flow lives in
`fpga_synth/fpga_opts.ml` (`Fpga_opts.get ()`), printed once per run as
`[fpga_opts] profile=...`.  The default profile is the measured-best one;
`SVS_PROFILE=legacy` restores the old everything-off behaviour, and each
knob still accepts its environment override (`GATE_MAP_BIR_OPT=0`,
`MEMLOWER_FPGA=0`, `LUTCOVER_BACKEND=native`, `GATE_MAP_DSP=0`,
`GATE_MAP_MODE=mixed`, `BALANCED_CASE=1`, ...).  Defaults:

| Knob | Default | Why |
|---|---|---|
| `bir_opt` (const-prop + constant-register fold + DCE before lowering) | on | picorv32 2088 -> ~1300 LUT; the single largest lever |
| `fpga_mem` (memlower -> RAM32M / RAMB instead of ASIC macros) | on | picorv32's register file: 1024 FDRE + muxes -> 12 RAM32M |
| `dsp_infer` (`BMul` -> DSP48E1, `Behavioral_dsp_infer`) | on, min width 4 | cascade_mac: 9 DSPs instead of ~1000 LUTs of array multiplier |
| `backend` (LUT cover) | `abc` when a `yosys-abc` is found, else native | same AIG through abc `if`: 853 vs 1244 LUT on picorv32 |
| abc script | resyn2 + `dch -f; if -K 6; mfs` (delay, then area recovery) | uart_transmitter depth 10 -> 4 for +3 LUT; `GATE_MAP_ABC_AREA=1` for area-only |
| `balanced_case`, `lutpack`, `mfs2`, `aig_balance` | off | measured worse on area with no depth gain |

**`svd.dsp_infer(prog)`** replaces each `a * b` whose operands fit a
DSP48E1 (signed 25 x 18 / unsigned 24 x 17, context width <= 48) by a
multiply-only DSP48E1 instance; the product is read back as a slice of P.
Verilog's rule applies: signed only when both operands are signed.  The Z3
side gets the matching model (`Xil_prim_models` "DSP48E1" and the
hierarchical-miter expansion in `Fpga_prim_expand`), so a DSP netlist still
proves against the source.  Not yet: >25x18 splitting, register absorption
(AREG/MREG/PREG), the P += A*B accumulate form.

**`test/qor/`** — the QoR + correctness regression (`run_qor.sh`,
`designs.tsv`, `dsp_selftest.sh`, README there).  Per design it maps with
this flow and with `yosys synth_xilinx` (SV read through the yosys-slang
plugin, or sv2v with `YOSYS_SV2V=1`), reports LUT / FF / CARRY4 / DSP / RAM /
MUXF / SRL and LUT depth, and gates the numbers on two formal checks: a
three-way source vote (Verible vs yosys-slang RTLIL vs Verilator JSON, pairwise
`svd.miter_hier`) and a hierarchical miter of the mapped netlist against a
fresh parse of the source.

Three defects the harness exposed and that are fixed:

- **Non-blocking slice / bit writes** (`r[k] <= v` after `r <= e` in the same
  clocked block) were lowered as a read-modify-write of the register's Q,
  overriding the pending whole-register write.  `Behavioral_nba_slice` (run
  from `Behavioral_iflift`) threads the pending value instead.  Found on
  `slib_counter`; it also restored picorv32's 64-bit cycle/instruction
  counters, which had been silently lost.
- **Signed arithmetic leaves were not widened** to the context width by the
  Verible frontend when the binop's result was already tagged with that width
  (`y[8:0] = a[4:0] * b[3:0]` multiplied unsigned).  Attributed by the vote:
  yosys-slang and Verilator agreed, Verible differed.
- **The BIR constant propagator** tracked `r := c` in clocked bodies with
  blocking semantics and kept the else-branch state after an `if`; both could
  prune live branches.  Clocked bodies no longer track (only `blocking_vars`),
  and the state after `if`/`case` is the meet of all paths.  Case arms whose
  constant key can never equal a constant selector are now dropped, and
  `x & 0` folds with `x` unknown.

Two ASIC-side sweeps silently removed logic and are fixed too: `Lib_map.dce`
never visited the per-bit drivers of a bus whose bare name was marked (every
multi-bit register's D cone was swept: picorv32 shipped 284 of 1565 FFs), and
`Kary_merge.dce_module` dropped every bus wire declaration.  Both netlists are
now complete and yosys-readable; the ASIC flow's area (3x yosys) and runtime
(picorv32 ~12 min) remain the next ASIC items.

**Operator sharing by common-operand hoisting** (`Behavioral_share_accum`, in
the pre-mapping BIR optimisation; `GATE_MAP_BIR_NOSHARE=1` skips).  A register
updated in many mutually exclusive arms (`sp <= sp + 1` / `sp <= sp - 3` /
`sp <= sp + imm` across a state machine) is threaded to one next-state tree and
the shared operand is hoisted out of the mux: `sp + (s==A ? 1 : s==B ? -3 :
... : 0)`, one adder fed by an operand mux instead of one adder per arm.
Constant subtractions become two's-complement additions; the hold leaf is
absorbed as `+ 0`; a bare two-constant up/down pair is left alone (two
half-adder chains beat an adder plus mux).  Exact algebra on the threaded
tree, verified by the netlist miter.  `test/qor/bir_share_scan.py` (over a
`GATE_MAP_BIR_JSON=<dir>` dump of the optimised BIR, printed by
`Behavioral_json`) reports the remaining opportunities per module: operator
inventory, mutually-exclusive same-op operators, mux-fed identical operators,
duplicate subexpressions, over-wide reads, constant-compare decoders.

**Structural restructuring before and during mapping.**  Each item has its own
off-switch so it can be A/B'd:

- *Flatten combinational children* (`svd.flatten` in the QoR flow, as
  `synth_xilinx -flatten` does; `QOR_FLATTEN=0` keeps them).  A generic child
  mapped once and instanced N times never sees the constants its parent feeds
  it: cascade_mac's `add48` got the zero-padded partial products as full
  48-bit operands, eight times over.
- *CARRY4 trimming* (`Bir_to_aig`, `CARRY4_TRIM=0`).  The chain covers only
  the span whose carry is unknown: below it, bits with a constant operand that
  pins the carry are plain XORs; above it, bits where both operands are
  constant are functions of the span's carry-out.  A short residual span
  against a constant (a 32-bit-context counter or decrement) ripples in LUTs
  (`CARRY4_TRIM_MIN`, default 8).
- *Carry-block CSE* (`CARRY4_CSE=0`).  A CARRY4 with the same carry source and
  the same S / DI literals as an emitted one is reused, so chains sharing a
  prefix share it: signed and unsigned compares of the same operands, and
  `a < b` with `a - b`.
- *Add/sub fusion* (`Behavioral_share_accum.fuse_addsub`,
  `GATE_MAP_BIR_NOADDSUB=1`).  `c ? x - y : x + y` becomes
  `(x + (c ? ~y : y)) + c`, and the trailing 1-bit add is fused into the
  chain's CYINIT (`CARRY4_FUSE=0`): one adder instead of two and a mux
  (picorv32's `alu_add_sub`).
- *Gated single-add hoist* (`Behavioral_share_accum`).  `c ? X : X + y`
  becomes `X + (c ? 0 : y)` when `X` is not the register being written,
  because a hold of another signal is a mux no clock enable absorbs.  The gate
  folds into the adder's per-bit LUT (cascade_mac's `dclr` accumulator: 48
  LUT3s).
- *Add-chain rebalancing* (`Behavioral_add_tree`,
  `GATE_MAP_BIR_NOADDTREE=1`).  Same-width add trees through single-reader
  wires are rebuilt balanced, pairing leaves by alignment (known trailing
  zeros) so each adder stays as narrow as the trimming allows.
- *Best-of-rounds ABC* (`Abc_cover`, `SVS_ABC_ROUNDS`, default 4).  ABC runs
  resyn2 + `dch; if -K 6; mfs` repeatedly in one process and writes the
  network after every round.  The flow keeps the smallest result no deeper
  than round 1.  ABC is chaotic on these netlists: the same picorv32 AIG gives
  1173 / 1215 / 1263 LUTs at 3 / 2 / 4 rounds.  Taking the best round means
  that noise can only help, and any single-run LUT difference under about ±50
  on picorv32 is noise.
- *Mux trees as AND-OR* (`Bir_to_aig`, `MUX_ANDOR=0`; trees of at least
  `MUX_ANDOR_MIN` leaves, default 6).  A case/if chain arrives as nested 2:1
  muxes, one per arm per bit.  Its root-to-leaf path conditions are mutually
  exclusive and exhaustive, which no cover can see, so the tree becomes
  `OR_j (sel_j & V_j)`: one path literal per leaf shared by every bit, one
  select per distinct leaf value, constant leaf bits reduced to plain ORs of
  selects, and the value most paths reach (usually the hold) as the default
  that needs no select of its own.  Exact for all inputs, so the
  combinational miter proves it.  picorv32 1212 -> 1109 LUTs (native cover
  1816 -> 1454), the OCaml VM 12732 -> 9387.
- *Wide-multiply DSP split* (`Behavioral_dsp_infer`, `DSP_SPLIT=0`).  A
  demanded-bits scan narrows a product to the bits anybody reads (the VM's
  72-bit `mul_p` is only read as `mul_p[34:0][31:0]`).  The operands are cut
  to that width and split into 24 x 17-bit unsigned limbs, one DSP48E1 per
  limb pair below the cut, with the partial products added in fabric (at most
  `DSP_SPLIT_MAX`, default 8, DSPs per multiply).  The VM's 36 x 36 multiply
  goes from 1094 LUTs to 3 DSPs plus 55 LUTs.  A multiply by a constant never
  becomes a DSP, because it folds to shifts and adds.  `dsp_selftest.sh`
  proves the split with `DSP_SPLIT_LIMBS=5,4` standing in for 24,17.
- *Blocking assignments as D-side reuse* (`Behavioral_blocking_subst`,
  `BLOCKING_SUBST_OLD=1` restores the heuristic).  In an `always_ff` every
  assigned variable is a flip-flop; a blocking `=` only means that a later
  read in the same body sees the expression being built for D rather than Q.
  The pass threads each blocking variable's pending expression through the
  body (into every if/case arm, merged on the way out), substitutes it at
  those reads, keeps the writes, and names the end-of-body value once as
  `v__d` so a value with many readers (the VM's RAM port temporaries) is one
  wire, not a copy per reader.  Registers whose Q nobody reads then fall to
  DCE: the VM lost its 200 memory-port FFs.
- *Registered-read block RAM ports* (`Behavioral_memlower`,
  `MEMLOWER_NO_REGREAD=1`).  `if (re) q <= mem[a];` is a RAMB36E1 port: the
  address goes to the tile in the cycle, `EN = re | we`, and q IS the tile's
  output register.  The write and the read at the write address share port
  A, a second read takes port B.  Port A's WRITE_MODE is proved: NO_CHANGE
  when `re & we` is unsatisfiable (a SAT query over the module's
  combinational definitions with every register free), READ_FIRST when
  `we & ~re` is; otherwise, or when q is also written elsewhere, q is rebuilt
  exactly as `re_d1 ? rdata : q__reg`.  Before this the FF stayed behind the
  tile (a second cycle of read latency) and both read ports shared one
  address -- the BIR-level models cut RAMB36E1 as a black box, so only the
  gate-level oracle below could see it.
- *Operators narrowed to the demanded bits* (`Behavioral_wreduce`,
  `GATE_MAP_BIR_NOWREDUCE=1`; yosys's wreduce).  `pc := (pc + offset) - 1`
  with a 24-bit pc is two 24-bit adders, not 32-bit ones; the demand flows
  through muxes, slices and concatenation parts.
- *Clock enables from the whole next-state tree* (`Bir_to_aig`,
  `CE_LIFT_TREE=0`, `CE_LIFT_MIN_W`, default 2).  A threaded tree keeps the
  register (`: accu`) in every arm that does not write it; CE becomes the OR
  of the paths to the other leaves and D the tree with the hold leaves
  replaced by the most common value.  yosys does the same through opt_dff
  and had been winning on exactly this: the VM's `accu` had CE tied high
  and 2400 LUTs in its D cone.  VM 8818 -> 7937 LUTs on that BIR (8737 after
  the fixes below changed it: ABC chaos, see the table note), picorv32
  1081 -> 1029.
- *ABC's duplicate LUTs merged* (`Abc_cover`, `ABC_NODEDUP=1`).  abc's `if`
  copies a node into every cut that uses it and the copies come back as
  distinct nets with the same function of the same inputs; one LUT per
  (truth table, inputs) now.  The VM had 10344 of them among 19165.
- *A gate-level oracle* (`test/qor/gl_cosim.sh <file> <top> [cycles]`).  The
  source RTL and the suite's netlist, the latter with Xilinx's own unisim
  models (three idioms patched for Verilator, cached in
  `~/.cache/svs-unisims-*`), driven by the same random inputs under
  Verilator with every output compared every cycle; `GL_STAGE=bir` simulates
  the post-memlower BIR instead, which splits the BIR passes from gate_map.
  It is the only check that sees memory and DSP mappings: `augment_xil_models`
  and `bmc_sim` cut RAMB36E1 as a black box.  `~/svs-gatesim/tools/gatesim.sh`
  runs the full-size VM against its RTL testbench the same way.

The oracle found, and the three-way vote then confirmed fixes for: dead-code
elimination dropping an emptied `case` arm next to a non-empty default (the
arm's opcodes took the default: `sysver_tests/case_dead_arm.sv`); a function
local declared with a width and initialised from an `int` argument keeping
the argument's 32 bits (`Make_header` lost its size field:
`func_local_width.sv`); `$signed(a) < $signed(b)` compared unsigned by all
three front ends (`signed_cmp_branch.sv`: the Verible post-pass, yosys's
`A_SIGNED`/`B_SIGNED` and Verilator's `LTS` forms now build the compare as a
sign-extended, sign-flipped unsigned one, `integer`/`int` declarations and
function returns counting as signed); a task whose body is only `<=`
assignments classed as a no-op and its calls dropped, and `flush();` parsed
as a bare identifier (`case_task_const.sv`: the VM's C_CALL1 arms); a
function's return width taken from the first range in its declaration, its
first parameter's (`integer Int_val(input logic [35:0] v)` was 36 bits wide,
so the signed compare extended only the other operand:
`branch_pc_share.sv`, the VM's BLTINT..BGEINT); an arithmetic operator over
a function call typed 1 bit wide because the call's width was not yet known,
so the call-argument normaliser zero-padded it and `Val_int(-Int_val(accu))`
spilled its sign into the word's metadata bit (`val_int_neg.sv`, the VM's
NEGINT/ADDINT: calls now carry their function's return width, and an unknown
width defaults to 32); and `width_of_expr_with` giving a mux the width of
its then arm; and the Hardcaml lowering's constant-narrowing rule for binary
operators cutting a constant to the other operand's width whatever its value
(`32'd8 * str_byte` with a 2-bit register became `0 * str_byte`, a constant,
so the VM's GETSTRINGCHAR shifted by nothing, read byte 0 of every string
word, and the now-unread `str_byte` register vanished: `strget_reg.sv`; the
constant now keeps at least the bits its value needs).  Each has a
`sysver_tests/` design that the three-way vote and
`gl_cosim.sh` both cover; `ram32k_regread.sv` / `ram32k_hi.sv` cover the
32K-deep x1 RAMB36E1 tiling the full-size VM uses.

### IR invariants, one lowering, and the writer as an oracle

Running the oracles on cva6 (`test/cva6/run_cva6_oracles.sh`: verilator
elaborates the core, the suite's verilator front end writes every specialised
module back out as behavioural Verilog, and that Verilog is what the strict
miter, the three-way vote and the unisim co-simulation then check) found
that most of what failed was scaffolding, not synthesis.  Three things now
make that class of failure visible at the source instead of in
`create_circuit`:

- *`Behavioral_check`* (`svd.check_ir(prog [, "unrolled"])`, and run on every
  front end's output, `SVS_CHECK_IR=0|verbose|strict`).  The invariants the
  passes rely on, written down: every name read or written is declared
  (ports, signals, mems, a function's formals, locals and result, a for-loop's
  induction variable); every call names a module function with its arity or
  an `@`/`$` intrinsic; a write pseudo-op's first argument is the *name* of a
  declared signal; every type has a positive width; no duplicate declarations;
  a clocked process names a declared clock and only lists `blocking_vars` it
  assigns; after unroll, no loop remains.  Warnings (several drivers, a slice
  past a signal's width) print under `verbose`.  On its first run it reported
  the verible front end dropping `for` loops inside functions (the body
  assignment was collected alone, `i` unbound), function locals never
  declared, and a function's result variable not being an indexable target
  (`rev[i] = d[7-i]` returned one bit).
- *`Behavioral_lower`*, the lowering every front end shares, applied in
  `post_load`: one loop shape (`init; while` becomes `BFor`), one return
  shape (`f = e; return` becomes `return e`), one bit-write shape (a
  constant-index `@mem_write` on a vector becomes `@slice_write(v, k, k, d)`,
  also after unroll turns `i` into a constant), empty blocks dropped, and the
  signed-compare lowering re-exported from `Behavioral_const`.  The Z3 fold
  (`Behavioral_ssa`) understood `@slice_write` and not the other, so a
  `y[3] = 1'b1` in an `always_comb` was a *free variable* to the miter: the
  roundtrip below caught it.  `Behavioral_ir.is_write_op` names the four
  write pseudo-ops once; blocking substitution matched two of them and
  substituted the just-assigned constant for the *target* of the other two.
- *The writer roundtrip* (`test/qor/roundtrip.sh <frontend> <top> <files>`,
  a column of `run_qor.sh`, `test/regressions/run_roundtrip.sh` over the
  whole `sysver_tests/` corpus for verible, yosys and verilator): a front
  end's tree, written with `svd.write_verilog` and read back by verible, must
  miter EQUIVALENT to the tree.  Nothing downstream can be trusted on a dump
  until this passes.  It found the writer emitting no function definitions,
  `while` loops verible could not read back, struct-typed ports (`parameter
  type`) collapsing to one bit in the verilator front end, block locals and
  loop variables undeclared, early `return`s dropped (verilator lowers them
  to an assignment plus a jump), loop control written non-blocking inside
  clocked blocks, and `(a[i]) >> j` sliced as if it were an l-value.

The harness asks the BIR instead of scraping text: `svd.ports`,
`svd.clocks` (clock and reset names with their edges) and `svd.children`
replace the regexes in `gl_cosim.sh` / `gen_tb.py` and the hand-written
children column of `designs.tsv` (`auto`).  Two bugs below the scaffolding
came out of the same run: constant propagation substituted the constant a
vector had just been assigned for the *target name* of its bit writes, so
`y = 0; y[i] = a[j]` mapped to a netlist with `y` tied to ground and `a`
dropped (`sysver_tests/comb_bitwrite_const.sv`; every earlier binary does
this), and the hierarchical miter left a 1-bit net declared `logic [0:0]`
undriven on one side (`obuf_` reconstruction skipped width 1), a spurious
DIFFER on cva6's `amo_buffer`.  The unroller now finds an `init; while` pair
at any position of a statement list, not only at the head of a block, and the
inliner reduces a function whose result is set by `return` inside `if`/`case`
arms (`func_return_case.sv`, `while_loop_reread.sv`).

The roundtrip also tests the miter against itself: a design mitered with its
own re-parse must come out EQUIVALENT, and `blocking_threads` did not.  Two
defects in the Z3 path: `Behavioral_share` merged registers with identical
next-state cones and aliased a merged-away output port to `<canon>__Q`, a
name ffrip only gives a *port* register (an internal one keeps its bare
name), so the port was left free and the miter disagreed with itself; and
ffrip read every right-hand name as a register's Q, so a blocking `t = a;
y <= t` in a clocked block gave `y__D = Q(t)` on both sides, consistently
wrong and different from what the gate lowering builds.  The miter now
threads blocking variables (`Behavioral_blocking_subst`) before SSA, and
a sink module with no outputs on either side (pulp's `unread`) is
vacuously EQUIVALENT instead of UNCOMPARABLE.  The verilator front end
scalarises 2-D+ unpacked arrays on their inner dimensions (`bht_q[63:0][1:0]`
becomes `bht_q__0` / `bht_q__1`): flattening the inner index into the word
turned two non-blocking writes to different elements of one word into two
read-modify-writes, and the second dropped the first.  `gl_cosim.sh
GL_NET_V=<dump.v>` co-simulates a front end's dump against the RTL Verilator
reads itself -- the importer oracle that confirmed it.

The first full run of the roundtrip column found two more.  The writer
escaped a hierarchical name (`\u.RST `, what yosys's flattening leaves)
everywhere except the sensitivity list and the reset test it writes from
the process header, so verible read `posedge u.RST` as a member select and
the importer crashed.  Behind that, the RTLIL importer's copy-propagation
folded the alias `u.RST = RST` into the child's reset *test* but left the
process header saying `async u.RST`: ffrip could not find the reset in the
body, and the writer printed the header's name over the body's, so the
dump said something the tree did not.  A clock or reset is a name, and any
pass that renames wires must rename it with them; `Behavioral_check` now
rejects an async-reset process whose body's outer `if` does not test the
header's reset (`sysver_tests/child_async_reset.sv`).  A verdict is also
only as good as the solver behind it: the miter falls back from CaDiCaL to
Z3 silently when the binary is not found, and cascade_mac's three-second
proof became a 120 s INCONCLUSIVE in a shell without it.

The same column showed the RTLIL importer's alias folding never finishing
on picorv32 (2,800 cell-output wires): it substituted to a fixed point,
duplicating every shared subtree at every reader.  It now resolves each
alias once, memoised, and folds a wire into its readers only when that
cannot grow the tree -- the wire is read once, or it resolves to something
tiny (`!rst_n`, a slice, a constant); a clock or reset only when it resolves
to a plain wire.  picorv32: 0.01 s, 2,829 processes to 610.
`SVS_RTLIL_TIMING=1` prints the per-module split and
`SVS_RTLIL_NO_COPYPROP=1` skips the pass, for the next such diagnosis.

With that read finishing, the yosys roundtrip of picorv32 DIFFERed on
`mem_la_wdata` at `mem_wordsize == 3`, a state the source marks `full_case`
and never reaches.  yosys renders the unassigned arm as a bit-mask mux that
feeds the variable back to itself, verible reads a case without default,
and both encode as `x = f(x)`: a fixed-point equation the solver may
satisfy differently on each side.  The miter (`z3_miter.ml`,
`break_comb_feedback`, after SSA) now reads a combinational target's
pre-write value as `x__pre`, an undriven internal that the existing tie
matches by name across the two designs, so both sides are compared as the
same function of the same old value -- a latch's state, which is what an
unassigned arm is.  `sysver_tests/comb_feedback_case.sv` is the four-line
version; its yosys roundtrip DIFFERed before and does not now.

Measured with every check proven equivalent (`test/qor/run_qor.sh`; the picorv32
range reflects ABC noise):

| design | before: LUT / CARRY4 / depth | after: LUT / CARRY4 / depth | yosys: LUT / CARRY4 / depth |
|---|---|---|---|
| uart_transmitter | 34 / 0 / 4 | 26 / 0 / 3 | 38 / 0 / 5 |
| uart_receiver | 64 / 11 / 4 | 46 / 0 / 3 | 63 / 6 / 3 |
| cascade_mac (9 DSP) | 483 / 109 / 15 | 155 / 52 / 20 | 326 / 25 / 16 |
| picorv32 | 1299 / 76 / 15 | 1109 / 56 / 15 | 1039 / 97 / 16 |

Checking the flattened MAC.  The hierarchical check can no longer abstract
children, and a flat miter with nine bit-blasted DSP models per side does not
finish within 300 s per solver.  The QoR flow therefore flattens the reference
too and runs the same `dsp_infer` on it.  `MITER_CUT_PRIMS=DSP48E1` then makes
`Fpga_prim_expand` treat each DSP as an uninterpreted function of its inputs on
both sides, which is sound by congruence.  The DSP-vs-`*` step itself is
proven by `test/qor/dsp_selftest.sh`.  CaDiCaL closes the flat MAC proof in
3 s.

**Large benchmark: the OCaml bytecode VM** (`~/bytecode/ocaml4142_vm_rtl.sv`,
4k lines, one module, 96-state machine, three memories of 32K/256K/8K x 36).
No yosys reader takes it (yosys-slang: blocking-after-non-blocking to `sp`;
sv2v: `bit is_block[int]`; yosys's own: always-block nesting), so it is mapped
through the Verilator front-end (`svd.parse("verilator", ...)`).  Getting it
through needed: `+incdir+` of each source's directory and `+define+SYNTHESIS`
on the Verilator call (and `SYNTHESIS` predefined in the Verible preprocessor,
as every synthesis tool does); task-call statements and `x++` in the Verible
frontend; function locals separated from formals and their initialisers kept
(`INITIALAUTOMATICSTMT`) in the Verilator converter; the inliner tolerating
dropped simulation statements; and BRAM **depth tiling** in
`fpga_bram_resolve` (a bank of RAMB36E1 tiles behind an address decode with a
registered read-select per port; the 256K-word heap becomes 288 tiles).

Measured (xc7, Verilator front-end, abc backend; no yosys reference exists
for this source):

| OCaml VM | LUT | FF | CARRY4 | RAMB36E1 | depth | wall |
|---|---|---|---|---|---|---|
| default flow | 18604 | 2518 | 2676 | 333 | 44 | 13.4 min (abc) |
| + operator sharing (58 add/sub folded) | 17796 | 2518 | 2471 | 333 | 41 | 9.9 min |
| Verible front-end, same flow (65 folded) | 18989 | 2264 | 4029 | 333 | 39 | 12.4 min |
| + carry trim/CSE, add/sub fusion, add trees, best-of-4 ABC | 12930 | 2264 | 1095 | 333 | 38 | 7.3 min |
| + wide-multiply DSP split (3 DSP48E1) | 12732 | 2266 | 1117 | 333 | 31 | 6.9 min |
| + mux trees as AND-OR (`test/qor/run_vm.sh`) | 9387 | 2266 | 1101 | 333 | 33 | 1.3 min |
| + group sharing, blocking-aware threading | 8889 | 2249 | 960 | 333 | 33 | 1.1 min |
| + registered-read RAM ports (correct latency, two read ports) | 9194 | 1898 | 990 | 333 | 36 | 1.0 min |
| + wreduce, clock enables from the tree (`CE_LIFT_MIN_W=2`) | 7937 | 1894 | 988 | 333 | 34 | 1.0 min |
| + the front-end and lowering fixes below (final; every check green) | **8737** | 1899 | 994 | 333 | 36 | 1.0 min |
| yosys 0.4x `synth_xilinx` (sv2v, 5.5 min) | 11308 + 520 INV, 725 MUXF | 1769 | 869 | 296 | 30 | |

The netlist of the final row, simulated with the unisims under the VM's own
Verilator testbench (`~/svs-gatesim/tools/gatesim.sh`), runs helloworld, fact
and hellofor cycle-exactly as the RTL does (the state/pc/accu log of every
cycle identical, HALT at the same cycle); before the fixes below helloworld
alone matched.  The small-memory VM (`vm_cosim`-sized) and a medium one with
the full 32K stack match the RTL for 20000 random cycles at both the BIR and
the netlist stage.

The yosys row is the first reference for this source: yosys-slang rejects it
(blocking and non-blocking writes to `sp`), sv2v then `read_verilog` takes it.
Its 296 tiles are 4K x 9 RAMB36E1s behind a 64:1 output mux per bit, which is
where most of its extra LUTs and all its MUXF7/8 go; the x1 tiling here
spends 37 more tiles and no mux.  Its 177 FFs with a constant CE against our
942 before the tree lift was the clock-enable gap above.  ABC is chaotic at
this size: the same BIR mapped at `CE_LIFT_MIN_W` 1 / 2 / 4 gave 8816 / 7937
/ 8865 LUTs, so VM LUT totals are judged with FF and CARRY4 counts and the
per-register cones, not alone.

(The rows from "+ carry trim" down were mapped from one Verible elaboration;
the 12732 row still turned two constant multiplies into DSPs, which the
constant guard has since removed.)  `test/qor/attrib.py` charges the LUTs to
the registers they feed.  The largest remaining sinks are `accu` (1076), the
heap write data and address (`hm_wd_a` 610, `temp_heap_addr` 531,
`hm_addr_a` 447) and `pc` (377).

**The Verible route now takes the VM too.**  It needed: the constant evaluator
folding user-function calls in localparam initialisers (`VAL_UNIT =
Val_int(0)`: `fname = expr` function bodies, `|` `&` `^` `~` `<<<` `>>>`,
concatenation and replication, folded on the typed tree with the module's
parameters in scope; `eval_cw` no longer mistakes a call's argument for its
value); task calls that Verible parses as instantiation-shaped nodes
(`stack_write(sp-1, accu);`, 50 of 52 call sites were silently dropped); and
initialised local declarations in function bodies (`logic [15:0] wosize16 =
wosize;`) becoming assignments the inliner can bind.  The two elaborations
agree on the named state: the Verible side additionally folds `heap_lo` and
`gc_semi` (written only in reset, from parameter-constant values), and the
Verilator side carries ~230 bits of anonymous temporaries.  A whole-module Z3
vote between the two did not finish in 40 minutes; the 96-state single module
needs a per-state or bounded decomposition before it can be proven.

The scan says what is left: 862 arithmetic/compare/shift operators (weighted
bit-cost 18443), of which the mutually-exclusive sharing estimate is a
further ~27 % once operators with different left operands are shared through
full input muxes; `state == K` for 96 constants is the decode structure;
`(!st_re_a & !st_we_a)` repeats 51 times (the task guards, deduplicated by
AIG hashing).

**Inout pads** (`Behavioral_iobuf_hoist`, run by `gate_map` and by the
miter's `prep_for_z3`): a behavioural tristate drive on an `inout` port
(`assign io = oe ? din : 1'bz`; the frontend now keeps `'bz` as an `@z`
marker) is hoisted onto one IOBUF black box per bit -- `io__o` / `io__t`
drive it, every read of the pad becomes the sensed `io__i`.  The mapped
netlist carries the same instance, so `cut_blackboxes` compares each pad as
three signals (drive, direction, sense) with the pad pin itself left to the
outside world.  `recipes/xil_models_selftest.lua`'s `inout_bidir` case, which
failed with "unbound identifier io" before, now proves EQUIVALENT (20/20).

**`svd.miter_strict(a, b)`** makes an output present on one side only a
DIFFER instead of a silently skipped signal; mapped-netlist register names are
preserved by default (`FPGA_LEC_NAMES=0` disables) so state pairs by name.

Measured with `test/qor/run_qor.sh` (xc7, k=6, default profile, abc backend; yosys 0.64 `synth_xilinx -noiopad -noclkbuf` as reference; depth = LUT/MUXF/CARRY4 levels between registers):

| Design | this flow (LUT / FF / CARRY4 / DSP / RAM / depth) | yosys synth_xilinx | source vote | netlist check |
|---|---|---|---|---|
| uart_transmitter | 34 / 7 / 0 / 0 / 0 / 4 | 38 / 16 / 0 / 0 / 0 / 5 | 3/3 | 1/1 EQUIVALENT |
| uart_receiver (+3 children) | 64 / 25 / 11 / 0 / 0 / 4 | 63 / 36 / 6 / 0 / 0 / 3 | 3/3 | 4/4 EQUIVALENT |
| cascade_mac (24-bit MAC) | 483 / 51 / 109 / 9 / 0 / 15 | 326 / 51 / 25 / 9 / 0 / 16 | 1/3 (2 inconclusive: 8x8 multiply exceeds the Z3 budget) | 3/3 EQUIVALENT |
| picorv32 | 1299 / 593 / 76 / 0 / 12 RAM32M / 15 (1377 / 84 before operator sharing) | 1039 / 574 / 97 / 0 / 12 RAM32M / 16 | 0/3 (no pair finished in the 40 min budget: whole-core miter) | 1/1 EQUIVALENT |

The yosys FF counts on the UARTs include its one-hot FSM recoding.  Before this
work the same flow gave 49 / 84 / 1641 / 4365 LUTs on these four designs, and
the earlier "853 LUT" picorv32 figure was produced by the unsound propagator
and is withdrawn.  Remaining gaps: the MAC's CARRY4 count (the 48-bit adders
are not yet absorbed into the DSPs), picorv32 +34 % LUTs, and FSM one-hot
recoding / `unique case` handling for control-path depth.


## DFT (scan + ATPG + JTAG / BSDL)

The synth pipeline carries an optional DFT track that turns any
design into a scan-flattenable, ATPG-friendly, JTAG-accessible chip.
All four passes are env-gated and compose freely:

| Env flag | Pass | Effect |
|---|---|---|
| `SV_DECOMP_SCAN=1` | `scan_insert.ml` | Functional DFFs → scan SDFFs, single chain in emission order |
| `SV_DECOMP_MEM_BSR=1` | `mem_boundary_scan.ml` | BSCs around every memory-macro port |
| `SV_DECOMP_HIER_BSR=1` | `mem_boundary_scan.ml` | BSCs around every hierarchical child instance (superset of `MEM_BSR`) |
| `SV_DECOMP_JTAG=1` | `jtag_tap_insert.ml` | IEEE 1149.1 TAP + per-pad BSCs at the chip top, BSDL sidecar emitted |

`test_atpg.exe` ties them together: it runs the full synth pipeline,
forces `SV_DECOMP_SCAN=1`, then runs bit-parallel fault simulation
(`fault_sim.ml`) on each module's gate netlist.  Three-tier ATPG:

1. **Random patterns** — 64-pattern-wide ints, `n_pattern_words` of
   them per module (default 16 → 1024 patterns).  `gate_sim.ml` does
   the bit-parallel propagation.
2. **Directed pass** (`atpg_directed.ml`) — for each fault not yet
   covered, backwards-justify a PI assignment that activates the fault
   site, randomise the remaining inputs, re-simulate.  Catches deep
   AND/OR chains whose 1/2ⁿ random hit rate makes pure random
   ineffective.
3. **PODEM** (`atpg_podem.ml`) — 5-valued logic ({0, 1, X, D, D̄}),
   D-frontier maintenance, decision-stack backtracking.  Off by
   default; `SV_DECOMP_PODEM=1` enables; `SV_DECOMP_PODEM_BUDGET=<N>`
   caps per-fault decisions (default 64); `SV_DECOMP_PODEM_TIMEOUT_MS`
   wall-clocks per fault.  The objective picker handles AND/OR/NAND/
   NOR/XOR/XNOR/INV/BUF/MUX2/AOI21/OAI21; XOR / MUX support is what
   lifts coverage on adder-heavy and mux-cascade designs (`alu4`
   14 % → 98 % after the picker fix).

### Coverage results

Measured with `SV_DECOMP_SCAN=1 SV_DECOMP_HIER_BSR=1 SV_DECOMP_JTAG=1
SV_DECOMP_PODEM=1`, 16 pattern-words (1024 random patterns):

| Block | Cells | Faults | Coverage |
|---|---:|---:|---:|
| `dffe_basic` | 13 | 8 | 100.00 % |
| `gcd` (top) | 115 | 230 | 100.00 % |
| `distributed_dual_port_ram` | 4622 | 9178 | 99.96 % |
| `picosoc_mem__W256` | 281 | 342 | 99.42 % |
| `gcd` / LtComparator | 100 | 196 | 98.98 % |
| `single_port_bram` | 70 | 138 | 98.55 % |
| `sdff_basic` | 33 | 64 | 98.44 % |
| `alu4` | 94 | 180 | 98.33 % |
| picorv32 CPU | 12 646 | 23 802 | 94.04 % |
| `aes_sbox` (8→8 LUT) | 6 241 | 12 482 | 93.99 % (PODEM 352 / 1102) |
| `spimemio` | 1 395 | 2 534 | 92.42 % |
| `simpleuart` | 1 832 | 3 400 | 92.32 % |
| `fifo_v3_small` | 89 | 176 | 88.64 % |
| `picosoc` (top, all DFT) | 2 287 | 4 156 | 85.23 % |
| `aes_rcon` | 535 | 1070 | 72.99 % |

Picosoc top coverage progresses with each DFT layer:

| Configuration | picosoc top coverage |
|---|---:|
| no DFT | 59.51 % |
| `SCAN` | 59.51 % (same — coverage limited by black-box children) |
| `SCAN` + `MEM_BSR` | 59.51 % (SRAM is inside a wrapper — wrapper itself unchanged) |
| `SCAN` + `JTAG` | 64.44 % (BSCs themselves all testable; +506 faults all detected) |
| `SCAN` + `HIER_BSR` | 83.18 % (boundary FFs around CPU/UART/SPI/MEM unblock internal cones) |
| `SCAN` + `HIER_BSR` + `JTAG` | **85.23 %** |

The residual ~15 % at picosoc top is scan-shift infrastructure
(`clk`/`reset`/`scan_en`/`scan_in` fanout, tie-cell paths) plus a
handful of structurally unreachable faults — fundamentally not
addressable through combinational ATPG.  `aes_rcon`'s low 72.99 % is
the same kind of structural unreachability: 24 of every 32 output
bits are constant-zero by design, so stuck-at-0 faults on those bits
have a fault-equals-good-value relation and are flagged "undetected"
where an industrial tool would report "untestable".

The pipeline carries the full chip-level structure: scan chain, mem
BSR, hier BSR, JTAG TAP + 126-bit boundary-scan register on picosoc.
The BSDL output passes `STD_1149_1_2001` boilerplate and is consumable
by external JTAG tools.

### GUI hook

`sv_gui.exe` exposes the four toggles + ATPG runner under a top-level
**DFT** menu:

```
DFT
├─ [ ] Scan-chain insert (SDFFs + chain)
├─ [ ] Memory boundary scan (around macros)
├─ [ ] Hier boundary scan (around every child)
├─ [ ] JTAG TAP + BSDL pads at chip top
├─ ───────────
├─ Run ATPG…       (renders coverage report in the text view)
└─ Show BSDL…      (auto-finds the most recent .bsd sidecar)
```

The check-items toggle the env vars at runtime; flipping them and
re-running ATPG picks up the new configuration without restarting.

## Cell-mapped equivalence (`test_synth_equiv.exe`)

Closes the verification loop: prove that the cell-mapped Verilog we
just emitted means the same thing as the source SV. The cell file
goes back through Verible into BIR (with auto-generated module stubs
for each Liberty cell so Verible's elaborator counts them as
instances), then `Gate_netlist_to_behavioral` expands each cell
using the Liberty `function` / `next_state` strings into BIR
expressions. The result is paired by module name with the source
BIR and run through `Z3_miter`. With boundary substitution applied
to both sides, parent modules verify by composition over their
already-proven leaves.

Headline numbers (single laptop, NangateOpenCellLibrary_typical):

| Design | Modules | Result |
|---|---|---|
| `gcd` (10 modules incl. RegEn / RegRst / Subtractor / Mux / LtComparator / ZeroComparator / Dpath / Ctrl / top) | 10 | 10 / 10 formally equivalent |
| AES (`aes_sbox`, `aes_rcon`, `aes_key_expand_128`, `aes_cipher_top`) | 4 | 4 / 4 formally equivalent (`Z3_MITER_TIMEOUT_MS=120000`) |
| `cascade_mac` (24-bit MAC = 9 × mul8 + 8 × add48 + 51-bit accumulator) | 3 | 3 / 3 formally equivalent in 5 min |
| 8-bit MAC through ORFS to post-CTS, then ripple → sklansky swap | n/a | predicted 592.7 ps · measured 592.67 ps · error -0.005 % · cert OK |

## Tools and entry points

Built via `dune build`. The most-used executables:

| Executable | Source | Use |
|---|---|---|
| `convert_hdl.exe` | `convert_hdl.ml` | VHDL ↔ (System)Verilog cross-translate; license-header-preserving, `--top` auto-detected from `module <NAME>` |
| `test_z3_oracle.exe` | `test_z3_oracle.ml` | **Unified pair-Z3 harness.** `--oracle <fa> --peer <fb>` selects any pair from {verible, slang, yosys, verilator, vhdl, surelog}. Replaces the per-pair test drivers (`test_slang_vs_verible`, `test_yosys_vs_verible`, `test_verilator_vs_verible`) with one knob |
| `test_yosys_oracle_sweep.exe` | `test_yosys_oracle_sweep.ml` | Parallel-correctness sweep across a corpus. `--oracle / --peer` for any frontend pair; `--incdir` threads through to verilator and our `Sv_preproc`. Two modes: per-file iteration (auto-top), or one elaboration set per `--top` |
| `test_cva6_slang_diff.exe` | `test_cva6_slang_diff.ml` | Slang ↔ Verible per-entity miter on the cva6 flat SV. Slang as the primary elaborator; pairs by base-name + port-shape across both frontends' specialisations |
| `test_grlib_vhdl_z3.exe` | `test_grlib_vhdl_z3.ml` | GRLIB bottom-up Z3 sweep: per leaf, our VHDL→BIR vs ghdl-synth→Verilog→{verilator JSON, slang}→BIR through the Z3 miter |
| `test_uart_vhdl_5way_miter.exe` | `test_uart_vhdl_5way_miter.ml` | 5-way cross-language miter: all four SV frontends (verible/slang/verilator/sv-parser) plus a parallel VHDL implementation, ten pairwise Z3 miters. Loads every `.vhd` in the top's directory so the VHDL hierarchy flattens. Used to drive the apb_uart convergence (all five frontends flatten to ~103-109 registers) |
| `test_dump_four.exe` | `test_dump_four.ml` | Side-by-side BIR dump from verible/slang/verilator/sv-parser for one design — the first stop when a miter pair disagrees |
| `test_vhdl_bir_dump.exe` | `test_vhdl_bir_dump.ml` | Single-VHDL-file BIR dump. `--multi <top> <files>` walks the instance graph for hierarchy dumps |
| `test_cva6_ff_diff.exe` | `test_cva6_ff_diff.ml` | Per-entity FF-set diff between Vivado's elaborated form and Verible-elaborated SV. `MITER_Z3=1` runs Z3 on FF-matching pairs |
| `test_cva6_bottom_up.exe` | `test_cva6_bottom_up.ml` | Pairwise Z3 miter for every Verilator-JSON module against the matching entity in Vivado's elaborated VHDL output |
| `test_verible_to_bir.exe` | `test_verible_to_bir.ml` | Verible-side BIR dump for a single SV file. Useful for debugging elaboration. `--miter <vhd>` adds Z3 against a Vivado entity |
| `ff_stats.exe` | `ff_stats.ml` | Run all four frontends on a single testcase and report each one's Q__Q / Q__D set, with pairwise overlap numbers |
| `random_sv_gen.exe` | `random_sv_gen.ml` | Constrained-random SV generator. `--features mixed` rotates through nine modes |
| `sv_suite.exe script <file.lua>` | `sv_suite.ml` + `sv_lua.ml` | Run a Lua script with the `svd.*` API exposed (parse / pick / miter / gate_miter / liberty / expand / bir / insts / timing / emit_verilog / emit_vhdl / write_verilog / write_vhdl / convert_hdl) |
| `test_atpg.exe` | `test_atpg.ml` | Run the DFT pipeline (forces `SV_DECOMP_SCAN=1`, honours `SV_DECOMP_MEM_BSR` / `HIER_BSR` / `JTAG` / `PODEM`) on a fileset and print a per-module coverage report. `--top <name>` and `--words <N>` (default 16 = 1024 random patterns) |
| `sv_gui.exe` | `sv_gui.ml` | lablgtk3 shell. **DFT menu** toggles the four env flags + runs ATPG and shows BSDL. Also has Decompile (Parse Verible/Slang/yosys/verilator/VHDL), Verify (Z3 miter, Cyclesim), and Topology (Run ORFS layout, Open ORFS run) menus. Lua scripts in `gui_scripts/*.lua` auto-register additional menu items via `gui.add_item` |

## Test infrastructure

### `test/regressions/` — hand-written SV regressions

Each `.sv` exercises a specific elaboration / converter pattern. The
ones added to track the const-fn evaluator chain:

- `struct_param_const_fn.sv` — minimal `cfg_pkg::mk_cfg()` returning a
  packed struct used as a parameter default
- `recursive_popcount.sv` — popcount-shape recursive instantiation,
  base case at W=1; verifies generate pruning + recursive flatten
- `struct_literal_arg_ternary.sv` — struct literal `'{W: 64}` as
  function arg, ternary in the function body
- The earlier set covers comma-list ANSI ports, packed-struct member
  access, RAM-pattern inference, etc.

### `test/random/fuzz_const_fn.sh` — constrained-random fuzzer

Drives `random_sv_gen` at four `cfg_*` modes:

| Mode | What it stresses |
|---|---|
| `cfg_struct` | const-fn returning struct, struct-field access on parameter default |
| `cfg_chain` | cross-package localparam — pkg A's localparam referenced from pkg B's const-fn body |
| `cfg_ternary` | ternary inside const-fn body, function-arg substitution, comparison ops |
| `cfg_recursive` | recursive instantiation with parameter-driven base case |

Run: `bash test/random/fuzz_const_fn.sh [n_seeds]` (default 25 seeds
per mode = 100 cases). Failures get a `.sv` saved under
`test/random/found_<mode>_<seed>.sv` for triage.

Result with prune-default-on: 100/100.

### `test/sv_tests/` — chipsalliance/sv-tests integration

Three runners for the upstream sv-tests corpus:

- `decompiler_verible_parse` — Verible→BIR conversion alone passes
- `decompiler_verilator_parse` — Verilator→JSON→BIR conversion alone
- `decompiler_miter` — both converters AND Z3-equivalence

Wrapper scripts pre-flatten via `verilator -E` for tests that need
include / define expansion. A `Vivado_Synth_RTL` column on the same
dashboard shows the `synth_design -rtl` ground-truth verdict per test.

See `test/sv_tests/README.md` for installation and full instructions.

### `test/cva6_ram/` — CVA6 hierarchical equivalence

Real-design driver. Vivado was fed CVA6's SystemVerilog source, ran
`synth_design -rtl`, and dumped the resulting structural VHDL to
`cva6_elab.vhd` (144 elaborated entities). The driver pairs each of
those entities against the corresponding Verible-elaborated
specialisation by base name + port-shape. The wrapper
`test/cva6_ram/run_ff_diff.sh` builds the flattened SV (verilator -E
with pragma + assertion stripping) for the Verible side and drives
the FF-set comparator end-to-end.

Current numbers (after the prune-default-on flip):
- 26 / 144 entities show FF-set parity with Vivado
- 2 / 5 FF-bearing pairs prove formally equivalent via Z3

The 117 unpaired entities are dominated by parameterised common-cells
modules whose port shape doesn't match any Verible specialisation —
either because the field-value resolution chain hits an unknown
localparam or the converter's port-width derivation doesn't yet
follow some particular package-constant pattern.

## Build

### OCaml toolchain

- OCaml **5.3.0** (older 4.x will not work — uses 5.x effects bindings
  in hardcaml + 5.x exception payloads).
- **dune** ≥ 3.0
- **menhir** ≥ 2.1 (parser generator, used for the OCaml port of
  Verible's grammar and for the Liberty / LEF / DEF parsers)

### OCaml packages (opam)

| Package | Purpose |
|---|---|
| `str` | stdlib regex (POSIX-style) — Verible token tree handling |
| `yojson` | parsing Verilator's `--json-only` AST dump |
| `ppx_deriving_yojson` | auto-generate yojson serialisers for BIR types |
| `unix` | stdlib syscalls (subprocess management for the Verilator/Yosys/Slang frontends) |
| `hardcaml` | the synthesis lowering target — `Signal.t` graph + `Circuit.create_exn` |
| `hardcaml_circuits` | upstream Wallace/Dadda multipliers and Kogge-Stone-style adders that hardcaml ships; we exercise these as an alternative to our own bit-blasted gen_mul/gen_add |
| `z3` | OCaml bindings for the SMT solver — drives every formal-equivalence step |
| `lua-ml` | scripting layer for the optional `sv_lua.ml` opaque-handle interface (#77) |
| `linenoise` | line editing for the interactive REPL drivers (`interactive_client*.exe`) |

Install via:
```sh
opam switch create 5.3.0       # if not already present
eval $(opam env --switch=5.3.0 --set-switch)
opam install dune menhir yojson ppx_deriving_yojson \
             hardcaml hardcaml_circuits z3 \
             lua-ml linenoise
```

The Z3 OCaml binding links against `libz3.so` at runtime; on Debian /
Ubuntu install `libz3-dev`, on macOS `brew install z3`.

### Vendored sub-libraries (built as part of `dune build`)

- `vhd_libs/` — VHDL front-end (parser + lexer + dump). Used by the
  Vivado-RTL-VHDL frontend.
- `ver_libs/` — Verilog front-end alternative parser, kept for the
  ver_front compatibility test (`test_ver_front_parse.exe`).
- `lef_def/` — LEF + DEF parsers, the `Synth_mac` cell-level netlist
  builder, the placement-aware delay model, and `Predict_swap` /
  `Fanout_cone` for the post-layout resynth path.

These live in-tree; nothing to install separately.

### External tools

Required for the test suites listed alongside each tool:

| Tool | Required for | Optional? |
|---|---|---|
| **Verilator** ≥ 5.024 | Verilator JSON frontend; pre-flatten wrapper for the sv-tests integration | required for the four-way miter; otherwise optional |
| **OpenROAD-flow-scripts** (recent) | layout demos (gcd, MAC), cell-mapped equivalence using Nangate45 cells, post-CTS arrival measurements | required for `test_synth_equiv.exe` against ORFS-emitted netlists; otherwise optional |
| **Slang** | Slang SV frontend; primary peer for the unified Z3 oracle harness; cva6 slang-primary sweep | recommended for new oracle work |
| **GHDL** ≥ 4.0 with `--synth` | GRLIB bottom-up Z3 sweep (`test_grlib_vhdl_z3.exe`); converts VHDL to Verilog so verilator/slang oracles can read it | required for the GRLIB sweep; otherwise optional |
| **Surelog** | UHDM frontend (parked at port-surface; slang strictly supersedes for SV elaboration) | optional |
| **Yosys** | RTLIL frontend, parallel-correctness sweep (`test_yosys_oracle_sweep.exe`), four-way miter (`test_yosys_vs_verible.exe`) | optional |
| **Vivado 2020.1** | CVA6 entity oracle, Vivado column on the sv-tests dashboard | optional |
| **nextpnr-xilinx** + **prjxray** | FPGA back end (`fpga_synth/`): chipdb build (`bbaexport`/`bbasm`), place & route, and FASM→bitstream (`fasm2frames` + `xc7frames2bit`); `uf2conv` for Sonata-board packaging | required for `fpga_synth/nextpnr_validate.sh`; otherwise optional |

Verible itself is **not** an external dependency — the SystemVerilog
parser used by the headline drivers is an OCaml port of Verible's
grammar (`Source_text_verible_lex` + `Source_text_verible`), built
from this repo via menhir.

### Building

```sh
eval $(opam env --switch=5.3.0 --set-switch)
dune build @all
```

The full build produces ~70 executables under `_build/default/`. Most
are diagnostic tools; the headline drivers are the ones in the table
above.

## Memory-of-the-flow

```
SV/RTLIL/UHDM    +   Vivado-emitted structural VHDL
     │
     ▼  per-frontend converter
   bprogram { modules: bmodule list }
     │
     ▼  optional pipeline
   unroll → inline → iflift → blocking_subst → meminfer → flatten
     │
     ▼  paired with another bmodule (any frontend)
   ffrip + share
     │
     ▼  encode as Z3 bit-vectors
   check_miter_equivalence → bool
```

Every pass and frontend stays at the BIR layer; the only places where
parser-tree-shaped data appears are inside the verible/vhdl/json/rtlil
frontends and (for elaboration) inside `verible_elaborate.ml`.

## Where work continues

Pending in the task tracker, in rough priority order:

1. **Auto-cascade `gen_mul`** for widths beyond Z3's monolithic ceiling
   (~10-bit) — emit hierarchical mul-leaves + adder-leaves automatically
   so any wide multiplier inherits the cascade verification path (#126)
2. **FPGA back end: memory / instance lowering** — `fpga_synth/bir_to_aig`
   raises on `Multiport_mem` / `Mem_read_port` / `Inst`; add block-RAM
   (RAMB), DSP48 and hierarchy inference so a full SoC (picosoc) can take
   the LUT/FF → nextpnr → prjxray path that counters already ride.
   Next milestone after that: VC707 / virtex7 (xc7vx485t) once the
   in-house prjxray DB finishes fuzzing
2. **Predictor calibration sweep against measured ORFS QoR** —
   close the loop between `predict_swap`'s analytical numbers and
   real post-CTS measurements (#109)
3. **Triage one `ct_vfdsu_*` failure** — the cva6 slang-primary sweep's
   11 NOTEQUIV cases all cluster in T-Head's VFDSU FP unit, almost
   certainly a single elaboration disagreement class shared across the
   block (#108 follow-up)
4. **Procedural-scope shadow renaming** — when two case arms declare
   the same name with different widths (cordic_sincos `x_shift` 31-bit
   vs 19-bit), qualify the bsignal name with the static-named (or
   anonymous `unnamedblkN`) hierarchy so the BIR carries both (#25
   follow-up)
5. **Full-synthesis equivalence path** — extend the cell-mapped miter
   to handle full `synth_design` output (Vivado's primitive-mapped
   form), not just `synth_design -rtl` (#26)
6. **Naja interchange format support** for incremental cert caching (#106)
7. **kepler-formal as a second oracle** for the parallel-correctness
   harness (#105)
8. **Surelog: extract widths, processes, instances** — parked, not
   blocked. Slang covers the same cases (and more) and is already a
   working Z3 oracle; revisit only if the UHDM ecosystem grows a
   feature slang doesn't have
9. **apb_uart 5-way convergence tail** — all five frontends now flatten
   into the same ~103-109 register band, but a few residuals remain
   before they miter to EQUIVALENT: the slang↔sv-parser pair reaches
   "interfaces match" then hits an *expression*-width mismatch (Z3
   BitVec 2 vs 3; per-module signal widths already agree); sv-parser
   needs `specialise_design`-style param specialisation (the WIDTH=11
   RX FIFO currently uses slib_fifo's default WIDTH=8); and the
   verible⊂slang 4-signal input-filter register residual (slang keeps
   a register per filter where verible aliases the output onto the
   parent net)
