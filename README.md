# System-Verilog-decompiler

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
| Verilator JSON | `verilator_to_behavioral.ml` | `verilator --json-only` AST dump | Already-elaborated, monomorphic. Walks procedural-scope `Var` declarations (named-begin / `unnamedblkN`) so locals carry source-stated widths into the BIR |
| Slang | `slang_to_behavioral.ml` | `slang --ast-json` | Independent SV elaborator, parses CVA6 (handles type parameters Surelog can't); first-class Z3 oracle peer |
| Vivado RTL VHDL | `vhdl_to_ver_front.ml` → `ver_front_to_behavioral.ml` | Structural VHDL emitted by Vivado's `synth_design -rtl` (after Vivado has parsed and elaborated the original SV source) | Treats RTL_REG_SYNC/ASYNC/_CE cells as `BSequential` processes; the structural oracle |
| GRLIB / VHDL native | `vhdl_to_behavioral.ml` | VHDL source via the `vhd_front` parser | Records, enums, conditional concurrent assigns. 1072/1105 GRLIB files (97%) emit clean BIR; first Z3 ✅ EQUIVALENT on `eth_rstgen` against ghdl-synth (verilator/slang oracles) |
| Yosys RTLIL | `rtlil_to_behavioral.ml` | `read_verilog`-then-`write_rtlil` | Used for the four-way miter |
| Surelog UHDM | `surelog_to_behavioral.ml` | UHDM dump from Surelog | **Parked at port-surface only** — slang covers a strict superset (CVA6's type parameters work) and is already a Z3 oracle. Use slang for new oracle work |

Each frontend produces a `bprogram = { modules: bmodule list; library_cells: ... }`.
A `bmodule` is `{ name; params; signals; processes; instances; funcs; mems }`
where `processes` is a list of `BCombinational` / `BSequential` blocks
holding `bstmt` trees over `bexpr` trees.

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

| Stage | Module | Purpose |
|---|---|---|
| `Behavioral_to_hardcaml.create_circuit` | `behavioral_to_hardcaml.ml` | Lower a `bmodule` to a hardcaml `Circuit.t` — operators map to `Signal` ops, `BSequential` blocks to `Reg_spec`-clocked registers |
| `Lib_map.map_circuit` | `lib_map.ml` | Tech-map hardcaml's `Comb_op` / `Mux` / `Eq` / `Add` / `Sub` / `Mul` / `Lt` nodes onto Liberty cells. Bit-blasts adders/subtractors as ripple-carry, multipliers as Array (b_w iterations of `gen_add`), gates as `AND2_X1`/`OR2_X1`/`XOR2_X1`/`INV_X1`/`MUX2_X1`. Set `LIB_MAP_DCE=1` for dead-code elimination |
| `Hier_synth.synth_program` | `hier_synth.ml` | Per-module gate-level synth in dependency order. Children get phantom IO at their parent so each module synthesises standalone, while the cell-Verilog emit step splices each instance line back in alongside the parent's cells. Preserves both hier (instance lines) and flat (gate cells) views |
| `Cell_verilog_emit` | `cell_verilog_emit.ml` | Write OpenROAD-readable structural Verilog: bus reconstruction emitted as `assign bus = {bit_w-1, …, 1, 0};` so OpenSTA can trace each gate's full bus driver |

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
| `test_vhdl_bir_dump.exe` | `test_vhdl_bir_dump.ml` | Single-VHDL-file BIR dump. `--multi <top> <files>` walks the instance graph for hierarchy dumps |
| `test_cva6_ff_diff.exe` | `test_cva6_ff_diff.ml` | Per-entity FF-set diff between Vivado's elaborated form and Verible-elaborated SV. `MITER_Z3=1` runs Z3 on FF-matching pairs |
| `test_cva6_bottom_up.exe` | `test_cva6_bottom_up.ml` | Pairwise Z3 miter for every Verilator-JSON module against the matching entity in Vivado's elaborated VHDL output |
| `test_verible_to_bir.exe` | `test_verible_to_bir.ml` | Verible-side BIR dump for a single SV file. Useful for debugging elaboration. `--miter <vhd>` adds Z3 against a Vivado entity |
| `ff_stats.exe` | `ff_stats.ml` | Run all four frontends on a single testcase and report each one's Q__Q / Q__D set, with pairwise overlap numbers |
| `random_sv_gen.exe` | `random_sv_gen.ml` | Constrained-random SV generator. `--features mixed` rotates through nine modes |
| `sv_decompiler.exe script <file.lua>` | `sv_decompiler.ml` + `sv_lua.ml` | Run a Lua script with the `svd.*` API exposed (parse / pick / miter / gate_miter / liberty / expand / bir / insts / timing / emit_verilog / emit_vhdl / write_verilog / write_vhdl / convert_hdl) |

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
