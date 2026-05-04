# System-Verilog-decompiler

A multi-frontend SystemVerilog elaborator and equivalence-checking pipeline,
written in OCaml. Pulls SV/Verilog/RTLIL/UHDM and Vivado's
post-elaboration structural VHDL through five distinct parsers into a
common Behavioral IR (BIR), runs SV-elaboration and post-elaboration
optimisation passes on it, and proves equivalence between any two BIR
modules via Z3 SMT. Originally built to recover behavioural Verilog from
synthesised netlists; grown into a side-by-side correctness checker for
SV elaboration, with Vivado's elaborated form as the oracle.

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
| Verible | `verible_to_behavioral.ml` + `verible_elaborate.ml` | SV source via OCaml port of Verible's grammar | Full elaboration: parameter overrides, generate unrolling, const-fn evaluation, struct-typed parameters |
| Verilator JSON | `verilator_to_behavioral.ml` | `verilator --json-only` AST dump | Already-elaborated, monomorphic — read after Verilator does the heavy lifting |
| Vivado RTL VHDL | `vhdl_to_ver_front.ml` → `ver_front_to_behavioral.ml` | Structural VHDL emitted by Vivado's `synth_design -rtl` (after Vivado has parsed and elaborated the original SV source) | Treats RTL_REG_SYNC/ASYNC cells as `BSequential` processes; the structural oracle |
| Yosys RTLIL | `rtlil_to_behavioral.ml` | `read_verilog`-then-`write_rtlil` | Used for the four-way miter |
| Surelog UHDM | `surelog_to_behavioral.ml` | UHDM dump from Surelog | Leaf-cell pipecleaner; not used for CVA6-scale designs (type-parameter limitation) |

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

## Z3 miter (`z3_miter.ml`)

`check_miter_equivalence : bmodule -> bmodule -> bool`

Encodes both modules as bit-vector functions of their primary inputs
(after `ffrip`+`share`), checks that for every input both produce
identical outputs (and identical D-pin next states for every shared FF
name). Fast path for combinational designs; for sequential designs the
state correspondence is via Q-name matching — a precondition the FF-set
comparator (#63) is responsible for verifying first.

## Tools and entry points

Built via `dune build`. The most-used executables:

| Executable | Source | Use |
|---|---|---|
| `test_cva6_ff_diff.exe` | `test_cva6_ff_diff.ml` | Per-entity FF-set diff between Vivado's elaborated form (its `synth_design -rtl` VHDL output) and Verible-elaborated SV. Optional `MITER_Z3=1` runs Z3 on FF-matching pairs |
| `test_cva6_bottom_up.exe` | `test_cva6_bottom_up.ml` | Pairwise Z3 miter for every Verilator-JSON module against the matching entity in Vivado's elaborated VHDL output (`cva6_elab.vhd`) |
| `test_verilator_vs_verible.exe` | `test_verilator_vs_verible.ml` | Verilator-JSON ↔ Verible-OCaml miter on a single SV file |
| `test_verible_to_bir.exe` | `test_verible_to_bir.ml` | Verible-side BIR dump for a single SV file. Useful for debugging elaboration. `--miter <vhd>` adds Z3 against a Vivado entity |
| `ff_stats.exe` | `ff_stats.ml` | Run all four frontends on a single testcase and report each one's Q__Q / Q__D set, with pairwise overlap numbers |
| `random_sv_gen.exe` | `random_sv_gen.ml` | Constrained-random SV generator. `--features mixed` rotates through nine modes |

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

Requires:

- OCaml 5.3.0 (dune, str, yojson, unix, hardcaml, z3, ppx_deriving_yojson, linenoise)
- Verilator (any recent — the project targets 5.024+)
- Vivado 2020.1 (only needed for the CVA6 oracle and sv-tests Vivado column)
- Yosys (optional, for the four-way miter)

```sh
opam switch 5.3.0
eval $(opam env)
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

1. CVA6 popcount Z3 — the converter now flattens the recursive tower
   correctly, but the body-level logic still diverges from Vivado on
   specific bit-positions. Likely a port-connection-expression issue
   the random fuzzer doesn't currently catch
2. ct_vfdsu_* width mismatches — 4 entities Z3-error on bitvec sort
   conflicts; the strict-pairing change shifted them from silent
   passes to explicit errors, next is to find the one signal whose
   width derivation differs
3. RTL_REG with CE pin support
4. Full-synthesis (post-synth_design) equivalence path
5. Surelog: extract widths, processes, instances (currently leaf-cell
   only; widening to support type parameters would unblock CVA6)
