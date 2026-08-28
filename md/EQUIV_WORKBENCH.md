# Equivalence workbench (Z3 GUI)

`sv_gui.exe` → **Verify → Equivalence workbench (Z3)…**, or headless with
`sv_suite equiv`. Same engine either way (`equiv_core.ml`); the GUI
(`gui_equiv.ml`) is widgets over it, so nothing the window claims is
unreproducible from a shell.

    make equiv-gui         # open the workbench
    make equiv-selftest    # 12-case self-test (engine, window, tool scan)
    make equiv-tools       # what each frontend needs, and what was found

## External tools are scanned, not assumed

Nothing is offered before it is found. `tool_scan.ml` resolves each frontend's
binary in the frontend's **own** search order — user selection → env override →
the checkout locations the frontend itself tries → `PATH` — and only frontends
that resolve appear in the page-1 combos and in the Verify menu's miter
pairings. Each entry is labelled with the binary that will run, because "yosys"
and "the yosys in a checkout you forgot about" are different tools and the
difference shows up later as a mysterious verdict.

    sv_suite equiv --tools                     # what each frontend needs, found, and used
    sv_suite equiv --tool slang=/opt/bin/slang # select one; remembered

The GUI's **Tools…** button is the same thing with a file picker. Selections
persist in `~/.config/sv_suite/tools.json` and are exported into the frontend's
env override (`YOSYS_BIN`, `SLANG_BIN`, `SV2V_BIN`, `SYNLIG_BIN`,
`SVS_VERILATOR_BIN`, `SV_PARSER_BIN`), so the tool the scan reported is exactly
the one that execs. Three of those overrides did not exist before this —
`yosys` and `slang` had no override at all, and slang never looked at `PATH`,
so a slang installed anywhere unusual simply "was not found" with nothing the
user could set to fix it.

> **`SVS_VERILATOR_BIN`, never `VERILATOR_BIN`.** `VERILATOR_BIN` is
> verilator's *own* variable — its perl driver reads it to find `verilator_bin`
> — so writing the wrapper's path into it makes the wrapper re-exec itself
> without end. SVS still *reads* `VERILATOR_BIN` if you set it by hand, but
> writes only `SVS_VERILATOR_BIN`. Self-test case 12 runs a discovered
> frontend under a wall-clock bound precisely so a recurrence shows up as a
> failure rather than a hang.

The scan also reports what it can only learn by asking:

* `yosys` — whether the **yosys-slang plugin** is present. Without it the
  default read path fails on the first SV file even though the binary is
  right there (`YOSYS_READ_VERILOG=1` falls back to the weaker built-in reader).
* `sv2v` — BLOCKED unless yosys is available too, since it converts and then
  hands off.
* `surelog` — marked dump-only: the frontend consumes a pre-captured
  `uhdm-dump`, it never runs surelog itself.

A frontend whose tool is missing fails **before** the parse, with a message
that names the fix and exit code 2 — not a stack trace from three layers down.

## Why the register step exists

The miter is combinational-after-FF-ripping: every flop's Q becomes a free
primary input and its next-state expression becomes an output `<q>__D`. The two
sides' state is then tied **by name**. That is fine for RTL-vs-RTL and useless
against a synthesised netlist — the moment a flow renames `acc_q` to `n42_reg`,
splits a bus, or appends `_reg`, the state is untied and Z3 reports a
counterexample about nothing.

So step 2 is not a convenience. Without it (self-test case 3) the renamed
design reports **DIFFER**; with it, **EQUIVALENT** — same two files.

Matching runs in stages, cheapest and most confident first:

| stage | method | shown as |
|---|---|---|
| 1 | exact name | `name` |
| 2 | canonical name (`canon_sep_name`, minus `_reg`/`_ff`/`_q`/`__Q`), only when unambiguous on **both** sides | `canonical` |
| 3 | simulation signature — partition refinement over `__D` values under shared random stimulus (`Sv_lua.reg_correspond`) | `simulation` |
| 4 | your decision | `manual` |

Manual overrides live in the project file, keyed by the A-side register, so a
matching is done once rather than once per run. An override naming a register
that no longer exists is reported, never silently dropped.

**The names an override must use are the ripped names**, not the RTL's:
`prep_for_z3` rewrites `_reg` suffixes and bit-splits scalars, so `n43_reg` in
the source is `n43__b0` by the time the miter sees it. `--list-regs` prints the
table the GUI shows on page 2.

## Assumptions

1:1 register correspondence. No retiming, no k-induction — for capacity, the
route through a big design is **bottom-up hierarchical** (`--mode hier`): each
module is proved with its children abstracted to uninterpreted functions, so a
parent's pass is an assume-guarantee proof over separately-proven children, and
the first divergent module in leaves-first order is the tightest localisation.

## The failure mode this is built against

Not "says DIFFER when it should say EQUIVALENT" — the **vacuous pass**. A miter
that matched no registers, compared no outputs, or constrained no inputs will
cheerfully report equivalence. So every verdict carries a census of what it is
a statement about:

    Proved over
      inputs constrained : 6 common  (A 6, B 6)
      outputs compared   : 5 common  (A 5, B 5)  = 2 next-state cones + 3 primary
      registers matched  : 2  (A 2, B 2)

and thin coverage downgrades the verdict to `EQUIVALENT (WEAK …)` with the
reasons listed. Exit codes: **0** proved, **1** a real difference, **2**
anything else (weak, inconclusive, uncomparable, error) — an INCONCLUSIVE must
never reach a Makefile looking like a pass.

## Counterexample debug

Page 4 (`--explain <cone>`) solves one cone, then explains it on the *partial*
netlist — only the cone behind that output:

1. Z3 model → stimulus (primary inputs + lifted state).
2. Both sides simulated on that stimulus (`Behavioral_initeval`). If the
   simulation does **not** reproduce the difference, the counterexample is an
   encoding artefact and says so.
3. Signals present on both sides whose values disagree.
4. Of those, the ones whose entire common support agrees — the **first
   divergence**: everything feeding it matches, its own value does not, so the
   fault is the logic in between. Selecting one shows both defining
   expressions:

       ovf_q__D   A=0x0  B=0x1   [5 common inputs, all equal]
         A: (rst ? 1'0 : (en ? wide[8:8] : ovf_q))
         B: (rst ? 1'0 : (en ? n17[7:7]  : ovf_q))

State inputs are free in a combinational miter, so a shown state may be
unreachable in operation; the report says that too.

## CLI

    sv_suite equiv <project.json> [options]
    sv_suite equiv --a <fe>,<top>,<file>[,<file>…] --b <fe>,<top>,<file>[,…] [options]

      --mode flat|per-cone|hier   per-cone localises which outputs differ
      --timeout <ms>              Z3 timeout per check (default 30000)
      --no-sim                    skip simulation-signature matching
      --scan                      list every differing cone
      --explain <cone>            counterexample + first-divergence report
      --list-regs                 print the register-matching table
      --save <file>               write the report
      --save-project <file>       write a project file the GUI can open
      --tools                     scan for external tools and print the table
      --tool <fe>=<path>          select a binary for one frontend, and remember it

`<fe>` is one of the frontends the scan found usable (`--tools` lists them):
`verible`, `verible-ext`, `vhdl` are built in; `slang`, `yosys`, `synlig`,
`sv2v`, `verilator`, `sv-parser` need their binary. Choosing it per side is how
the synthesis tool is chosen: A through `verible` and B through `yosys` is
RTL-vs-post-synthesis.
