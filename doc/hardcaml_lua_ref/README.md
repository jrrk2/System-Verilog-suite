# Hardcaml-lua reference files

Two source files lifted verbatim from `~/hardcaml-lua.0.0.1/` as design
references for the Tier-2.5 hardcaml-as-synth flow.  They are NOT built
or linked into our tree — they exist here so future work on #98 / #99 has
the prior art on hand.

## `Input_hardcaml.ml`  (770 lines)

BIR-shape → hardcaml signal lowering.  Reference for **#98 (BIR →
hardcaml lowering for control logic)**.  Worth lifting:
- `tranitm` (line 419) — body translator covering always-blocks,
  registered assigns, mux, case
- `combiner` (553) — signal-merge logic for procedural assignments
- `spec width signedness architecture` (155) — arch-driven primitive
  instantiation; this is where the per-operator arch knob lives in his
  flow.  Maps cleanly to our cert system (#80, #95)
- `compare lft rght op` (396) — comparators
- `relational`, `arithnegate`, `signednegate`, `notequal/equal` —
  per-op signal builders

What we'll improve:
- Operate on our BIR directly (richer types than his `rw`)
- Lean on modern hardcaml's improved Always.ml / Reg_spec API
- Replace string-keyed arch knobs (`"fastest"`) with our cert-keyed
  arch enum (Sklansky_a / Kogge_stone_a / ...)

## `rtl_map.ml`  (454 lines)

Hardcaml-lua's tech mapper.  Reference for **#99 (Liberty-driven
structural tech-mapper)**.  The whole mapper is structural pattern-
match on the AST + filter Liberty cells by operator.  No AIG, no
ABC, no minimisation.  Worth lifting:
- `_map mapcnt itms` (312) — top-level dispatch on AST tuple shape
- `filtcells cells func` (125) — pick the Liberty cells whose function
  matches a given operator (AND/OR/XOR/NOT/MUX/+/POSEDGE)
- `filt'`, `filtmap`, `filtxor`, `filtbuf`, `filtmux`, `filtedge` —
  the per-operator predicates that recognise cell function shapes
- `_chk_arith` (401) — arithmetic-as-submodule pattern (synthesise an
  adder module, inline its body, recurse).  This is structurally what
  our cert-driven arch swap already does (#80, #95)

What we'll improve:
- Match against PARSED bexpr (alpha-equivalence + commutativity), not
  raw expression strings
- Coverage cost: when multiple cells match, pick by area (then leakage)
- Drive-strength sizing using per-cell-arc tables we already extract
  in #88
- Tail of dispatch falls back to yosys for unmapped subtree, with
  logging — not `failwith` like his version
- FF detection from Liberty `clocked_on` / `next_state` already lives
  in our Cell_delay; reuse rather than re-implement

## Why these are here, not in the tree proper

- They're MIT-licensed (Jonathan Kimmitt, 2024) so vendoring is fine
- But they're written against hardcaml-lua's RTL-parser AST, not BIR
- And their pattern-match style is fragile (`failwith` everywhere)
- Building on top of them directly would import the fragility; using
  them as design references lets us keep the good ideas while writing
  our own version against BIR + the parsed-bexpr Liberty representation

When #98 / #99 land, this directory can be deleted (or left as a
historical reference — they're tiny).
