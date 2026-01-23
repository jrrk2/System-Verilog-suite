# Simple VHDL to IR Converter - Recommended Approach

## Current Situation

You have:
1. **rewrite.ml** - Battle-tested VHDL→SystemVerilog converter using `vhdintf` tree patterns
2. **Existing buggy converter** - Complex vhdl_to_ir.ml with many issues
3. **Parse tree example** - vhdl_uart.ml showing the `vhdintf` structure

## Recommended Simple Approach

Instead of spending 30 days debugging the existing converter, use **rewrite.ml's proven patterns** but generate **IR nodes** instead of **SystemVerilog strings**.

### Three Options

#### Option 1: Minimal - VHDL→SV→IR (Quickest)
```
VHDL → rewrite.ml → SystemVerilog → Existing SV parser → IR
```
**Pros**: Reuses 100% of existing working code
**Cons**: Extra parsing step
**Timeline**: < 1 week

####Option 2: Direct Pattern Reuse (Recommended)
```
VHDL → vhdl_rewrite_to_ir.ml (based on rewrite.ml patterns) → IR
```
**Approach**:
1. Copy proven pattern matches from rewrite.ml (lines 553-676 for processes, 175-268 for expressions)
2. Replace `Buffer.add_string` with IR node creation
3. Reuse match2_args context for tracking signals/widths

**Pros**: Clean, maintainable, proven patterns
**Cons**: Requires understanding vhdintf tree structure
**Timeline**: 1-2 weeks

#### Option 3: New Simple Typed Converter
```
VHDL → VhdlParser → VhdlTypes (typed AST) → new converter → IR
```
**Approach**: Write from scratch using VhdlTypes (not vhdintf)
**Pros**: Type-safe, potentially cleaner
**Cons**: Reinventing pattern matching that rewrite.ml already has
**Timeline**: 2-3 weeks

## My Recommendation

**Use Option 1 for immediate results**:
```ocaml
(* vhdl_via_sv_to_ir.ml *)
let convert_vhdl_file filename =
  (* Use rewrite.ml to generate SystemVerilog *)
  let sv_code = Vhd_front.Rewrite.convert_to_sv filename in
  (* Parse SystemVerilog with existing parser *)
  let sv_ast = parse_systemverilog sv_code in
  (* Convert SV AST to IR (already working) *)
  sv_to_ir sv_ast
```

Then **migrate to Option 2 incrementally** by replacing parts with direct IR generation.

## Key Insight from rewrite.ml

The patterns you need are already proven in rewrite.ml:

**Process with clock/reset** (line 553):
```ocaml
| Sextuple (Vhdprocess_statement, Str process, _,
            Double (VhdSensitivityExpressionList, List dep_lst),
            decls,
            Double (VhdSequentialIf, ...)) when has_clk_and_reset ->
    (* rewrite.ml generates: "always @(posedge clk or posedge rst)" *)
    (* You generate:  Register IR nodes with clock/reset inputs *)
```

**Expressions** (lines 175-196):
```ocaml
| Triple (VhdEqualRelation, lft, rght) ->
    (* rewrite.ml: Buffer.add_string " == " *)
    (* You: add_node ctx (Compare { cmp_op = `Eq; ... }) [l_id; r_id] *)
```

## Next Steps

1. Test Option 1 first - should work immediately
2. Profile to see if the extra SV parsing step is a bottleneck
3. If performance is fine, ship it
4. If not, migrate incrementally to Option 2

## Why This is Better Than 30-Day Debug

- rewrite.ml has handled edge cases for years
- You get working code in days, not months
- Can incrementally optimize later
- No risk of missing corner cases the old converter handled
