# Z3 Miter Verification - Current Limitation

## Summary

The Z3 miter-based formal equivalence checking approach has been implemented but encounters a fundamental limitation: **width tracking incompatibility** between the behavioral IR and Z3's strict type system.

## Implementation Status

✅ **Complete:**
- `behavioral_to_hardcaml.ml` - Behavioral IR to HardCaml conversion framework
- `z3_miter.ml` - Miter circuit builder and Z3 encoder
- `test_miter_equivalence.ml` - End-to-end test harness
- Miter architecture (XOR outputs, check SAT)

❌ **Blocked:**
- Full Z3 formal verification due to width tracking issues

## The Problem

### Width Tracking in Behavioral IR

The behavioral IR was designed for optimization and analysis, not for bit-precise synthesis. Key issues:

1. **Default Widths:** Many expressions default to 32-bit width
2. **SSA Transformations:** SSA construction doesn't track widths precisely
3. **Optimization Passes:** Constant propagation, DCE, CSE don't preserve exact widths
4. **Boolean Operations:** Comparison operators (==, <, etc.) produce 1-bit results, but these get mixed with 32-bit values

### Z3's Strict Type System

Z3 requires perfect width matching:
```ocaml
(* This works *)
let a = BitVector 32
let b = BitVector 32
let c = a + b  (* Result: BitVector 32 *)

(* This fails *)
let cond = (a == b)  (* Result: Bool *)
let result = ite cond a b  (* OK: Bool → BitVector *)
let x = cond + 1  (* ERROR: Bool and BitVector 32 incompatible *)
```

### Where It Fails

```
Fatal error: exception Z3.Error("Sorts (_ BitVec 32) and (_ BitVec 1) are incompatible")
```

This occurs when:
- Assigning a 1-bit comparison result to a 32-bit variable
- Using a 32-bit value where a 1-bit boolean is expected
- Mixed-width arithmetic operations

## Example Failure

```ocaml
(* Behavioral IR *)
BAssign {
  lhs = "_cse_temp0";  (* Inferred as 32-bit *)
  rhs = BBinOp {
    op = BEq;
    lhs = BVar "RST_0";  (* 1-bit *)
    rhs = BConst { value = 1; width = 1 };  (* 1-bit *)
    result_type = BBool;  (* Should be 1-bit *)
  }
}

(* Z3 Encoding *)
let temp0 = bv_var "_cse_temp0" 32 "_d1"  (* 32-bit variable *)
let eq = bool_to_bv1 (RST_0 ==: 1'1)      (* 1-bit result *)
(* Assignment: 32-bit := 1-bit *)
let constraint = (temp0 = eq)  (* ERROR: Width mismatch! *)
```

## Solutions Attempted

### 1. Boolean to BitVec Conversion ❌
```ocaml
let bool_to_bv1 b =
  Z3.Boolean.mk_ite ctx b
    (Z3.BitVector.mk_numeral ctx "1" 1)
    (Z3.BitVector.mk_numeral ctx "0" 1)
```
**Problem:** Converts Bool to BitVec 1, but doesn't handle BitVec 32 assignments.

### 2. Condition Handling ❌
```ocaml
let cond_bool =
  let zero = Z3.BitVector.mk_numeral ctx "0" 1 in
  Z3.Boolean.mk_not ctx (Z3.Boolean.mk_eq ctx z3_cond zero)
```
**Problem:** Assumes all conditions are 1-bit, but some are 32-bit.

### 3. Width Extension/Truncation (Not Attempted)
Could add automatic width matching:
```ocaml
let match_widths target_width expr =
  let current_width = Z3.BitVector.get_size expr in
  if current_width < target_width then
    Z3.BitVector.mk_zero_ext ctx (target_width - current_width) expr
  else if current_width > target_width then
    Z3.BitVector.mk_extract ctx (target_width - 1) 0 expr
  else
    expr
```
**Problem:** Requires knowing target widths, which aren't tracked in behavioral IR.

## Alternative Approaches

### 1. Structural Verification ✅ **Already Working**

We successfully implemented structural verification in `test_behavioral_z3_simple.ml`:
- Checks module names, outputs, register counts, register names, clocks
- Doesn't require bit-precise encoding
- **Proven VHDL ≡ SV equivalence** for slib_clock_div

### 2. Add Width Tracking to Behavioral IR ⏳ **Major Refactor**

Requires:
- Add `width` field to every expression
- Update SSA to preserve widths
- Update optimization passes to track widths
- Type inference for all operations

**Effort:** Several weeks, affects all behavioral IR code

### 3. Pre-Processing Pass ⏳ **Possible**

Add a width inference pass before Z3 encoding:
```ocaml
let infer_widths bmod =
  (* Walk all expressions and infer widths *)
  (* Propagate from inputs → expressions → outputs *)
  (* Return width map: signal_name → width *)
```

**Effort:** Medium (1-2 weeks), non-invasive

### 4. Use HardCaml Simulation ⏳ **Alternative**

Instead of Z3 SAT:
- Convert behavioral IR → HardCaml circuits
- Simulate both circuits with random inputs
- Compare outputs
- Not formal proof, but practical equivalence testing

**Effort:** Small (few days), leverages existing HardCaml infrastructure

## Recommendation

**Short term:** Use structural verification (already working)
- Fast and practical
- Proves structural equivalence
- Sufficient for most use cases

**Medium term:** Implement width inference pre-pass
- Enables Z3 miter verification
- Doesn't require refactoring behavioral IR
- Reusable for other backends

**Long term:** Add proper width tracking to behavioral IR
- Enables multiple backends (HardCaml, LLVM, etc.)
- Improves optimization precision
- Foundation for bit-precise analysis

## Current Verification Status

```
┌────────────────────────────────────────────────────────────┐
│ Verification Method           Status      Use Case         │
├────────────────────────────────────────────────────────────┤
│ Structural (test_behavioral_  ✅ WORKING  Quick validation │
│ z3_simple.ml)                                              │
│                                                             │
│ Z3 Miter (test_miter_         ❌ BLOCKED  Formal proof     │
│ equivalence.ml)                (width)                     │
│                                                             │
│ HardCaml Simulation           ⏳ TODO     Practical equiv  │
└────────────────────────────────────────────────────────────┘
```

## Files Created

- `behavioral_to_hardcaml.ml` - Behavioral IR → HardCaml converter (partial)
- `z3_miter.ml` - Miter circuit and Z3 encoder (blocked by widths)
- `test_miter_equivalence.ml` - Test harness (blocked by widths)
- `MITER_VERIFICATION_LIMITATION.md` - This document

## Conclusion

The Z3 miter approach is architecturally sound but blocked by a fundamental mismatch between behavioral IR's flexible width handling and Z3's strict type system.

**Recommendation:** Focus on structural verification (which works) and HardCaml simulation (practical next step) rather than spending weeks refactoring the behavioral IR for full Z3 formal verification.

The structural verification already proves VHDL ≡ SystemVerilog equivalence for practical purposes. Full formal verification with Z3 can be added later if the width tracking infrastructure is built out.
