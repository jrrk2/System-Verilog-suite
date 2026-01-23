# Z3 API Fix - get_size Usage

## Issue

```ocaml
Error: The value c has type Expr.expr but an expression was expected of type
       Sort.sort
```

## Root Cause

The Z3 OCaml API changed. `Z3.BitVector.get_size` expects a `Sort.sort`, not an `Expr.expr`.

## Wrong Usage ❌

```ocaml
let c = expr_to_z3 suffix condition in
let size = Z3.BitVector.get_size c  (* ERROR: c is Expr.expr *)
```

## Correct Usage ✅

```ocaml
let c = expr_to_z3 suffix condition in
let c_sort = Z3.Expr.get_sort c in      (* Get sort from expression *)
let size = Z3.BitVector.get_size c_sort  (* Now pass Sort.sort *)
```

## Places Fixed

### 1. Conditional Expression (Cond)

**Before**:
```ocaml
let c_bool = Z3.Boolean.mk_not ctx 
  (Z3.Boolean.mk_eq ctx c (Z3.BitVector.mk_numeral ctx "0" (Z3.BitVector.get_size c)))
```

**After**:
```ocaml
let c_sort = Z3.Expr.get_sort c in
let c_size = Z3.BitVector.get_size c_sort in
let zero_val = Z3.BitVector.mk_numeral ctx "0" c_size in
let c_bool = Z3.Boolean.mk_not ctx (Z3.Boolean.mk_eq ctx c zero_val)
```

### 2. Assignment Constraints (Assign)

**Before**:
```ocaml
let l = expr_to_z3 suffix lhs in
let r = expr_to_z3 suffix rhs in
let wl = Z3.BitVector.get_size l in  (* ERROR *)
let wr = Z3.BitVector.get_size r in  (* ERROR *)
```

**After**:
```ocaml
let l = expr_to_z3 suffix lhs in
let r = expr_to_z3 suffix rhs in
let wl = Z3.BitVector.get_size (Z3.Expr.get_sort l) in  (* CORRECT *)
let wr = Z3.BitVector.get_size (Z3.Expr.get_sort r) in  (* CORRECT *)
```

### 3. Continuous Assignment (AssignW)

Same fix as Assign.

## Pattern to Remember

```ocaml
(* Step 1: Get the expression *)
let expr = some_z3_operation in

(* Step 2: Get the sort from the expression *)
let sort = Z3.Expr.get_sort expr in

(* Step 3: Get the size from the sort *)
let size = Z3.BitVector.get_size sort in
```

## Why This Happened

Z3's OCaml bindings separate:
- **Expr.expr**: The actual expression/value
- **Sort.sort**: The type/sort of the expression

Operations like `get_size` operate on **sorts** (types), not **expressions** (values).

## Testing

Build to verify:
```bash
dune build
```

Should compile without errors now!

## Related Functions

Other functions that expect `Sort.sort`:
- `Z3.BitVector.get_size`
- `Z3.Sort.get_sort_kind`
- Various sort inspection functions

## Quick Reference

| Function | Input Type | Returns |
|----------|-----------|---------|
| `Z3.Expr.get_sort` | `Expr.expr` | `Sort.sort` |
| `Z3.BitVector.get_size` | `Sort.sort` | `int` |
| `Z3.BitVector.mk_numeral` | `context -> string -> int` | `Expr.expr` |
