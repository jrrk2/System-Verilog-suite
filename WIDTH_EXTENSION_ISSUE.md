# Width Extension Representation Difference

## The Core Problem

Given this Verilog:
```verilog
wire [3:0] a, b;        // 4-bit inputs
wire [7:0] product;     // 8-bit output
assign product = a * b; // 4×4=8 bit multiplication
```

### Yosys RTLIL Representation:
```
cell $mul:
  A_WIDTH: 4
  B_WIDTH: 4
  Y_WIDTH: 8
  A ← wire a (4-bit)
  B ← wire b (4-bit)
  Y → wire product (8-bit)
```

**Implicit width extension**: The $mul cell handles 4-bit → 8-bit internally.

### Verilator JSON Representation:
```
ASSIGNW:
  product ← MUL(
              EXTEND(VARREF(a): 4-bit → 8-bit),
              EXTEND(VARREF(b): 4-bit → 8-bit)
            ): 8-bit
```

**Explicit width extension**: EXTEND nodes inserted before multiplication.

### Our IR Conversion:

**From Yosys RTLIL:**
```
Node 1: Mul { width=8, signed=false }
  inputs: [a_id, b_id]  // 4-bit inputs directly
```

**From Verilator JSON:**
```
Node 1: ZeroExtend { from_width=4, to_width=8 }
  inputs: [a_id]
Node 2: ZeroExtend { from_width=4, to_width=8 }
  inputs: [b_id]
Node 3: Mul { width=8, signed=false }
  inputs: [node1_id, node2_id]  // 8-bit extended inputs
```

## Z3 Verification Failure

When we compare these IRs with Z3:

**Yosys IR generates:**
```smt2
(define-fun product () (_ BitVec 8)
  (bvmul
    ((_ zero_extend 4) a)  ; Extension happens in adjust_to_width
    ((_ zero_extend 4) b)))
```

**Verilator IR generates:**
```smt2
(define-fun product () (_ BitVec 8)
  (bvmul
    ((_ zero_extend 4) a)
    ((_ zero_extend 4) b)))
```

Wait... they SHOULD be the same! Let me check what's actually happening in sv_ir_verify.ml...

## Actual Issue

The problem is that `adjust_to_width` in sv_ir_verify.ml is called AFTER the operation,
but Verilator's EXTEND nodes create separate IR nodes that get converted to Z3 independently.

The Z3 expressions end up being:
```smt2
; Yosys path:
(define-fun mul_result () (_ BitVec 8)
  (bvmul a b))  ; a and b are still 4-bit here!

; Verilator path:
(define-fun extend_a () (_ BitVec 8) ((_ zero_extend 4) a))
(define-fun extend_b () (_ BitVec 8) ((_ zero_extend 4) b))
(define-fun mul_result () (_ BitVec 8)
  (bvmul extend_a extend_b))
```

## Solution Approaches

### 1. Fix Yosys → IR Converter
Make sv_rtlil_to_ir.ml insert explicit ZeroExtend nodes when input widths don't match output width:

```ocaml
let cell_to_ir_operation cell =
  match cell.cell_type with
  | "$mul" ->
      let a_width = get_param cell "A_WIDTH" in
      let b_width = get_param cell "B_WIDTH" in
      let y_width = get_param cell "Y_WIDTH" in

      (* If inputs narrower than output, insert extends *)
      if a_width < y_width || b_width < y_width then
        (* Return special marker to insert extend nodes *)
        Some (NeedsExtension { ... })
      else
        Some (Mul { width=y_width; signed=... })
```

### 2. Fix IR → Z3 Converter
Make sv_ir_verify.ml handle mixed-width operations correctly:

```ocaml
let rec ir_op_to_z3 ir node =
  match node.node_op with
  | Mul { width; signed } ->
      if List.length inputs_z3 >= 2 then
        let a = List.nth inputs_z3 0 in
        let b = List.nth inputs_z3 1 in

        (* Get actual widths of inputs *)
        let a_width = Z3.BitVector.get_size (Z3.Expr.get_sort a) in
        let b_width = Z3.BitVector.get_size (Z3.Expr.get_sort b) in

        (* Extend to output width if needed *)
        let a_ext = if a_width < width then
          Z3.BitVector.mk_zero_ext ctx (width - a_width) a
        else a in
        let b_ext = if b_width < width then
          Z3.BitVector.mk_zero_ext ctx (width - b_width) b
        else b in

        Z3.BitVector.mk_mul ctx a_ext b_ext
```

THIS IS WHAT WE ALREADY HAVE with `extend_to_match_width`!

### 3. The Real Bug

Looking at sv_ir_verify.ml:150-160, we DO call `extend_to_match_width` and `adjust_to_width`.

The bug must be that we're comparing node structures, not final Z3 values!

Let me check the actual counterexample...

```
Counterexample:
(define-fun n1_simple_mult () (_ BitVec 4) #xf)
(define-fun n2_simple_mult () (_ BitVec 4) #xf)
```

This shows n1 and n2 are both 4-bit with value 0xF (15).
For 15 × 15:
- Expected: 225 (0xE1 in 8 bits)
- If truncated to 8 bits: 0xE1 = 225 ✓

So why does Z3 think they're different? Let me trace through the actual Z3 generation...

## Investigation Needed

Run verification with debug output to see exact Z3 formulas generated for each path.
