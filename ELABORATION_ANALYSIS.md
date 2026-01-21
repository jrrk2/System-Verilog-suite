# SystemVerilog Elaboration Analysis

## The Problem

Comparing Yosys, Verilator, and Verible outputs is comparing **different levels of processing**:

### 1. **Verible** - Parse Tree (No Elaboration)
```verilog
input [WIDTH-1:0] a
```
Produces:
```
kBinaryExpression {
  kReference("WIDTH")
  '-'
  kNumber("1")
}
```
- Symbolic parameter references
- No constant evaluation
- No type resolution
- No width calculation

### 2. **Yosys RTLIL** - Elaborated + Synthesized
```verilog
parameter WIDTH = 8;
input [WIDTH-1:0] a;
```
Produces:
```
parameter \WIDTH 8
wire width 8 input 1 \a
```
- Parameters kept but **widths resolved to concrete values**
- Procedural blocks converted to gates (`$add`, `$mux`, etc.)
- Constants evaluated
- Types resolved

### 3. **Verilator JSON** - Elaborated + Optimized
```verilog
parameter WIDTH = 8;
input [WIDTH-1:0] a;
```
Produces:
```json
{
  "type": "VAR",
  "name": "WIDTH",
  "varType": "GPARAM",
  "valuep": [{"type": "CONST", "name": "32'sh8"}]
}
{
  "type": "VAR",
  "name": "a",
  "direction": "INPUT",
  "dtypep": "(L)" // Points to 8-bit type
}
```
- Parameters evaluated to constants
- **Widths resolved in type table**
- Hierarchies can be flattened
- Optimizations applied

## What Elaboration Does

Both Yosys and Verilator perform these steps that Verible does NOT:

### 1. **Parameter Evaluation**
```verilog
parameter WIDTH = 8;
wire [WIDTH-1:0] a;  // WIDTH-1 = 7
```
→ Resolves to `wire [7:0] a`

### 2. **Constant Propagation**
```verilog
localparam SIZE = 4 * 2;
wire [SIZE-1:0] data;
```
→ Resolves to `wire [7:0] data`

### 3. **Type Resolution**
```verilog
logic [7:0] a;
```
→ Resolves width, signedness, packed/unpacked dimensions

### 4. **Width Calculation**
```verilog
wire [3:0] a, b;
assign sum = a + b;  // What width is sum?
```
→ Yosys: Creates `$add` with explicit Y_WIDTH parameter
→ Verilator: Creates ADD node with result width computed

### 5. **Generate Block Expansion**
```verilog
generate
  for (i=0; i<4; i++) begin
    assign out[i] = in[i] & mask;
  end
endgenerate
```
→ Expands to 4 separate assignments with concrete indices

### 6. **Hierarchy Resolution**
```verilog
submodule #(.WIDTH(16)) inst (.a(sig));
```
→ Parameter passed to submodule, hierarchy resolved

## Current Verification Status

### ✅ Works: Same-width operations
```verilog
wire [7:0] a, b, sum;
assign sum = a + b;
```
Both Yosys and Verilator produce equivalent IR:
- Inputs: 2 × 8-bit
- Operations: 1 × Add(8-bit)
- Outputs: 1 × 8-bit

### ❌ Fails: Width-changing operations
```verilog
wire [3:0] a, b;
wire [7:0] product;
assign product = a * b;
```
Different IR structures:
- **Yosys**: Direct `Mul(width=8)` node
- **Verilator**: `ZeroExtend(4→8)` + `ZeroExtend(4→8)` + `Mul(width=8)`

Both are functionally correct but structurally different.

### ⚠️ Cannot compare with Verible yet
- Verible only gives parse tree
- No parameter evaluation → Can't resolve `[WIDTH-1:0]`
- No width calculation → Can't determine operation widths
- No type resolution → Can't convert to IR operations

## Solutions

### Option 1: Implement Elaboration in OCaml
Create `sv_verible_elaborate.ml` to:
- Evaluate parameters and constants
- Calculate widths and resolve types
- Expand generate blocks
- Flatten hierarchy (optional)

**Pros:** Full control, pure OCaml
**Cons:** Large implementation effort (1000+ lines)

### Option 2: Use Verible's Export + Yosys/Verilator for Elaboration
- Parse with Verible (validation)
- Elaborate with Yosys or Verilator
- Convert elaborated output to IR

**Pros:** Leverage existing elaboration
**Cons:** Not a truly independent path

### Option 3: IR Normalization
Instead of making Verible elaborate, normalize the IRs from Yosys/Verilator:
- Detect explicit width extension patterns in Verilator IR
- Make Yosys IR explicit about width extensions
- Compare normalized IRs

**Pros:** Solves the immediate verification issue
**Cons:** Doesn't help with Verible path

### Option 4: Use Verible for Syntax Validation Only
Keep the current 3-way approach:
- Verible: Syntax validation (already working)
- Yosys: Synthesis path
- Verilator: Behavioral path
- Compare Yosys ↔ Verilator only

**Pros:** Pragmatic, leverages each tool's strengths
**Cons:** Not a full tie-breaker

## Recommendation

**Start with Option 4 (current approach) + Option 3 (IR normalization)**:

1. Keep Verible for syntax validation (done ✅)
2. Implement IR normalization to handle width extension differences
3. This makes Yosys ↔ Verilator verification more robust
4. Later, optionally implement elaboration if needed

The IR normalization would add a pre-processing step:
```ocaml
let normalize_ir ir =
  (* Detect and normalize width extension patterns *)
  (* Make implicit extensions explicit in both IRs *)
  (* Return normalized IR *)
```

Then verify:
```ocaml
let yosys_norm = normalize_ir yosys_ir in
let verilator_norm = normalize_ir verilator_ir in
verify_ir_equivalence yosys_norm verilator_norm
```
