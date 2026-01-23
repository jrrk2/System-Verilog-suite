# Meaningful Naming Strategy for IR Nodes

## Date: 2026-01-22

## Overview

Implemented a unified naming strategy that generates meaningful signal names based on operations and input names. This makes IR comparison dramatically easier by replacing generic names like `n1`, `n2`, `n3` with descriptive names like `clk_and_rst`, `a_plus_b`, `mux_sel_a_b`.

## Motivation

### Before (Generic Names)
```verilog
logic n2;
logic n3;
logic n7;
logic n9;
logic n10;

always @(posedge CLK or posedge RST) begin
  if (RST) begin
    n10 <= 0;
  end else begin
    n10 <= n9;
  end
end

assign n2 = CLK == n1;
assign n3 = CLK & n2;
assign n7 = RST == n6;
assign n9 = n3 ? n8 : n4;
```

**Problems:**
- Can't tell what n2, n3, n7, n9, n10 represent
- Hard to spot equivalent logic between VHDL and SV
- Need to trace through multiple assignments to understand logic
- Comparing two IRs requires manual mapping of signals

### After (Meaningful Names)
```verilog
logic clk_eq_n1;
logic clk_and_n2;
logic rst_eq_n6;
logic mux_n3_n4_n8;
logic n9_reg;

always @(posedge clk or posedge rst) begin
  if (rst) begin
    n9_reg <= 0;
  end else begin
    n9_reg <= mux_n3_n4_n8;
  end
end

assign clk_eq_n1 = clk == n1;
assign clk_and_n2 = clk & clk_eq_n1;
assign rst_eq_n6 = rst == n6;
assign mux_n3_n4_n8 = clk_and_n2 ? n8 : n4;
```

**Benefits:**
- `clk_eq_n1` clearly shows comparison of CLK signal
- `clk_and_n2` shows AND operation on CLK
- `mux_n3_n4_n8` shows it's a multiplexer
- `n9_reg` shows it's a register output
- Can immediately understand data flow
- Easy to spot equivalent operations across IRs

## Implementation

Created `opt_ir_naming.ml` module with:

### 1. Operation-Based Name Generation

Maps each operation type to a descriptive name pattern:

| Operation | Input Example | Generated Name |
|-----------|---------------|----------------|
| Add | `[a, b]` | `a_plus_b` |
| Sub | `[a, b]` | `a_minus_b` |
| Mul | `[a, b]` | `a_mul_b` |
| And | `[a, b]` | `a_and_b` |
| Or | `[a, b]` | `a_or_b` |
| Xor | `[a, b]` | `a_xor_b` |
| Not | `[a]` | `not_a` |
| Compare (Eq) | `[a, b]` | `a_eq_b` |
| Compare (Lt) | `[a, b]` | `a_lt_b` |
| Compare (Gt) | `[a, b]` | `a_gt_b` |
| Mux | `[sel, in0, in1]` | `mux_sel_in0_in1` |
| Register | `[d]` | `d_reg` |
| Shift Left | `[a]` | `a_shl` |
| Shift Right | `[a]` | `a_shr` |
| ZeroExtend | `[a]` (1→8) | `a_zext_1to8` |
| SignExtend | `[a]` (1→8) | `a_sext_1to8` |
| Extract | `[a]` [7:4] | `a_7to4` |
| Concat | `[a, b, c]` | `concat_a_b_c` |

### 2. Name Sanitization

Ensures all names are valid Verilog identifiers:
- Converts to lowercase
- Replaces invalid characters with underscores
- Adds 'n' prefix if name starts with digit
- Removes array indices and bit selects from base names

### 3. Uniqueness Handling

When multiple nodes generate the same base name:
```ocaml
let count = try Hashtbl.find name_counts base_name with Not_found -> 0 in
let final_name =
  if count = 0 then base_name
  else Printf.sprintf "%s_%d" base_name count
in
```

Example:
- First AND: `a_and_b`
- Second AND: `a_and_b_1`
- Third AND: `a_and_b_2`

### 4. Dependency-Ordered Processing

Nodes are sorted by depth before naming:
```ocaml
let sorted_nodes = List.sort (fun (_, n1) (_, n2) ->
  compare n1.node_depth n2.node_depth
) nodes_list in
```

This ensures input names are available when generating output names.

### 5. Name Length Control

Long names are truncated to keep code readable:
```ocaml
let truncate_name name max_len =
  if String.length name > max_len then
    String.sub name 0 (max_len - 3) ^ "___"
  else
    name
```

Example: `mux_n16_const_0_mux_n14_n6_n13` (37 chars) is within limit (40)

## Integration

Modified `opt_ir_to_behavioral.ml`:

```ocaml
let convert_to_behavioral ir =
  (* Generate meaningful names for all nodes *)
  let id_to_name =
    if !use_meaningful_names then
      Opt_ir_naming.apply_naming_strategy ~verbose:!debug ir
    else
      (* Fall back to generic n1, n2, n3... names *)
      ...
  in
  ...
```

The naming strategy can be toggled with `use_meaningful_names` flag.

## Examples

### Example 1: slib_edge_detect (VHDL)

```verilog
module slib_edge_detect (
  input CLK,
  input RST
);
  logic clk_eq_n1;           // CLK == n1
  logic clk_and_n2;          // CLK & (CLK == n1)
  logic rst_eq_n6;           // RST == n6
  logic mux_n3_n4_n8;        // Mux based on (CLK & ...)
  logic n9_reg;              // Register output

  always @(posedge clk or posedge rst) begin
    if (rst) begin
      n9_reg <= 0;
    end else begin
      n9_reg <= mux_n3_n4_n8;
    end
  end

  assign clk_eq_n1 = clk == n1;
  assign clk_and_n2 = clk & clk_eq_n1;
  assign rst_eq_n6 = rst == n6;
  assign mux_n3_n4_n8 = clk_and_n2 ? n8 : n4;
endmodule
```

**Insights from names:**
- Logic checks if CLK equals something (likely detecting edge)
- Uses AND operation to combine conditions
- Muxes between two values based on condition
- Stores result in register

### Example 2: slib_edge_detect (SystemVerilog)

```verilog
module slib_edge_detect (
  output FE,
  output RE
);
  logic n4_reg;                             // Register holding n4
  logic n4_eq_const_1;                      // n4 == 1
  logic n4_eq_const_0;                      // n4 == 0
  logic n6_eq_const_1;                      // register_output == 1
  logic n6_eq_const_0;                      // register_output == 0
  logic n6_eq_const_1_and_n4_eq_const_0;   // Rising edge detection
  logic n12_and_n13;                        // Falling edge detection
  logic [31:0] fe;                          // Falling edge output
  logic [31:0] re;                          // Rising edge output

  always @(posedge n0 or posedge n0) begin
    if (n0) begin
      n4_reg <= 0;
    end else begin
      n4_reg <= n4;
    end
  end

  assign n4_eq_const_1 = n4 == const_1;
  assign n4_eq_const_0 = n4 == const_0;
  assign n6_eq_const_1 = n4_reg == const_1;
  assign n6_eq_const_0 = n4_reg == const_0;

  // Rising edge: old=0, new=1
  assign n6_eq_const_1_and_n4_eq_const_0 = n6_eq_const_1 & n4_eq_const_0;
  assign re = n6_eq_const_1_and_n4_eq_const_0 ? const_0 : const_1;

  // Falling edge: old=1, new=0
  assign n12_and_n13 = n6_eq_const_0 & n4_eq_const_1;
  assign fe = n12_and_n13 ? const_0 : const_1;

  assign const_0 = 32'd0;
  assign const_1 = 32'd1;
  assign FE = fe;
  assign RE = re;
endmodule
```

**Insights from names:**
- Compares current input to 0 and 1: `n4_eq_const_0`, `n4_eq_const_1`
- Compares registered (old) value to 0 and 1: `n6_eq_const_0`, `n6_eq_const_1`
- Detects rising edge: `n6_eq_const_1_and_n4_eq_const_0` (old=1 AND new=0)
- Detects falling edge: `n12_and_n13` (old=0 AND new=1)
- Clear edge detection pattern visible from names alone

### Example 3: slib_counter (VHDL)

```verilog
module slib_counter (
  input CLK,
  input RST
);
  logic n4_eq_n5;                               // Comparison
  logic n14_eq_n15;                             // Comparison
  logic n18_eq_n19;                             // Comparison
  logic [31:0] n7_plus_n12;                     // Addition
  logic [31:0] n7_minus_n8;                     // Subtraction
  logic [31:0] mux_n11_n13_mux_n16_n17_n26;    // Nested mux
  logic [31:0] mux_n6_n7_minus_n8_mux_n11___;  // Complex mux
  logic n29_reg;                                // Register
  logic n3_reg;                                 // Register

  always @(posedge clk or posedge rst) begin
    if (rst) begin
      n29_reg <= 0;
      n3_reg <= 0;
    end else begin
      n29_reg <= mux_n6_n7_minus_n8_mux_n11___;
      n3_reg <= n3;
    end
  end

  assign n7_plus_n12 = n29_reg + n12;
  assign n7_minus_n8 = n29_reg - n8;
  assign mux_n6_n7_minus_n8_mux_n11___ = n4_eq_n5 ? mux_n11_n13_mux_n16_n17_n26 : n7_minus_n8;
  assign mux_n11_n13_mux_n16_n17_n26 = n4_eq_n10 ? mux_n16_n17_n26 : n7_plus_n12;
  ...
endmodule
```

**Insights from names:**
- Counter uses addition and subtraction: `n7_plus_n12`, `n7_minus_n8`
- Has multiple comparison conditions: `n4_eq_n5`, `n14_eq_n15`, `n18_eq_n19`
- Uses nested muxes to select between operations
- Two registers maintain state

### Example 4: slib_counter (SystemVerilog)

```verilog
module slib_counter (
  input D,
  output OVERFLOW,
  output Q
);
  logic [31:0] n4_plus_const_1;                 // Increment
  logic [31:0] n4_minus_const_1;                // Decrement
  logic n10_and_n11;                            // Condition AND
  logic n14_or_n16;                             // Condition OR
  logic n10_or_n10_and_n11_or_n14_or_n16;      // Complex condition
  logic [4:0] mux_n10_and_n11_n4_plus_const___; // Inc/Dec mux
  logic [4:0] mux_n14_n6_n13;                   // Selection mux
  logic [4:0] n17_reg;                          // Counter register

  always @(posedge n0 or posedge n0) begin
    if (n0) begin
      n17_reg <= 0;
    end else begin
      n17_reg <= mux_n16_const_0_mux_n14_n6_n13;
    end
  end

  assign n4_plus_const_1 = n4 + const_1;
  assign n4_minus_const_1 = n4 - const_1;
  assign n10_and_n11 = n10 & n11;
  assign mux_n10_and_n11_n4_plus_const___ = n10_and_n11 ? n4_minus_const_1 : n4_plus_const_1;
  ...
endmodule
```

**Insights from names:**
- Counter increments/decrements by 1: `n4_plus_const_1`, `n4_minus_const_1`
- Uses AND and OR for control logic: `n10_and_n11`, `n14_or_n16`
- Muxes select between increment and decrement
- Register stores counter value: `n17_reg`

## Comparison Benefits

### Easy Pattern Spotting

**VHDL:**
```verilog
assign clk_and_n2 = clk & clk_eq_n1;
```

**SV:**
```verilog
assign n6_eq_const_1_and_n4_eq_const_0 = n6_eq_const_1 & n4_eq_const_0;
```

Both show AND operations, making them easy to align.

### Operation Type Identification

Can quickly scan for:
- All additions: `grep "_plus_" ir_dumps/*.v`
- All comparisons: `grep "_eq_\|_lt_\|_gt_" ir_dumps/*.v`
- All muxes: `grep "mux_" ir_dumps/*.v`
- All registers: `grep "_reg" ir_dumps/*.v`

### Constant Detection

Named constants reveal logic patterns:
```verilog
n4_eq_const_1      // Checking for 1 (true)
n4_eq_const_0      // Checking for 0 (false)
n4_plus_const_1    // Incrementing
n4_minus_const_1   // Decrementing
```

### Width Tracking

Names include width information where relevant:
```verilog
a_zext_1to8        // Zero extend from 1-bit to 8-bit
a_sext_4to32       // Sign extend from 4-bit to 32-bit
a_7to4             // Extract bits [7:4]
```

## Limitations and Undefined Signals

The naming strategy reveals issues more clearly:

### Undefined Input References

**VHDL:**
```verilog
assign clk_eq_n1 = clk == n1;  // What is n1?
```

**SV:**
```verilog
assign n4_reg <= n4;           // What is n4? (missing input)
```

Names like `n1`, `n4`, `n6` indicate:
- Missing port connections
- Undefined constants
- Parser extraction issues

### Clock/Reset Issues

**SV:**
```verilog
always @(posedge n0 or posedge n0) begin  // n0 = undefined clock/reset
```

The naming strategy helps spot these problems immediately.

## Technical Details

### Name Generation Algorithm

1. **Sanitize base names** from inputs (lowercase, remove invalid chars)
2. **Generate operation-specific pattern** based on operation type
3. **Combine input names** with operation pattern
4. **Check uniqueness** and add suffix if needed
5. **Truncate if too long** (max 40 characters)

### Processing Order

Nodes are processed in dependency order (by depth) so that when naming node N:
- All input nodes to N have already been named
- Can use meaningful input names in N's name generation

### Integration Point

```ocaml
(* In opt_ir_to_behavioral.ml *)
let id_to_name =
  if !use_meaningful_names then
    Opt_ir_naming.apply_naming_strategy ~verbose:!debug ir
  else
    (* Generic names: n1, n2, n3... *)
```

Can toggle between meaningful and generic names for comparison.

## Results

### Readability Improvement

**Generic names:** Need to trace through 5-10 lines to understand logic
**Meaningful names:** Understand logic from signal name alone

### Comparison Speed

**Before:** Manual mapping required, taking minutes per module
**After:** Visual scan reveals differences in seconds

### Debugging Efficiency

Names reveal:
- What operation each signal represents
- Which signals are related
- Missing or incorrect connections
- Logic flow patterns

### Documentation Value

IR dumps with meaningful names serve as:
- Self-documenting intermediate representation
- Visual guide to optimization decisions
- Comparison baseline for equivalence checking

## Future Improvements

### 1. Hierarchical Naming

For deeply nested expressions, could use hierarchical abbreviations:
```
Current: mux_n16_const_0_mux_n14_n6_n13
Better:  sel1_sel2_a_b  (with hierarchy encoded in structure)
```

### 2. Type-Based Naming

Include signal types in names:
```
clk_eq_const_1_bool    // Boolean result
counter_plus_1_u8      // 8-bit unsigned
```

### 3. Semantic Analysis

Detect common patterns and use semantic names:
```
detect_rising_edge     // Instead of: n6_eq_const_1_and_n4_eq_const_0
counter_next_val       // Instead of: mux_inc_dec_rst
```

### 4. Alias Detection

When two signals are equivalent, use same base name:
```
// If analysis shows these are equivalent
a_plus_1   // VHDL
b_plus_1   // SV
// Could unify to: input_plus_1
```

## Conclusion

The meaningful naming strategy dramatically improves IR analysis by:

✅ **Making logic visible** - Names describe operations
✅ **Enabling pattern matching** - Easy to spot equivalent logic
✅ **Revealing issues** - Undefined signals stand out
✅ **Documenting flow** - Self-explanatory signal names
✅ **Speeding comparison** - Visual scanning vs manual mapping

This transforms IR dumps from cryptic node graphs into readable, analyzable behavioral descriptions that clearly show what logic is being performed and where VHDL and SystemVerilog IRs differ.
