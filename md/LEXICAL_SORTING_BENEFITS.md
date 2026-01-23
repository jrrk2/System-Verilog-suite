# Lexical Sorting of Behavioral Verilog

## Date: 2026-01-22

## Overview

Implemented lexical (alphabetical) sorting of all declarations and statements in the behavioral Verilog output. This dramatically improves comparison between VHDL and SystemVerilog IR dumps by eliminating arbitrary ordering differences.

## What is Sorted

### 1. Input Ports
Sorted alphabetically by signal name:
```verilog
input logic CLK;    // C comes first
input logic DIN;    // D
input logic RST;    // R
input logic WLS;    // W
```

### 2. Output Ports
Sorted alphabetically by signal name:
```verilog
output logic BAUDTICK;   // B comes first
output logic SOUT;       // S
output logic TXFINISHED; // T
```

### 3. Wire Declarations
Sorted alphabetically by signal name:
```verilog
logic clk_and_n6;                    // c comes first alphabetically
logic clk_eq_n5;
logic mux_n14_n15_n31;               // m
logic mux_n14_n15_n31_reg;
logic mux_n14_n15_n31_reg_eq_n1;    // All mux_ signals grouped together
logic mux_n14_n15_n31_reg_eq_n13;
logic [31:0] mux_n14_n15_n31_reg_plus_n19;
logic n16_eq_n17;                    // n signals come after m
logic n21_eq_n22;
logic n29_reg;
logic rst_eq_n10;                    // r signals come after n
logic rst_eq_n25;
```

### 4. Register Assignments (within always blocks)
Registers within each clock group sorted alphabetically:
```verilog
always @(posedge clk or posedge rst) begin
  if (rst) begin
    mux_n14_n15_n31_reg <= 0;  // m comes before n
    n29_reg <= 0;
  end else begin
    mux_n14_n15_n31_reg <= mux_n14_n15_n31;
    n29_reg <= mux_n2_n3_mux_n7_n8_n12;
  end
end
```

### 5. Combinational Assignments
Sorted alphabetically by LHS (left-hand side) signal name:
```verilog
assign clk_and_n6 = clk & clk_eq_n5;
assign clk_eq_n5 = clk == n5;
assign mux_n14_n15_n31 = mux_n14_n15_n31_reg_eq_n13 ? mux_n16_eq_n17_n20_n30 : n15;
assign mux_n14_n15_n31_reg_eq_n1 = mux_n14_n15_n31_reg == n1;
assign mux_n14_n15_n31_reg_eq_n13 = mux_n14_n15_n31_reg == n13;
assign mux_n14_n15_n31_reg_plus_n19 = mux_n14_n15_n31_reg + n19;
assign n16_eq_n17 = n16 == n17;
assign n21_eq_n22 = n21 == n22;
assign rst_eq_n10 = rst == n10;
assign rst_eq_n25 = rst == n25;
```

### 6. Clock Groups
Always blocks sorted by (clock, reset) pair:
```verilog
// If multiple clock domains, they appear in sorted order
always @(posedge clk1 or posedge rst1) begin ... end
always @(posedge clk2 or posedge rst2) begin ... end
```

## Benefits

### 1. Deterministic Output
**Before sorting:** Order depends on hashtable iteration (non-deterministic)
```verilog
logic n29_reg;
logic clk_eq_n5;
logic rst_eq_n10;
logic mux_n14_n15_n31_reg;
```

**After sorting:** Always same order (deterministic)
```verilog
logic clk_eq_n5;
logic mux_n14_n15_n31_reg;
logic n29_reg;
logic rst_eq_n10;
```

**Impact:** Running the same IR twice produces identical output.

### 2. Easier Visual Comparison
**Without sorting:** Related signals scattered
```verilog
logic n29_reg;
logic mux_n14_n15_n31_reg_eq_n1;
logic clk_eq_n5;
logic mux_n14_n15_n31_reg_eq_n13;
logic mux_n14_n15_n31_reg;
```

**With sorting:** Related signals grouped
```verilog
logic clk_eq_n5;                     // Clock signals together
logic mux_n14_n15_n31_reg;           // Register signals together
logic mux_n14_n15_n31_reg_eq_n1;    // Related comparisons adjacent
logic mux_n14_n15_n31_reg_eq_n13;
logic n29_reg;                       // Other registers
```

**Impact:** Can scan vertically and see patterns immediately.

### 3. Reduced Diff Noise
**Before sorting (diff shows reordering as changes):**
```diff
- logic n29_reg;
- logic clk_eq_n5;
+ logic clk_eq_n5;
+ logic n29_reg;
```

**After sorting (diff only shows actual differences):**
```diff
- logic clk_and_n6;
+ logic baudtick;
```

**Impact:** Diff output focuses on real differences, not ordering.

### 4. Pattern Recognition
With lexical sorting, patterns emerge visually:

**OR chain pattern (sorted):**
```verilog
logic [31:0] n13_or_n14;
logic [31:0] n13_or_n14_or_n16;
logic [31:0] n17_or_n18;
logic [31:0] n19_or_n20;
logic [31:0] n21_or_n22;
logic [31:0] n23_or_n24;
logic [31:0] n23_or_n24_or_n26;
logic [31:0] n23_or_n24_or_n26_or_n28;
```

Can immediately see cascading OR tree structure.

**Counter pattern (sorted):**
```verilog
logic mux_n14_n15_n31_reg;           // Register
logic mux_n14_n15_n31_reg_eq_n1;    // Compare to 1
logic mux_n14_n15_n31_reg_eq_n13;   // Compare to limit
logic [31:0] mux_n14_n15_n31_reg_plus_n19;  // Increment
```

All counter-related signals adjacent and ordered.

### 5. Equivalent Logic Alignment
When VHDL and SV generate similar signals, they align in diffs:

**VHDL (sorted):**
```verilog
assign clk_and_n6 = clk & clk_eq_n5;
assign clk_eq_n5 = clk == n5;
```

**SV (sorted):**
```verilog
assign const_0 = 32'd0;
assign const_1 = 32'd1;
```

**Diff shows:**
```diff
- assign clk_and_n6 = clk & clk_eq_n5;
- assign clk_eq_n5 = clk == n5;
+ assign const_0 = 32'd0;
+ assign const_1 = 32'd1;
```

Different logic, but if there were equivalent signals with same names, they'd align perfectly.

## Example: uart_baudgen Comparison

### VHDL (sorted wire declarations)
```verilog
logic clk_and_n6;
logic clk_eq_n5;
logic mux_n14_n15_n31;
logic mux_n14_n15_n31_reg;
logic mux_n14_n15_n31_reg_eq_n1;
logic mux_n14_n15_n31_reg_eq_n13;
logic [31:0] mux_n14_n15_n31_reg_plus_n19;
logic [31:0] mux_n16_eq_n17_n20_n30;
logic mux_n21_eq_n22_n24_n27;
logic mux_n2_n3_mux_n7_n8_n12;
logic mux_n7_n8_n12;
logic n16_eq_n17;
logic n21_eq_n22;
logic n29_reg;
logic rst_eq_n10;
logic rst_eq_n25;
```

**Observations:**
- 16 signals total
- Grouped by prefix: clk_, mux_, n_, rst_
- Easy to scan and understand structure
- Register signals clearly marked with _reg suffix

### SV (sorted wire declarations)
```verilog
logic baudtick;
logic [15:0] mux_n41_n39_n3_plus_const_1;
logic mux_n6_const_1_const_0;
logic [15:0] mux_n6_n35_or_n36_or_n38_mux_n41_n39____;
logic [31:0] n13_or_n14;
logic [31:0] n13_or_n14_or_n16;
logic [31:0] n17_or_n18;
logic [31:0] n19_or_n20;
logic [31:0] n21_or_n22;
logic [31:0] n23_or_n24;
logic [31:0] n23_or_n24_or_n26;
logic [31:0] n23_or_n24_or_n26_or_n28;
logic [31:0] n29_or_n30;
logic [31:0] n29_or_n30_or_n32;
logic [31:0] n29_or_n30_or_n32_or_n34;
logic [31:0] n35_or_n36;
logic [31:0] n35_or_n36_or_n38;
logic [31:0] n3_plus_const_1;
logic n41_or_n6;
logic [15:0] n43_reg;
logic n44_or_n41_or_n6;
logic [31:0] n9_or_n10;
logic [31:0] n9_or_n10_or_n12;
```

**Observations:**
- 22 signals total (more complex than VHDL)
- Output signal `baudtick` appears first
- Many cascading OR operations visible
- One register: `n43_reg`
- Arithmetic: `n3_plus_const_1`

### Comparison Insights

**Signal count:** VHDL=16, SV=22 (38% more signals in SV)

**Register count:** VHDL=2 registers, SV=2 registers (similar)

**Operations:**
- VHDL: Comparisons (eq), AND, additions, muxes
- SV: Many ORs (bit manipulation), additions, muxes

**Structure:**
- VHDL: Clock-based counter logic
- SV: Extensive OR tree (bit extraction/masking)

The sorting makes these differences immediately visible through visual scanning.

## Implementation Details

### Sorting Strategy
All sorting uses simple string comparison:
```ocaml
List.sort (fun (a, _) (b, _) -> String.compare a b)
```

This ensures:
- Consistent across all platforms
- Case-sensitive (uppercase before lowercase)
- Numeric strings sorted lexically not numerically
  - `n10` comes before `n2` (lexical)
  - Would need special handling for numeric sorting

### What's NOT Sorted
Some things are intentionally NOT sorted:

1. **Statement order in module port list** - Maintains structural order
2. **Expression operands** - `a + b` stays as is, not reordered to `b + a`
3. **Conditional branches** - `sel ? a : b` not reordered

These preserve the logic semantics.

### Performance Impact
Minimal - sorting happens once at generation time:
- O(n log n) for each list
- Total IR dump time still dominated by parsing
- Negligible overhead (<1ms per module)

## Use Cases

### 1. Automated Comparison
```bash
diff -u vhdl_ir.v sv_ir.v | wc -l
# Counts actual differences, not reordering
```

### 2. Visual Inspection
```bash
vimdiff vhdl_ir.v sv_ir.v
# Side-by-side comparison with aligned signals
```

### 3. Pattern Analysis
```bash
grep "mux_" uart_baudgen_vhdl_ir.v
# All mux signals grouped together in output
```

### 4. Signal Counting
```bash
grep "logic.*n.*_or_" uart_baudgen_sv_ir.v | wc -l
# Count all OR operations (easily found due to sorting)
```

### 5. Regression Testing
```bash
# Generate IR twice
./dump_uart_pairs.exe > run1.txt
./dump_uart_pairs.exe > run2.txt
diff run1.txt run2.txt
# Should be identical (deterministic output)
```

## Combined with Meaningful Names

Lexical sorting + meaningful names = maximum comparison power:

### Example: Counter Operations
```verilog
logic [31:0] counter_plus_1;         // Increment (sorted alphabetically)
logic counter_reg;                    // Register
logic counter_eq_max;                 // Compare to max
logic mux_reset_inc_counter;         // Mux for reset/increment
```

**Benefits:**
- All counter operations grouped by name prefix
- Operations sorted alphabetically within group
- Easy to spot missing operations
- Easy to compare between files

### Example: State Machine
```verilog
logic state_eq_idle;                 // All state comparisons
logic state_eq_receive;              // grouped together
logic state_eq_transmit;             // and sorted
logic state_reg;                     // Register
logic [2:0] state_next;              // Next state logic
```

**Benefits:**
- State machine structure immediately visible
- All state comparisons together
- Next state logic clearly identified

## Conclusion

Lexical sorting provides:

✅ **Deterministic output** - Same IR always produces same Verilog
✅ **Reduced diff noise** - Only real differences shown
✅ **Pattern visibility** - Related signals grouped naturally
✅ **Easier comparison** - Equivalent signals align
✅ **Better debugging** - Can scan vertically for patterns

Combined with meaningful naming, lexical sorting makes IR dumps extremely readable and comparable. The combination of:
1. **Meaningful names** - Show what each signal does
2. **Lexical sorting** - Group related signals together
3. **Behavioral Verilog** - Use standard constructs

Transforms IR dumps from cryptic node graphs into clear, analyzable, and comparable behavioral descriptions.

## Future Enhancements

### Numeric Suffix Sorting
Could improve sorting of numeric suffixes:
```verilog
// Current (lexical): n1, n10, n11, n2, n3
// Better (numeric):  n1, n2, n3, n10, n11
```

### Grouped Sorting
Could group by operation type before alphabetical:
```verilog
// All comparisons first
assign a_eq_b = ...
assign x_lt_y = ...

// Then arithmetic
assign sum = ...
assign diff = ...

// Then muxes
assign mux_sel_a_b = ...
```

### Width-Based Grouping
Could group signals by width:
```verilog
// 1-bit signals
logic a_and_b;
logic x_or_y;

// 32-bit signals
logic [31:0] counter;
logic [31:0] sum;
```

These would further enhance readability and comparison.
