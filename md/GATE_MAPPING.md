# Gate-Level Mapping and RTL Collapse

## Overview

The SystemVerilog decompiler now includes complete support for:
1. **Gate-level mapping** - converting high-level operations to standard cells
2. **Netlist reading** - parsing gate-level Verilog with cell instantiations
3. **RTL collapse** - reconstructing behavioral RTL from gate-level netlists

This enables full round-trip transformation: Behavioral RTL → Gates → Behavioral RTL

## Architecture

### Module Structure

```
sv_liberty.ml           - Liberty file parser (cell library)
sv_gate_map.ml          - Gate mapping (operations → cells)
sv_netlist_reader.ml    - Netlist reader (parses gate-level Verilog)
sv_rtl_collapse.ml      - RTL collapse (gates → behavioral)
test_gate_mapping.ml    - Test program demonstrating full flow
```

### Data Flow

```
Behavioral RTL
      ↓
  [Gate Mapping]
      ↓
Gate-Level Netlist (.v with cell instantiations)
      ↓
  [Netlist Reader]
      ↓
Internal Netlist Representation
      ↓
  [Pattern Recognition]
      ↓
  [RTL Collapse]
      ↓
Behavioral RTL (reconstructed)
```

## Gate Mapping (sv_gate_map.ml)

### Purpose
Maps high-level operations (AND, OR, XOR, etc.) to standard cells from a Liberty file.

### Key Features

1. **Cell Lookup by Operation Type**
   ```ocaml
   find_cell_for_op : library_info -> string -> (string * cell_info) option
   ```
   - Finds appropriate cell for operation (AND, OR, XOR, NOT, BUF)
   - Uses heuristics based on cell function expressions

2. **Single-Bit Gate Mapping**
   ```ocaml
   map_operation : library_info -> string -> string list -> string -> gate_instance option
   ```
   - Maps single-bit operation to gate instance
   - Connects input/output pins correctly

3. **Multi-Bit Operation Mapping**
   ```ocaml
   map_multibit_operation : library_info -> string -> string list list -> string -> int ->
     (gate_instance list * (string * int) list)
   ```
   - Instantiates multiple gates for multi-bit operations
   - Manages bit-level signals and wires

4. **Verilog Generation**
   ```ocaml
   verilog_of_mapped_netlist : mapped_netlist -> string
   ```
   - Generates structural Verilog with cell instantiations

### Example Usage

```ocaml
(* Load liberty file *)
let lib = Sv_liberty.parse_liberty_file "cells.lib" in

(* Find AND cell *)
match Sv_gate_map.find_cell_for_op lib "AND" with
| Some (cell_name, cell) ->
    Printf.printf "AND maps to %s\n" cell_name
| None -> Printf.printf "No AND cell found\n"

(* Map operation *)
let inst = Sv_gate_map.map_operation lib "AND" ["a"; "b"] "y" in

(* Generate netlist *)
let netlist = {
  module_name = "my_and";
  inputs = [("a", 1); ("b", 1)];
  outputs = [("y", 1)];
  wires = [];
  instances = [inst];
} in
let verilog = Sv_gate_map.verilog_of_mapped_netlist netlist in
print_endline verilog
```

### Output Example

```verilog
module my_and (
  a,
  b,
  y
);

  input a;
  input b;
  output y;

  AND2 u1 (.A1(a), .A2(b), .ZN(y));

endmodule
```

## Netlist Reading (sv_netlist_reader.ml)

### Purpose
Parses gate-level Verilog netlists and builds internal representation using Liberty file for cell understanding.

### Key Features

1. **Netlist Data Structure**
   ```ocaml
   type netlist = {
     top_module: string;
     net_inputs: net_signal list;
     net_outputs: net_signal list;
     net_wires: net_signal list;
     net_instances: net_instance list;
   }
   ```

2. **Expression Building**
   ```ocaml
   build_expr_for_signal : library_info -> netlist -> string -> expr option
   ```
   - Traces signal through gates to build expression tree
   - Uses Liberty function expressions for each cell

3. **Function Parsing**
   ```ocaml
   parse_function_expr : string -> (string * string) list -> expr
   ```
   - Parses Liberty function syntax: "(A1 & A2)", "(A ^ B)", etc.
   - Maps pin names to actual signal names

### Expression Type

```ocaml
type expr =
  | EVar of string
  | EAnd of expr * expr
  | EOr of expr * expr
  | EXor of expr * expr
  | ENot of expr
  | EConst of int
```

### Example

For a netlist:
```verilog
AND2 u1 (.A1(a), .A2(b), .ZN(w1));
OR2 u2 (.A1(w1), .A2(c), .ZN(y));
```

The expression for `y` would be:
```ocaml
EOr (EAnd (EVar "a", EVar "b"), EVar "c")
```

String representation: `((a & b) | c)`

## RTL Collapse (sv_rtl_collapse.ml)

### Purpose
Reconstructs behavioral RTL from gate-level netlists through pattern recognition.

### Key Features

1. **Pattern Matchers**
   - `match_and_pattern` - recognizes AND gates
   - `match_or_pattern` - recognizes OR gates
   - `match_xor_pattern` - recognizes XOR gates
   - `match_not_pattern` - recognizes NOT gates
   - `match_mux_pattern` - recognizes MUX: `(s & a) | (!s & b)`
   - `match_multibit_and` - recognizes multi-bit AND operations

2. **RTL Operations**
   ```ocaml
   type rtl_operation =
     | RtlAnd of string * string * string      (* out = a & b *)
     | RtlOr of string * string * string       (* out = a | b *)
     | RtlXor of string * string * string      (* out = a ^ b *)
     | RtlNot of string * string               (* out = !a *)
     | RtlAdd of string * string * string      (* out = a + b *)
     | RtlSub of string * string * string      (* out = a - b *)
     | RtlMux of string * string * string * string  (* out = sel ? a : b *)
     | RtlAssign of string * expr              (* out = expr *)
   ```

3. **Behavioral Verilog Generation**
   ```ocaml
   verilog_of_rtl_module : rtl_module -> string
   ```
   - Generates clean behavioral Verilog with assign statements
   - Reconstructs high-level operations

### Pattern Recognition

The system recognizes:

- **Basic Gates**: AND, OR, XOR, NOT, BUF
- **Multiplexers**: `(s & a) | (!s & b)` → `s ? a : b`
- **Multi-bit Operations**: Parallel single-bit gates → vector operation
- **Generic Expressions**: Fallback for complex logic

### Example Transformation

**Input (Gate-level):**
```verilog
module example (a, b, s, y);
  input a, b, s;
  output y;

  wire n1, n2, ns;

  AND2 u1 (.A1(s), .A2(a), .ZN(n1));
  INV u2 (.I(s), .ZN(ns));
  AND2 u3 (.A1(ns), .A2(b), .ZN(n2));
  OR2 u4 (.A1(n1), .A2(n2), .ZN(y));
endmodule
```

**Output (Behavioral RTL):**
```verilog
module example (
  a,
  b,
  s,
  y
);

  input a;
  input b;
  input s;
  output y;

  assign y = s ? a : b;

endmodule
```

## Testing

### Test Program (test_gate_mapping.ml)

The test program demonstrates:

1. **Liberty File Loading**
   - Creates test liberty file with basic cells (AND2, OR2, XOR2, INV, BUF)
   - Loads and validates cell definitions

2. **Cell Lookup**
   - Tests finding cells for each operation type
   - Verifies correct cell selection

3. **Gate Mapping**
   - Creates gate-level netlist
   - Generates structural Verilog

4. **RTL Collapse** (example structure)
   - Shows netlist representation
   - Demonstrates pattern matching flow

### Running Tests

```bash
# Build test program
dune build test_gate_mapping.exe

# Run tests
_build/default/test_gate_mapping.exe
```

### Expected Output

```
=== Gate Mapping Test ===

Loading liberty file: test_cells.lib
Loaded 5 cells

Testing cell lookup:
  AND -> AND2
  OR -> OR2
  XOR -> XOR2
  NOT -> INV
  BUF -> BUF

Creating test gate-level netlist:
module test_and (
  a,
  b,
  y
);

  input a;
  input b;
  output y;


  AND2 u1 (.A1(a), .A2(b), .ZN(y));

endmodule

Written gate-level netlist to test_gates.v

Gate mapping test completed!

=== All Tests Completed Successfully ===
```

## Integration with HardCaml Backend

### Future Integration Points

1. **Optional Gate Mapping in sv_gen_hardcaml.ml**
   ```ocaml
   let generate_with_mapping lib circuit =
     (* Map HardCaml operations to gates *)
     match lib with
     | Some liberty_lib ->
         (* Use gate mapping *)
         Sv_gate_map.map_circuit liberty_lib circuit
     | None ->
         (* Use behavioral generation *)
         generate_behavioral circuit
   ```

2. **Command-Line Flag**
   ```bash
   sv_main_unified file hardcaml input.json output.v --liberty cells.lib --map-gates
   ```

3. **Round-Trip Verification**
   ```bash
   # Original → Gates → RTL
   sv_main_unified file hardcaml original.json mapped.v --liberty cells.lib --map-gates
   sv_main_unified collapse mapped.v collapsed.v --liberty cells.lib
   # Verify: original.v ≈ collapsed.v
   ```

## Use Cases

### 1. Technology Mapping for ASIC
```bash
# Map design to specific technology library
sv_main_unified file hardcaml design.json design_mapped.v \
  --liberty /path/to/tech.lib --map-gates
```

### 2. Gate-Level Simulation Preparation
```bash
# Convert behavioral to gates for gate-level simulation
sv_main_unified file hardcaml behavioral.json gates.v \
  --liberty cells.lib --map-gates
```

### 3. Reverse Engineering
```bash
# Collapse gate netlist back to behavioral RTL
sv_main_unified collapse gates.v behavioral.v \
  --liberty cells.lib
```

### 4. Library Evaluation
```bash
# Compare different cell libraries
for lib in lib1.lib lib2.lib lib3.lib; do
  sv_main_unified file hardcaml design.json mapped_$lib.v \
    --liberty $lib --map-gates
  # Compare area, delay, power
done
```

## Implementation Notes

### Current Limitations

1. **AST Integration**: Netlist reader currently has placeholder for full Sv_ast integration
2. **Sequential Cells**: FF/latch mapping needs extension
3. **Complex Patterns**: Only basic patterns currently recognized
4. **Multi-Output Cells**: Not yet supported

### Future Enhancements

1. **Complete AST Integration**
   - Full Verilator JSON parsing for gate-level netlists
   - Support for all Sv_ast node types

2. **Advanced Pattern Recognition**
   - Half/full adder detection
   - Counter patterns
   - FSM reconstruction
   - Datapath identification

3. **Optimization**
   - Drive strength selection
   - Area/delay/power optimization
   - Technology-specific optimizations

4. **Sequential Logic**
   - FF/latch mapping
   - Clock domain handling
   - Reset network management

## References

- Liberty format specification
- rtl_map.ml from hardcaml-lua (inspiration)
- LIBERTY_SUPPORT.md (Liberty parser documentation)
- sv_gen_hardcaml.ml (HardCaml backend)
