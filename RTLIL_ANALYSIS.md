# Yosys RTLIL Format Analysis

## Overview

Investigation of Yosys RTLIL (Register Transfer Level Intermediate Language) parser from hardcaml-lua project to understand how to read and process synthesized netlists with high confidence in correctness.

## What is RTLIL?

**RTLIL** is Yosys's intermediate representation format used for:
- Internal netlist representation during synthesis
- Import/export of designs between tools
- Formal verification and equivalence checking
- Technology mapping and optimization

## File Format Structure

### Basic Syntax (from gold.il example)

```
# Comment
autoidx 1
attribute \top 1

module \blocking_add
  wire $abc$367$new_n100
  wire width 4 input 1 \a
  wire width 4 output 2 \c

  cell $_AND_ $abc$367$auto$blifparse.cc:396:parse_blif$368
    connect \A \a [3]
    connect \B \b [2]
    connect \Y $abc$367$new_n13
  end

  cell $_XOR_ $abc$367$auto$blifparse.cc:396:parse_blif$370
    connect \A $abc$367$new_n14
    connect \B $abc$367$new_n13
    connect \Y $abc$367$new_n15
  end
end
```

### Key Components

1. **Design Level**
   - `autoidx` - Auto-index counter
   - `attribute` - Design-level attributes
   - Multiple modules

2. **Module Level**
   - `module \name` ... `end`
   - Contains wires, cells, processes, connections

3. **Wire Declarations**
   ```
   wire [options] \signal_name
   ```
   Options:
   - `width N` - Signal width
   - `input N` - Input port with number
   - `output N` - Output port with number
   - `inout N` - Inout port
   - `signed` - Signed signal
   - `upto` - Up-to range
   - `offset N` - Bit offset

4. **Cell Instantiations**
   ```
   cell \cell_type \instance_name
     parameter \name value
     connect \pin_name signal_spec
   end
   ```

5. **Signal Specifications (sigspec)**
   - Simple: `\signal_name`
   - Bit select: `\signal[3]`
   - Range: `\signal[3:0]`
   - Concatenation: `{ \a \b \c }`
   - Constants: `4'b1010` or direct values

6. **Processes** (Sequential Logic)
   ```
   process \name
     switch \condition
       case 1'1
         assign \dest \source
       end
     end
     sync posedge \clk
       update \signal \new_value
     end
   end
   ```

7. **Direct Connections**
   ```
   connect \signal1 \signal2
   ```

## Grammar Analysis (Rtlil_input.mly)

### Token Types

**Structural Tokens:**
- `TOK_MODULE`, `TOK_END` - Module boundaries
- `TOK_WIRE`, `TOK_CELL` - Component declarations
- `TOK_PROCESS` - Sequential logic blocks
- `TOK_CONNECT` - Wire connections

**Port/Signal Tokens:**
- `TOK_INPUT`, `TOK_OUTPUT`, `TOK_INOUT` - Port directions
- `TOK_WIDTH`, `TOK_OFFSET`, `TOK_SIZE` - Bit specifications
- `TOK_SIGNED`, `TOK_UPTO` - Signal attributes

**Process Tokens:**
- `TOK_SWITCH`, `TOK_CASE` - Conditional logic
- `TOK_ASSIGN` - Assignment within process
- `TOK_SYNC` - Synchronization specification
- `TOK_UPDATE` - Sequential update
- `TOK_POSEDGE`, `TOK_NEGEDGE`, `TOK_EDGE` - Edge types
- `TOK_ALWAYS`, `TOK_GLOBAL`, `TOK_INIT` - Sync types

**Value Tokens:**
- `TOK_ID` - Identifier (signal/module name)
- `TOK_INT` - Integer value
- `TOK_VALUE` - Bit vector value (e.g., "4'b1010")
- `TOK_STRING` - String literal

### Grammar Structure

```
design := module* attribute* autoidx*

module := module_stmt*
module_stmt :=
  | param_stmt
  | wire_stmt
  | memory_stmt
  | cell_stmt
  | proc_stmt
  | conn_stmt
  | attr_stmt

wire_stmt := wire wire_options ID

wire_options :=
  | width INT
  | input INT | output INT | inout INT
  | signed | upto
  | offset INT

cell_stmt := cell ID ID cell_body end
cell_body :=
  | parameter ID constant
  | connect ID sigspec

proc_stmt := process ID case_body sync_list end

sync_list := sync sync_type sigspec update_list

update_list := update sigspec sigspec

sigspec :=
  | constant
  | ID
  | sigspec[INT]
  | sigspec[INT:INT]
  | { sigspec_list }
```

## Type System (Rtlil_input_rewrite_types.ml)

### Structured AST Types

```ocaml
type ilang =
  (* Design level *)
  | Module12 of string * ilang list
  | Design6 of ilang list * ilang list
  | Attr_stmt of string * ilang list
  | Autoidx_stmt26 of int

  (* Wires *)
  | Wire_stmt of ilang list * string
  | Wire_optionswidth of int
  | Wire_optionsinput of int
  | Wire_optionsoutput of int
  | Wire_optionsinout of int
  | Wire_optionsoffset of int
  | Wire_optionsinvalid
  | Signed
  | Upto

  (* Cells *)
  | Cell_stmt of string * string * ilang list * ilang list
  | Cell_bodyconnect of ilang list * string * ilang list * ilang list
  | Cell_bodyparam of ilang list * string * ilang list * ilang list

  (* Processes *)
  | Proc_stmt of string * ilang list * ilang list * ilang list
  | Switch_stmt of ilang list * ilang list * ilang list * ilang list
  | Switch_bodycase of ilang list * ilang list * ilang list
  | Assign_stmt67 of ilang list * ilang list
  | Update_list82 of ilang list * ilang list

  (* Connections *)
  | Conn_stmt96 of ilang list * ilang list
  | TokConn of ilang list * ilang list

  (* Signal specs *)
  | Sigspec90 of string * int              (* bit select *)
  | Sigspecrange of string * int * int     (* range *)
  | Sigspec92 of ilang list                (* concatenation *)

  (* Values *)
  | TokInt of int
  | TokID of string
  | TokVal of string
  | TokStr of string

  (* Edge types *)
  | TokPos | TokNeg | TokEdge
```

## Standard Cell Types in RTLIL

### Logic Gates
- `$_AND_` - 2-input AND gate
- `$_NAND_` - 2-input NAND gate
- `$_OR_` - 2-input OR gate
- `$_NOR_` - 2-input NOR gate
- `$_XOR_` - 2-input XOR gate
- `$_XNOR_` - 2-input XNOR gate
- `$_NOT_` - Inverter
- `$_BUF_` - Buffer

### Arithmetic
- `$add` - Addition
- `$sub` - Subtraction
- `$mul` - Multiplication
- `$div` - Division
- `$mod` - Modulo

### Comparison
- `$eq` - Equal
- `$ne` - Not equal
- `$lt` - Less than
- `$le` - Less or equal
- `$gt` - Greater than
- `$ge` - Greater or equal

### Sequential
- `$_DFF_P_` - D flip-flop (positive edge)
- `$_DFF_N_` - D flip-flop (negative edge)
- `$_DFFE_PP_` - D flip-flop with enable
- `$_SDFF_PP0_` - D flip-flop with sync reset
- `$_ADFF_PP0_` - D flip-flop with async reset

### Multiplexers
- `$_MUX_` - 2:1 multiplexer
- `$mux` - Parametric multiplexer

### Memory
- `$mem` - Memory block
- `$memrd` - Memory read port
- `$memwr` - Memory write port

## Parsing Flow

1. **Lexer** (Rtlil_input_lex.mll)
   - Tokenizes RTLIL text
   - Handles identifiers, numbers, bit vectors
   - Recognizes keywords

2. **Parser** (Rtlil_input.mly)
   - Builds parse tree with TUPLE/CONS nodes
   - Maintains structure through grammar rules
   - Creates intermediate token-based AST

3. **Rewriter** (Rtlil_input_rewrite.ml)
   - Function `rw`: Simplifies parse tree structure
   - Function `rw'`: Converts to structured ilang types
   - Function `rw''`: Reverses lists for correct order
   - Result: Clean structured AST

4. **Dumper** (Rtlil_dump.ml)
   - Converts ilang back to RTLIL text
   - Used for verification and output

## Key Insights for SystemVerilog Decompiler

### 1. Advantages of RTLIL

**Structured Format:**
- Well-defined grammar
- Unambiguous semantics
- Clear cell types and connections
- Standard cell naming conventions

**Rich Information:**
- Port directions and widths
- Signal attributes (signed, etc.)
- Parameter values on cells
- Process/sync information for sequential logic

**Tool Support:**
- Yosys widely used in open-source synthesis
- Established format with good documentation
- Formal verification tools support RTLIL
- Easy to generate and validate

### 2. Comparison with Liberty-based Approach

| Aspect | RTLIL | Liberty+Verilog |
|--------|-------|-----------------|
| Format | Line-based text | Liberty + structural Verilog |
| Cell types | Yosys standard cells | Technology-specific cells |
| Semantics | Built-in, unambiguous | Inferred from Liberty functions |
| Sequential | Explicit process blocks | Inferred from cell types |
| Parsing | Single format | Two formats (lib + v) |
| Confidence | Very high | Medium (heuristic-based) |

### 3. Integration Strategy

**Phase 1: RTLIL Reader**
- Implement OCaml parser (can adapt from hardcaml-lua)
- Or implement simpler line-based parser in OCaml
- Convert RTLIL to internal netlist representation

**Phase 2: RTLIL to Internal IR**
- Map RTLIL cells to internal operations
- Handle signal specifications (bit select, range, concat)
- Process synchronous blocks (processes)

**Phase 3: RTL Reconstruction**
- Apply pattern matching to RTLIL cells
- Reconstruct high-level operations
- Recognize multi-bit operations
- Identify FSMs and datapaths

**Phase 4: Equivalence Checking**
- Use RTLIL as golden reference
- Compare behavioral RTL against RTLIL
- Leverage Yosys for formal verification

### 4. Benefits for Correctness

**High Confidence:**
- RTLIL has precise semantics (no ambiguity)
- Can validate against Yosys
- Formal verification possible
- Established format with test cases

**Better Pattern Recognition:**
- Standard cell types are well-known
- Clear indication of cell function
- Easier to recognize adders, multipliers, etc.
- Process blocks show synchronous structure

**Verification:**
- Can round-trip through Yosys
- Formal equivalence checking available
- Test against Yosys's own transformations

## Implementation Plan

### Minimal RTLIL Reader

```ocaml
(* sv_rtlil_reader.ml *)

type rtlil_wire = {
  name: string;
  width: int option;
  port_dir: port_direction option;
  port_num: int option;
  signed: bool;
}

type rtlil_connection = {
  pin: string;
  sigspec: signal_spec;
}

type rtlil_cell = {
  cell_type: string;
  instance_name: string;
  parameters: (string * string) list;
  connections: rtlil_connection list;
}

type rtlil_module = {
  module_name: string;
  wires: rtlil_wire list;
  cells: rtlil_cell list;
  connections: (string * string) list;
}

let parse_rtlil_file filename = ...
let rtlil_to_netlist rtlil_module = ...
let reconstruct_rtl netlist = ...
```

### Integration with Existing Code

```ocaml
(* In sv_netlist_reader.ml *)

(* Add RTLIL support alongside Liberty *)
let read_netlist_from_rtlil rtlil_file =
  let rtlil = Sv_rtlil_reader.parse_rtlil_file rtlil_file in
  let netlist = Sv_rtlil_reader.rtlil_to_netlist rtlil in
  netlist

(* Pattern matching benefits from RTLIL's explicit cell types *)
let recognize_operation cell =
  match cell.cell_type with
  | "$_AND_" -> Some (RtlAnd ...)
  | "$_OR_" -> Some (RtlOr ...)
  | "$add" -> Some (RtlAdd ...)
  | "$mux" -> Some (RtlMux ...)
  | _ -> None
```

## Example Workflow

```bash
# 1. Synthesize design with Yosys
yosys -p "read_verilog design.v; proc; opt; write_rtlil design.il"

# 2. Read RTLIL into decompiler
sv_main_unified read-rtlil design.il --output design_reconstructed.v

# 3. Verify equivalence
yosys -p "
  read_verilog design.v
  read_verilog design_reconstructed.v
  equiv_make design_v design_reconstructed_v equiv
  equiv_simple equiv
  equiv_status equiv
"

# 4. Use for gate mapping with technology
yosys -p "
  read_rtlil design.il
  techmap -map tech.lib
  write_verilog design_mapped.v
"
```

## Test Cases

### Simple AND Gate
```
module \test
  wire input 1 \a
  wire input 2 \b
  wire output 3 \y

  cell $_AND_ $and_1
    connect \A \a
    connect \B \b
    connect \Y \y
  end
end
```

Expected reconstruction:
```verilog
module test (
  input a,
  input b,
  output y
);
  assign y = a & b;
endmodule
```

### D Flip-Flop
```
module \test
  wire input 1 \clk
  wire input 2 \d
  wire output 3 \q

  cell $_DFF_P_ $ff_1
    connect \C \clk
    connect \D \d
    connect \Q \q
  end
end
```

Expected reconstruction:
```verilog
module test (
  input clk,
  input d,
  output reg q
);
  always @(posedge clk)
    q <= d;
endmodule
```

## Recommendations

1. **Immediate: Simple RTLIL Line Parser**
   - Don't need full OCaml parser initially
   - Line-based parsing sufficient for basic cases
   - Focus on wire and cell statements

2. **Near-term: Full RTLIL Support**
   - Adapt hardcaml-lua parser
   - Handle all RTLIL constructs
   - Support processes for sequential logic

3. **Long-term: Yosys Integration**
   - Use RTLIL as interchange format
   - Leverage Yosys for synthesis and verification
   - Formal equivalence checking

4. **Validation**
   - Test against Yosys-generated RTLIL
   - Round-trip through Yosys
   - Compare with Liberty-based approach
   - Formal verification where possible

## Conclusion

Yosys RTLIL provides a **high-confidence** intermediate format for reading synthesized netlists. Key advantages:

✅ **Well-defined semantics** - No ambiguity
✅ **Standard cell types** - Clear operation mapping
✅ **Tool support** - Yosys ecosystem
✅ **Verification** - Formal methods available
✅ **Structure** - Easier pattern recognition

Implementing RTLIL support would significantly improve confidence in correctness of the gate-to-RTL transformation, especially when combined with formal equivalence checking through Yosys.

**Confidence Level**: Using RTLIL as source → **VERY HIGH**
**Recommended Priority**: Implement after basic Liberty/Verilog approach is working, use for validation and improved accuracy.

## Test Results

### Implementation Status: ✅ COMPLETE

**Files Created**:
- `sv_rtlil_reader.ml` (326 lines) - Line-based RTLIL parser
- `test_rtlil_reader.ml` (78 lines) - Test program

**Test Execution**: `_build/default/test_rtlil_reader.exe`

### Synthetic Test Results
Created test file with 3 modules:

**Module: test_and**
- Wires: 3 (2 inputs, 1 output)
- Cells: 1 ($_AND_)
- Status: ✅ PASS

**Module: test_add**
- Wires: 3 (2 inputs [4-bit], 1 output [4-bit])
- Cells: 1 ($add with parameters)
- Status: ✅ PASS

**Module: test_dff**
- Wires: 3 (2 inputs, 1 output)
- Cells: 1 ($_DFF_P_)
- Status: ✅ PASS

### Real Yosys Output Test
Parsed: `/Users/jonathan/hardcaml-lua/blocking_add/gold.il`

**Module: blocking_add**
- Wires: 106 (2 inputs [4-bit], 1 output [4-bit], 103 internal)
- Total Cells: 107
- Cell Breakdown:
  - $_XOR_ (XOR): 39 gates
  - $_ANDNOT_: 25 gates
  - $_AND_ (AND): 12 gates
  - $_OR_ (OR): 11 gates
  - $_NAND_ (NAND): 10 gates
  - $_XNOR_ (XNOR): 5 gates
  - $_ORNOT_: 3 gates
  - $_NOT_ (NOT): 2 gates
- Status: ✅ PASS

### Bug Fixes Applied
**Bug**: Parser only counted first cell, then immediately finished module
**Root Cause**: `finish_module()` called after every `finish_cell()`
**Fix**: Check if currently in cell before deciding to finish cell vs module:
```ocaml
| "end" :: _ ->
    if !current_cell <> None then
      finish_cell ()
    else
      finish_module ()
```

### Conclusion
RTLIL reader successfully implemented and tested with both synthetic and real Yosys-generated netlists. Parser correctly handles:
- Module boundaries
- Wire declarations with widths and port directions
- Cell instantiations with parameters and connections
- Multiple cells per module
- Complex gate-level netlists (100+ cells)

**Ready for integration** with RTL collapse module for high-confidence gate-to-RTL transformation.
