# VHDL Parser Integration

## Overview

This document describes the integration of the VHDL parser from `~/gnusynthesis/vhd_front/` into the SystemVerilog decompiler project. This enables direct comparison between VHDL source files and their SystemVerilog translations using Z3-based formal verification.

## Why This Matters

Previously, we compared our SystemVerilog decompiler output against Verilator's IR, which has bugs (missing nodes, wrong widths). Now we can compare against the **original VHDL source**, which is the ground truth for expected behavior.

## Files Added

### Core Integration Files

1. **vhdl_parse.ml** - VHDL parser wrapper
   - Parses .vhd files using the vhd_front library
   - Returns VHDL AST (Abstract Syntax Tree)
   - Error handling and reporting

2. **vhdl_elaborate.ml** - VHDL elaborator
   - Extracts entity and architecture from AST
   - Identifies processes and assignments
   - Prepares for IR conversion

3. **test_vhdl_parse.ml** - Parser test program
   - Tests parsing of UART module VHDL files
   - Validates parser integration

4. **test_vhdl_elaborate.ml** - Elaboration test program
   - Tests extraction of architecture info
   - Shows module structure

5. **vhdl_semantic_checker.ml** - Expected behavior reference
   - Documents expected IR for each pattern
   - Reference for understanding correct semantics

### Library Files

Located in **vhd_libs/**:
- `ver_front.cma` - Verilog front-end library (1.1MB)
- `vhd_front.cma` - VHDL front-end library (2.3MB)
- `*.mli` - Interface source files (11 files)
- `*.ml` - Key source files (VhdlTypes, VhdlTree, etc.)

Note: We copy `.mli` interface source files (not `.cmi` compiled interfaces)
because they can be recompiled for any OCaml version, while `.cmi` files
are version-specific and cause compatibility issues.

## Dependencies

The VHDL parser requires:
- **num** - Arbitrary precision arithmetic (for Big_int)
- **menhirLib** - Parser generator runtime
- **str** - String operations
- **unix** - Unix system calls

Installed via opam:
```bash
opam install num menhir
```

## Building the VHDL Libraries

The libraries in `vhd_libs/` were rebuilt from source:

```bash
# Build ver_front (Verilog frontend)
cd ~/gnusynthesis/ver_front
make clean
make ocamlyacc

# Build vhd_front (VHDL frontend)
cd ~/gnusynthesis/vhd_front
make clean
make vhd_front.cma

# Copy to project (using .mli source files, not .cmi compiled)
cp ~/gnusynthesis/ver_front/ver_front.cma ~/System-Verilog-decompiler/vhd_libs/
cp ~/gnusynthesis/vhd_front/vhd_front.cma ~/System-Verilog-decompiler/vhd_libs/
cp ~/gnusynthesis/ver_front/*.mli ~/System-Verilog-decompiler/vhd_libs/
cp ~/gnusynthesis/vhd_front/*.mli ~/System-Verilog-decompiler/vhd_libs/
cp ~/gnusynthesis/vhd_front/VhdlTypes.ml ~/System-Verilog-decompiler/vhd_libs/
cp ~/gnusynthesis/vhd_front/VhdlTree.ml ~/System-Verilog-decompiler/vhd_libs/
```

**Why .mli not .cmi?**
- `.mli` = Interface source files (text)
- `.cmi` = Compiled interface files (binary, OCaml version-specific)
- `.mli` files can be recompiled for any OCaml version
- `.cmi` files cause "not a compiled interface for this version" errors

## Compilation

### Manual Compilation

```bash
ocamlfind ocamlc \
  -package num,menhirLib \
  -I ~/gnusynthesis/ver_front \
  -I ~/gnusynthesis/vhd_front \
  -linkpkg \
  -o test_vhdl \
  ~/gnusynthesis/ver_front/ver_front.cma \
  ~/gnusynthesis/vhd_front/vhd_front.cma \
  vhdl_parse.ml test_vhdl_parse.ml
```

### Using Local Libraries

```bash
ocamlfind ocamlc \
  -package num,menhirLib \
  -I vhd_libs \
  -linkpkg \
  -o test_vhdl \
  vhd_libs/ver_front.cma \
  vhd_libs/vhd_front.cma \
  vhdl_parse.ml test_vhdl_parse.ml
```

## Usage

### Basic Parsing

```ocaml
(* Parse a VHDL file *)
match Vhdl_parse.parse_vhdl_file "module.vhd" with
| Some ast ->
    Printf.printf "Parsed %d design units\n" (List.length ast)
| None ->
    Printf.printf "Parse failed\n"
```

### Elaboration

```ocaml
(* Elaborate and extract architecture *)
let result = Vhdl_elaborate.analyze_file "module.vhd" in
(* Returns true if successful *)
```

## VHDL AST Structure

```
vhdl_design_file (list of design_unit)
  └─ vhdl_design_unit
      ├─ context_clause
      └─ library_unit
          ├─ PrimaryUnit (EntityDeclaration)
          │   ├─ entityname
          │   ├─ entityheader
          │   ├─ entitydeclarations
          │   └─ entitystatements
          └─ SecondaryUnit (ArchitectureBody)
              ├─ archname
              ├─ archentityname
              ├─ archdeclarations
              └─ archstatements (processes, assignments)
```

## Test Results

### Parser Test
```
./test_vhdl
═══════════════════════════════════════════════════════════════
  VHDL Parser Integration Test
═══════════════════════════════════════════════════════════════

Parsing VHDL file: slib_clock_div.vhd
✅ Successfully parsed (2 design units)

Parsing VHDL file: slib_input_filter.vhd
✅ Successfully parsed (2 design units)

Parsing VHDL file: slib_mv_filter.vhd
✅ Successfully parsed (2 design units)

Parsing VHDL file: uart_baudgen.vhd
✅ Successfully parsed (2 design units)

Results: 4/4 files parsed successfully
```

### Elaboration Test
```
./test_vhdl_elaborate
═══════════════════════════════════════════════════════════════
  VHDL Elaboration Test
═══════════════════════════════════════════════════════════════

Analyzing: slib_clock_div.vhd
✅ Parsed successfully (2 design units)
Entity: slib_clock_div
Architecture: rtl of entity slib_clock_div
  Statements: 2

Results: 4/4 files analyzed successfully
```

## Next Steps

### 1. Extract Process Statements

Need to walk the `archstatements` list and extract:
- Process sensitivity lists
- Sequential statements within processes
- Signal assignments
- Conditional logic (if/elsif/else)

### 2. Convert to IR

Map VHDL constructs to our IR format (sv_ast.ml):
- VHDL signals → IR registers
- VHDL processes → IR always blocks
- VHDL conditions → IR MUX trees
- VHDL expressions → IR operations

### 3. Z3 Comparison

Compare VHDL-derived IR against SystemVerilog-derived IR:
- Parse both VHDL and SV versions
- Convert both to IR
- Use Z3 to prove equivalence
- This gives us ground truth comparisons!

### 4. Expected Patterns

Based on VHDL source analysis (see VHDL_TO_SYSTEMVERILOG_ANALYSIS.md):

**Pattern A: Unconditional + Conditional**
- slib_clock_div (lines 54-57)
- uart_baudgen (lines 57-60)
- Default assignment followed by conditional override

**Pattern B: Mutually Exclusive**
- slib_input_filter (lines 60-64)
- elsif creates nested MUX

**Pattern C: Sequential Independent If**
- slib_mv_filter (lines 58-69)
- Two separate if blocks
- Later condition overrides earlier

## Benefits

### 1. Ground Truth Verification
- VHDL is the original source
- No reliance on buggy Verilator IR
- Direct semantic comparison

### 2. Pattern Validation
- Confirm our SystemVerilog decompiler is correct
- Validate MUX tree generation
- Verify statement ordering

### 3. Debugging Tool
- When SV decompiler fails, compare against VHDL
- Identify exactly where behavior differs
- Understand intended semantics

## References

- VHDL Sources: `~/gnusynthesis/vhd_front/*.vhd`
- VHDL Parser: `~/gnusynthesis/vhd_front/` (VhdlParser.mly)
- Verilog Parser: `~/gnusynthesis/ver_front/` (grammar.mly)
- SystemVerilog Tests: `/tmp/*.sv`
- Our Decompiler: `sv_elaborate.ml`, `sv_verible_to_ir.ml`

## Integration Status

✅ **Completed**:
- VHDL parser integrated
- Basic elaboration working
- Test programs created
- Libraries copied to project

🔄 **In Progress**:
- Process statement extraction
- VHDL→IR conversion
- Z3 comparison framework

⏳ **TODO**:
- Full VHDL→IR converter
- VHDL vs SV comparison test
- Integration with test_uart_modules_z3.exe
- Documentation of comparison results
