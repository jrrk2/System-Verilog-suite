# Type-Safe File Registry

## Overview

The interactive mode now includes a **type-safe file registry** that tracks file types and automatically performs conversions when needed. This eliminates manual file management and ensures type safety throughout the HDL workflow.

## Concept

Instead of manually tracking intermediate files and their formats, the registry:

1. **Infers file types** from extensions and content
2. **Tracks transformations** available for each type
3. **Auto-converts** files when commands need different formats
4. **Validates operations** before execution

## File Types

The registry recognizes these file types:

| Type | Extensions | Description |
|------|-----------|-------------|
| **SystemVerilog** | `.sv`, `.v` | SystemVerilog source files |
| **VHDL** | `.vhd`, `.vhdl` | VHDL source files |
| **Verilator JSON** | `.json` | Verilator JSON AST output |
| **Yosys RTLIL** | `.il`, `.rtlil` | Yosys RTLIL/ILANG format |
| **Behavioral IR** | (future) | Language-neutral behavioral IR |
| **Optimized IR** | (future) | Optimized behavioral IR |

## Valid Transformations

Each file type has specific transformations available:

### SystemVerilog → ...
- `verilator-json` → Verilator JSON AST
- `synth-yosys` → Yosys RTLIL
- `to-behavioral-ir` → Behavioral IR (planned)

### VHDL → ...
- `to-behavioral-ir` → Behavioral IR (planned)

### Verilator JSON → ...
- `to-behavioral-ir` → Behavioral IR (via test-verilator)

### Yosys RTLIL → ...
- `to-opt-ir` → Optimized IR (planned)

### Behavioral IR → ...
- `optimize` → Optimized IR (planned)
- `to-z3` → Z3 constraints (planned)

## Commands

### register <file>
Register a file with automatic type inference.

**Usage:**
```bash
sv> register sysver_tests/slib_clock_div.sv
✓ Registered: sysver_tests/slib_clock_div.sv [SystemVerilog]
```

### list-files
List all registered files with their types.

**Usage:**
```bash
sv> list-files

Registered Files:
  • sysver_tests/slib_clock_div.sv [SystemVerilog]
  • /tmp/sv_interactive_1.json [Verilator JSON]
  • /tmp/sv_interactive_2.il [Yosys RTLIL]
```

### file-info <file>
Show file type and available transformations.

**Usage:**
```bash
sv> file-info sysver_tests/slib_clock_div.sv
File: sysver_tests/slib_clock_div.sv [SystemVerilog]
  Valid transformations:
    • verilator-json
    • synth-yosys
    • to-behavioral-ir
```

### convert <file> <type>
Explicitly convert a file to target type.

**Usage:**
```bash
sv> convert sysver_tests/slib_clock_div.sv json
📋 Auto-converting sysver_tests/slib_clock_div.sv from SystemVerilog to Verilator JSON
→ Converting sysver_tests/slib_clock_div.sv to Verilator JSON...
✓ Generated: /tmp/sv_interactive_1.json
✓ Registered: /tmp/sv_interactive_1.json [Verilator JSON]
```

**Valid target types:**
- `json` or `verilator-json` → Verilator JSON
- `il` or `yosys-rtlil` → Yosys RTLIL
- `behavioral-ir` or `bir` → Behavioral IR (planned)
- `optimized-ir` or `oir` → Optimized IR (planned)

## Auto-Conversion

Commands now support **automatic conversion**. When you pass a file of the wrong type, the system converts it automatically.

### Example: test-verilator

**Before (manual):**
```bash
sv> verilator-json sysver_tests/slib_clock_div.sv /tmp/clock.json
sv> test-verilator /tmp/clock.json
```

**Now (automatic):**
```bash
sv> test-verilator sysver_tests/slib_clock_div.sv
📋 Auto-converting sysver_tests/slib_clock_div.sv from SystemVerilog to Verilator JSON
→ Converting sysver_tests/slib_clock_div.sv to Verilator JSON...
✓ Generated: /tmp/sv_interactive_1.json
✓ Registered: /tmp/sv_interactive_1.json [Verilator JSON]
Running: dune exec ./test_verilator_behavioral.exe /tmp/sv_interactive_1.json
[output from test...]
```

The system:
1. Detects input is SystemVerilog
2. Knows test-verilator needs Verilator JSON
3. Automatically runs verilator-json conversion
4. Registers the generated file
5. Executes the command with correct input

## Type Safety

The registry prevents invalid operations:

```bash
sv> file-info /tmp/output.json
File: /tmp/output.json [Verilator JSON]
  Valid transformations:
    • to-behavioral-ir

sv> convert /tmp/output.json il
✗ Cannot convert Verilator JSON to Yosys RTLIL
  Valid transformations: to-behavioral-ir
```

## Complete Workflow Example

```bash
$ dune exec ./sv_main_unified.exe interactive

sv> # Register source files
sv> register sysver_tests/slib_clock_div.vhd
✓ Registered: sysver_tests/slib_clock_div.vhd [VHDL]

sv> register sysver_tests/slib_clock_div.sv
✓ Registered: sysver_tests/slib_clock_div.sv [SystemVerilog]

sv> # Show what's registered
sv> list-files

Registered Files:
  • sysver_tests/slib_clock_div.vhd [VHDL]
  • sysver_tests/slib_clock_div.sv [SystemVerilog]

sv> # Check available transformations
sv> file-info sysver_tests/slib_clock_div.sv
File: sysver_tests/slib_clock_div.sv [SystemVerilog]
  Valid transformations:
    • verilator-json
    • synth-yosys
    • to-behavioral-ir

sv> # Test Verilator pipeline (auto-converts .sv → .json)
sv> test-verilator sysver_tests/slib_clock_div.sv
📋 Auto-converting sysver_tests/slib_clock_div.sv from SystemVerilog to Verilator JSON
→ Converting sysver_tests/slib_clock_div.sv to Verilator JSON...
✓ Generated: /tmp/sv_interactive_1.json
✓ Registered: /tmp/sv_interactive_1.json [Verilator JSON]
Running: dune exec ./test_verilator_behavioral.exe /tmp/sv_interactive_1.json

═══════════════════════════════════════════════════════════════
  Verilator JSON → Behavioral IR → Optimization
═══════════════════════════════════════════════════════════════
[... test output ...]
✅ SUCCESS

sv> # Explicitly convert to RTLIL
sv> convert sysver_tests/slib_clock_div.sv il
📋 Auto-converting sysver_tests/slib_clock_div.sv from SystemVerilog to Yosys RTLIL
→ Synthesizing sysver_tests/slib_clock_div.sv with Yosys...
✓ Generated: /tmp/sv_interactive_2.il
✓ Registered: /tmp/sv_interactive_2.il [Yosys RTLIL]

sv> # View all generated files
sv> list-files

Registered Files:
  • /tmp/sv_interactive_2.il [Yosys RTLIL]
  • /tmp/sv_interactive_1.json [Verilator JSON]
  • sysver_tests/slib_clock_div.vhd [VHDL]
  • sysver_tests/slib_clock_div.sv [SystemVerilog]

sv> quit
Goodbye!
```

## Implementation Details

### File Registry State

```ocaml
type file_type =
  | SystemVerilog
  | VHDL
  | VerilatorJSON
  | YosysRTLIL
  | BehavioralIR
  | OptimizedIR
  | Unknown

type file_handle = {
  path: string;
  file_type: file_type;
  metadata: (string * string) list;
}

type registry = {
  mutable files: (string, file_handle) Hashtbl.t;
  mutable temp_counter: int;
}
```

### Type Inference

```ocaml
let infer_file_type path =
  if Filename.check_suffix path ".sv" || Filename.check_suffix path ".v" then
    SystemVerilog
  else if Filename.check_suffix path ".vhd" || Filename.check_suffix path ".vhdl" then
    VHDL
  else if Filename.check_suffix path ".json" then
    VerilatorJSON
  else if Filename.check_suffix path ".il" || Filename.check_suffix path ".rtlil" then
    YosysRTLIL
  else
    Unknown
```

### Transformation Graph

```ocaml
let can_transform file_type target_type =
  match file_type, target_type with
  | SystemVerilog, VerilatorJSON -> true
  | SystemVerilog, YosysRTLIL -> true
  | SystemVerilog, BehavioralIR -> true
  | VHDL, BehavioralIR -> true
  | VerilatorJSON, BehavioralIR -> true
  | YosysRTLIL, OptimizedIR -> true
  | BehavioralIR, OptimizedIR -> true
  | _, _ -> false
```

### Auto-Conversion

```ocaml
let auto_convert registry path target_type =
  match lookup_file registry path with
  | Some handle when handle.file_type = target_type ->
      Some path  (* Already correct type *)
  | Some handle when can_transform handle.file_type target_type ->
      (* Auto-convert and register result *)
      let output = gen_temp_path registry ".ext" in
      execute_transformation registry handle.file_type target_type path output;
      register_file registry output;
      Some output
  | _ ->
      None  (* Cannot convert *)
```

## Benefits

### ✅ Type Safety
Prevents running incompatible operations:
- Can't test Verilator pipeline on VHDL without conversion
- Can't synthesize with Yosys on JSON AST
- Clear error messages when operations are invalid

### ✅ Automatic Conversion
No manual intermediate file management:
- `test-verilator counter.sv` just works
- System handles `.sv → .json` conversion transparently
- Generated files are tracked and reusable

### ✅ File Tracking
Always know what files exist:
- `list-files` shows all registered files
- `file-info` shows type and valid operations
- Temporary files are tracked and named consistently

### ✅ Transformation Discovery
Easy to find valid operations:
- `file-info` lists available transformations
- Clear error messages suggest alternatives
- Prevents trial-and-error workflow

### ✅ Workflow Simplification
Focus on analysis, not file management:
- Register source files once
- Commands handle conversions
- Temporary files auto-generated and tracked
- Clean workflow without manual cleanup

## Future Enhancements

### Planned Features
1. **Behavioral IR serialization** - Save/load optimized IR
2. **Dependency tracking** - Track which files depend on others
3. **Caching** - Reuse conversions if source unchanged
4. **Multi-step conversion** - Chain transformations (e.g., .sv → .json → .bir → .oir)
5. **Garbage collection** - Clean up unused temporary files
6. **Export/import** - Save registry state between sessions

### Additional Transformations
- `BehavioralIR → HardCaml` - Generate HardCaml circuits
- `BehavioralIR → SystemVerilog` - Round-trip testing
- `OptimizedIR → LLVM` - LLVM IR backend
- `VHDL + SystemVerilog → Z3` - Cross-language verification

## Status

✅ **Core functionality complete and tested**

The type-safe file registry provides:
- Automatic type inference
- Transformation validation
- Auto-conversion for test-verilator
- File tracking and listing
- Clear error messages

Future work will extend to more transformations and advanced features.
