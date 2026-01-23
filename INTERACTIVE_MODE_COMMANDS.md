# Interactive Mode - Behavioral IR Commands

## Summary

Added five new commands to the unified interactive console (`sv_main_unified.exe interactive`) for testing the behavioral IR infrastructure and Yosys synthesis.

## New Commands

### synth-yosys
Synthesize SystemVerilog with Yosys and output RTLIL format.

**Usage:**
```
sv> synth-yosys <file.v> [output.il]
```

**Examples:**
```
# Output to file
sv> synth-yosys sysver_tests/slib_clock_div.sv /tmp/output.il

# Output to stdout
sv> synth-yosys sysver_tests/slib_clock_div.sv
```

**What it does:**
1. Reads SystemVerilog file with Yosys
2. Runs synthesis pass
3. Writes RTLIL/ILANG format to file or stdout

**Yosys Command:**
```bash
yosys -q -q -p 'read_verilog -sv <file>; synth; write_rtlil [output]'
```

**Use Cases:**
- Generate RTLIL for analysis
- Compare Yosys synthesis with Verilator
- Prepare input for RTLIL → IR conversion
- Debug synthesis issues

### test-verilator
Test Verilator JSON → Behavioral IR pipeline with full optimization.

**Usage:**
```
sv> test-verilator <verilator.json>
```

**Example:**
```
sv> test-verilator test/obj_dir/Vcounter.tree.json
```

**What it does:**
1. Converts Verilator JSON → Behavioral IR
2. Runs optimization pipeline (SSA, const prop, DCE, CSE)
3. Performs register inference
4. Shows module statistics and register counts

### test-equiv
Test VHDL vs SystemVerilog behavioral equivalence with structural comparison.

**Usage:**
```
sv> test-equiv <file.vhd> <file.sv>
```

**Example:**
```
sv> test-equiv sysver_tests/slib_clock_div.vhd sysver_tests/slib_clock_div.sv
```

**What it does:**
1. Converts VHDL → Behavioral IR
2. Converts SystemVerilog → Behavioral IR
3. Optimizes both modules
4. Performs register inference on both
5. Compares:
   - Register counts
   - Register names
   - Clock signals
   - Module structure

### test-miter
Formal Z3 miter verification proving VHDL ≡ SystemVerilog.

**Usage:**
```
sv> test-miter <file.vhd> <file.sv>
```

**Example:**
```
sv> test-miter sysver_tests/slib_clock_div.vhd sysver_tests/slib_clock_div.sv
```

**What it does:**
1. Converts both VHDL and SV → Behavioral IR
2. Optimizes both modules
3. Builds miter circuit (connects same inputs, XORs outputs)
4. Encodes as Z3 bitvector constraints
5. Checks SAT: ∃ inputs where outputs differ?
   - **UNSAT** → Designs are formally equivalent ✅
   - **SAT** → Counterexample found ❌

**Result:**
Formal proof that VHDL ≡ SV for **all possible inputs**.

### test-z3-simple
Structural Z3 verification (faster, less complete than miter).

**Usage:**
```
sv> test-z3-simple <file.vhd> <file.sv>
```

**Example:**
```
sv> test-z3-simple sysver_tests/slib_clock_div.vhd sysver_tests/slib_clock_div.sv
```

**What it does:**
1. Converts both VHDL and SV → Behavioral IR
2. Optimizes both modules
3. Compares structural properties:
   - Module names
   - Output signals
   - Register counts and names
   - Clock signals
4. Verifies structure matches without full SAT check

**Result:**
Quick structural equivalence check (not a formal proof).

## Starting Interactive Mode

```bash
$ dune exec ./sv_main_unified.exe interactive

╔════════════════════════════════════════════════════════════╗
║  SystemVerilog Decompiler - Interactive Console           ║
║  Type 'help' for commands, 'quit' to exit                 ║
╚════════════════════════════════════════════════════════════╝

sv> help
```

## Example Session

```
sv> synth-yosys sysver_tests/slib_clock_div.sv /tmp/clock_div.il
Synthesizing sysver_tests/slib_clock_div.sv with Yosys...
✓ Synthesis complete: /tmp/clock_div.il

sv> test-verilator test/obj_dir/Vcounter.tree.json
═══════════════════════════════════════════════════════════════
  Verilator JSON → Behavioral IR → Optimization
═══════════════════════════════════════════════════════════════

[1/3] Converting Verilator JSON to Behavioral IR...
✓ Conversion successful (1 modules)

Module: counter
  Signals: 4
  Processes: 1

[2/3] Running Optimization Pipeline...
✓ Optimization complete

[3/3] Register Inference...
  Registers: 1
  Wires: 2

✅ SUCCESS

sv> test-miter sysver_tests/slib_clock_div.vhd sysver_tests/slib_clock_div.sv
═══════════════════════════════════════════════════════════════
  Z3 Miter Circuit - Formal Equivalence Verification
═══════════════════════════════════════════════════════════════

[1/5] Converting VHDL to Behavioral IR...
✓ VHDL conversion successful

[2/5] Converting SystemVerilog to Behavioral IR...
✓ SV conversion successful

[3/5] Optimizing both designs...
✓ Optimization complete

[4/5] Building miter circuit...
✓ Miter circuit constructed

[5/5] Running Z3 solver...
✓ Z3 solve time: 0.012 seconds

═══════════════════════════════════════════════════════════════
  ✅ PROVEN EQUIVALENT
═══════════════════════════════════════════════════════════════

Z3 proved that no input exists where outputs differ.
The two designs are formally equivalent! ✅

sv> quit
Goodbye!
```

## Benefits

1. **Quick Access** - Test behavioral IR without leaving the REPL
2. **No Path Management** - Commands handle `dune exec` automatically
3. **Consistent Interface** - Same commands across all test executables
4. **Shell Integration** - Can mix with other interactive commands (load, ls, cd, etc.)

## Implementation

The commands are implemented in `sv_main_unified.ml` in the `Interactive` module:

```ocaml
| "test-verilator" :: json_file :: _ ->
    let cmd = Printf.sprintf "dune exec ./test_verilator_behavioral.exe %s" json_file in
    execute_shell_command cmd

| "test-miter" :: vhdl :: sv :: _ ->
    let cmd = Printf.sprintf "dune exec ./test_miter_equivalence.exe %s %s" vhdl sv in
    execute_shell_command cmd
```

Each command:
1. Validates arguments (shows usage if missing)
2. Builds `dune exec` command string
3. Executes via `execute_shell_command`
4. Returns exit code and output to user

## Status

✅ **All four commands working and tested**

The interactive mode now provides convenient access to the complete behavioral IR test suite.
