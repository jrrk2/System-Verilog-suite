# Yosys-Compatible Decompiler Changes

## Overview

Modified `sv_gen.ml` to create `sv_gen_yosys.ml` with three key improvements for Yosys synthesis:

1. **Interface flattening** - Breaks apart SystemVerilog interfaces into individual ports
2. **Simulation construct filtering** - Removes non-synthesizable constructs with warnings
3. **Module hierarchy preservation** - Maintains module boundaries for bottom-up synthesis

## Key Changes

### 1. Interface Port Flattening

**Problem:** Yosys doesn't support SystemVerilog interfaces
**Solution:** Automatically flatten interface ports into individual signals

```systemverilog
// Before (with interface)
module slave (
  simple_bus_if.slave sif
);

// After (flattened)
module slave (
  input logic clk,
  input logic [7:0] addr,
  input logic [31:0] data,
  input logic we
);
```

The flattening happens in two places:
- **Module ports**: Interfaces are expanded to individual port declarations
- **Top-level signals**: Interface instances become individual wire declarations
- **Connections**: Interface.signal becomes flattened_signal

### 2. Non-Synthesizable Construct Filtering

**New function:** `is_synthesizable` - Returns false for simulation-only constructs

Filtered constructs with warnings:
- `$display`, `$monitor`, `$write` - Display statements
- `initial` blocks (except for memory initialization in some cases)
- `final` blocks
- `$finish`, `$stop`, `$fatal` - Simulation control
- `#delay` - Timing delays
- `@ event` - Event controls (outside always blocks)
- `$random`, `$urandom` - Random number generation
- `$isunknown` - Unknown value checking (warning only)

**Example:**
```systemverilog
// Input from Verilator
always_comb begin
  data = mem[addr];
  $display("Reading %h", data);  // Removed with warning
end

// Output for Yosys
always_comb begin
  data = mem[addr];
end
```

### 3. Warning System

**New features:**
- Accumulates warnings during generation
- Can output to file or stderr
- Categorizes issues by severity

**API:**
```ocaml
(* Generate with warnings collected *)
let (result, warnings) = Sv_gen_yosys.generate_sv_with_warnings ast 0 in

(* Check warnings *)
List.iter (fun w -> Printf.printf "WARNING: %s\n") warnings
```

### 4. Array Declaration Fix

**Fixed:** The declRange parsing for array types

```systemverilog
// Before (incorrect)
logic byte_enables [4'h3];  // Used constant value incorrectly

// After (correct)
logic byte_enables [0:3];   // Proper range syntax
```

### 5. Redundant Selection Removal

**Optimization:** Removes meaningless `[0]` selections

```systemverilog
// Before
assign out = data[0];  // When data is already 1-bit

// After  
assign out = data;     // Simplified
```

## Usage

### Batch Processing (scan directory)
```bash
./decompiler scan output_dir/
# Generates: output_dir/synthesis_warnings.txt
```

### Single File Processing
```bash
./decompiler file input.json output.sv
# Warnings printed to stderr
```

### Integration with Build Flow

```makefile
# Step 1: Run Verilator to get JSON
verilator --dump-tree-json -Wall design.sv

# Step 2: Decompile with Yosys compatibility
./decompiler scan yosys_input/

# Step 3: Check warnings
cat yosys_input/synthesis_warnings.txt

# Step 4: Run Yosys synthesis
yosys -p "read_verilog yosys_input/*.sv; synth_ice40 -top top"
```

## Module Hierarchy Preservation

**Critical for bottom-up synthesis:**

The decompiler maintains module boundaries:
- Each Verilator module becomes a separate `.sv` file
- Module instantiations preserved with flattened ports
- Parameters passed through correctly
- Allows Yosys to synthesize modules independently

**Example hierarchy:**
```
top.sv
├── cpu.sv (synthesized separately)
├── memory.sv (synthesized separately)
└── peripherals.sv (synthesized separately)
```

## Warning Categories

### Critical (Must Fix)
- Unsynthesizable constructs in synthesizable blocks
- Interface flattening failures
- Missing module definitions

### Informational
- Interface flattening performed
- Simulation blocks removed
- System functions used

### Style
- Redundant selections removed
- Generate block simplifications

## Limitations & Known Issues

1. **Memory initialization**: `initial` blocks for memory may need manual review
2. **X-propagation**: Yosys handles X differently than simulation
3. **Timing**: All timing information is removed
4. **Assertions**: SVA assertions are removed (could be made optional)
5. **Parameterization**: Complex parameter expressions may need simplification

## Future Enhancements

### Potential additions:
1. **Assert preservation**: Option to keep synthesis-compatible assertions
2. **FSM extraction**: Identify and optimize state machines
3. **Memory inference**: Better RAM/ROM pattern matching
4. **Clock domain handling**: Explicit CDC markers
5. **Reset handling**: Async vs sync reset standardization

## Testing Recommendations

1. **Compare module lists**: Ensure all modules preserved
```bash
# Count modules in input
grep "^module" design.sv | wc -l
# Count modules in output  
grep "^module" yosys_input/*.sv | wc -l
```

2. **Check port counts**: Verify interface flattening
```bash
# Check a specific module's ports
grep "module slave" -A 20 yosys_input/decompile_*.sv
```

3. **Warning review**: Read all warnings before synthesis
```bash
cat yosys_input/synthesis_warnings.txt | grep "CRITICAL"
```

4. **Synthesis test**: Run Yosys on each module
```bash
for f in yosys_input/*.sv; do
  yosys -p "read_verilog $f; hierarchy -check" || echo "FAIL: $f"
done
```

## Example Warning Output

```
=== simple_bus_top.json ===
  WARNING: Flattening interface 'simple_bus' to individual ports for Yosys compatibility
  WARNING: Removing initial block (simulation-only)
  WARNING: Removing $display statement (simulation-only)

=== cpu_core.json ===
  WARNING: Using $isunknown (may not synthesize in all tools)
  WARNING: Removing $finish statement (simulation-only)
```

## Differences from sv_gen.ml

**Preserved:**
- Module hierarchy
- Port ordering
- Parameter handling
- Generate block structure

**Changed:**
- Interface handling (flattened vs preserved)
- Simulation construct handling (filtered vs passed through)
- Warning generation (added)
- Selection simplification (enhanced)

**File comparison:**
```bash
# See what changed
diff -u sv_gen.ml sv_gen_yosys.ml
```
