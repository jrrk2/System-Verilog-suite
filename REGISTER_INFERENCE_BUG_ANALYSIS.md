# Register Inference Bug Analysis

## Problem Statement

The Verilator JSON → Behavioral IR conversion is reporting unrealistically low register counts for many Ariane modules, indicating the optimizer or register inference is being over-aggressive.

## Evidence

### std_icache Module

**Source Code:**
```bash
$ grep -c "_q" ../ariane/src/cache_subsystem/std_icache.sv
27
```
The source has 27 `_q` signals (standard naming for registers in SystemVerilog).

**Our Conversion:**
```
Module: std_icache
  Registers: 0
  Wires: 0
```
Reports 0 registers and 0 wires!

### commit_stage Module

**Our Conversion:**
```
Module: commit_stage
  Registers: 2 (or 0 depending on where you look)
  Wires: 47
```

**Source Analysis:**
- The module is actually entirely combinational (`always_comb` only)
- No `always_ff` blocks
- This might actually be correct!

### Other Suspicious Modules

```
Module: std_nbdcache
  Registers: 0
  Wires: 0

Module: id_stage
  Registers: 0
  Wires: 0

Module: frontend
  Registers: 0
  Wires: 0
```

Many modules report 0/0 which is implausible.

## Root Cause Analysis

### Possible Issue 1: Submodule Flattening

Verilator may be flattening the design. Registers in submodules like `std_icache` might be:
- Moved to parent modules during elaboration
- Merged/optimized away
- Not extracted into separate Behavioral IR modules

Evidence: The Verilator JSON shows 67 modules extracted from ~180 source modules.

### Possible Issue 2: Over-Aggressive DCE

The dead code elimination removed **55% of statements** (2821 → 1275 in main module).

This could be removing:
- Registers that appear unused due to missing connectivity
- Signals that are outputs but not yet connected
- State that's used but not properly tracked

### Possible Issue 3: Register Inference Algorithm

The current algorithm:
1. Looks for clock edges in processes
2. Groups assignments by original signal name (stripping SSA suffixes)
3. Filters out CSE temps

Potential bugs:
- **May not handle Verilator's JSON structure correctly**
- Verilator JSON might represent registers differently than our parser expects
- Clock signal detection might be failing

### Possible Issue 4: Missing Process Blocks

Some modules show:
```
Module: std_icache
  Signals: X
  Processes: 0
```

If processes aren't being extracted, registers won't be found.

## Comparison: Individual vs Full Processor

### Individual Module Test (Earlier)
```
Module: counter
  Registers: 1
  Wires: 13
```
✅ Correct for a simple counter

### Full Processor Test (This Issue)
```
Module: std_icache
  Registers: 0
  Wires: 0
```
❌ Clearly wrong

**Hypothesis:** When processing the full Ariane design with all dependencies, something goes wrong with:
- Module extraction
- Register tracking
- Process/signal association

## What Should We See?

### Realistic Register Counts

For a RISC-V processor like Ariane, we'd expect:

| Module | Expected Registers |
|--------|-------------------|
| **scoreboard** | 50-100 (instruction queue entries) |
| **std_icache** | 20-50 (cache state, tags, valid bits) |
| **std_dcache** | 50-100 (cache state, miss buffer) |
| **load_unit** | 10-30 (load queue, address translation) |
| **store_unit** | 10-30 (store queue, pending writes) |
| **frontend** | 20-50 (PC, fetch buffer, prediction) |
| **controller** | 10-30 (privilege mode, exceptions) |
| **csr_regfile** | 30-50 (all the CSR registers) |

**Total for full processor:** 500-2000+ registers is realistic

**What we're reporting:** ~50 registers total ❌

## Debugging Steps Needed

### 1. Check Verilator JSON Structure
```bash
# Look at how registers are represented in JSON
jq '.modules[] | select(.name == "std_icache") |
    {name, vars: [.vars[]? | select(.varType == "LOGIC" and .direction == "VAR") | .name]}' \
    obj_dir/Variane.tree.json
```

### 2. Check Process Extraction
Look at how `Verilator_to_behavioral.convert_verilator_json_to_behavioral` extracts processes and registers.

### 3. Test Individual Module JSON
Generate Verilator JSON for just `std_icache.sv` with dependencies and see if registers are found correctly.

### 4. Disable Optimizations
Run without DCE/CSE to see if optimizer is removing registers:
```ocaml
let (optimized, _) = optimize_custom
  { default_config with
    enable_dce = false;
    enable_cse = false;
    verbose = true
  } bprog
```

### 5. Check Register Inference Logic
Review `Behavioral_registers.analyze_module` to ensure it correctly:
- Identifies always_ff patterns in Verilator JSON
- Handles clock signals
- Groups SSA versions correctly
- Doesn't filter out real registers

## Likely Culprits

### Most Likely: Module Boundary Issue

Verilator may be:
- Inlining submodules during elaboration
- Moving registers to parent modules
- Optimizing away module boundaries

When we extract "std_icache" as a module, it might be empty because Verilator has:
- Moved its registers to `std_cache_subsystem`
- Inlined it completely
- Replaced it with wires to parent module's registers

### Second Most Likely: Process Extraction Bug

The `Verilator_to_behavioral` conversion might not be correctly:
- Extracting always_ff blocks from JSON
- Associating signals with processes
- Identifying sequential vs combinational logic

## Recommended Fix

### Option 1: Fix Module Extraction
Ensure that when Verilator inlines/flattens modules, we still attribute registers to their logical source module.

### Option 2: Report Parent Module Registers
If Verilator moves registers to parent, track which module they "logically" belong to.

### Option 3: Use Module Hierarchy Flag
Generate Verilator JSON with `--hierarchical` or similar flag to preserve module boundaries.

### Option 4: Accept Flattening
Acknowledge that Verilator flattens and report all registers in parent modules only.

## Impact

This bug means:
- ❌ Register counts are meaningless for complex designs
- ❌ Can't trust module-level analysis
- ❌ Register inference appears broken
- ⚠️  Optimization might be removing real hardware
- ✅ Top-level processor still converts (but with wrong granularity)

## Next Steps

1. **Investigate Verilator JSON structure** for std_icache
2. **Test individual module conversion** vs full processor
3. **Disable optimizations** and see if registers reappear
4. **Review Verilator_to_behavioral.ml** for process extraction bugs
5. **Consider using Verilator's tree-JSON format** differently

The user is absolutely correct - these register counts are implausibly low and indicate a real bug in either:
- How we're extracting information from Verilator JSON
- How the optimizer is treating registers
- How register inference is classifying signals

This needs investigation before we can claim the conversion is working correctly.
