# HDL Development & Verification Environment (interactive_client_v2)

## Overview

An embedded Lua-ML 2.5 interactive client for HDL verification and synthesis, following the **building blocks philosophy** - providing fundamental tools that can be composed into any workflow rather than pre-packaged specific solutions.

## Philosophy: Building Blocks Over Recipes

This client provides **modular building blocks** organized into logical namespaces:

```lua
convert.*   -- IR conversions between formats
optimize.*  -- Optimization operations
dump.*      -- Statistics and serialization
liberty.*   -- Technology library operations
verify.*    -- Verification methods
```

**Advantages of this approach:**
- **Composability**: Combine building blocks in custom scripts
- **Flexibility**: Any workflow imaginable, not limited by pre-defined commands
- **Memorability**: Small set of fundamental operations
- **Performance**: Script complex workflows with negligible overhead

## Installation

Build with dune:
```bash
dune build interactive_client_v2.exe
```

Run:
```bash
./_build/default/interactive_client_v2.exe
```

## Interactive Features

The client includes **linenoise** for enhanced line editing and command history:

- **Command history**: Use Up/Down arrows to navigate previous commands
- **Line editing**: Left/Right arrows, Home/End, Ctrl-A/E for cursor movement
- **Persistent history**: Commands saved to `~/.hdl_history` (1000 commands)
- **Multi-line support**: Edit longer Lua expressions with line wrapping
- **Screen control**: Ctrl-L to clear screen
- **Editing shortcuts**: Ctrl-K (kill to end), Ctrl-U (kill to start), Ctrl-W (delete word)

See [LINENOISE_FEATURES.md](LINENOISE_FEATURES.md) for complete documentation.

## Module Reference

### convert.* - IR Conversions

Convert between hardware description languages and intermediate representations.

```lua
convert.vhdl_to_behavioral(file)        -- VHDL → Behavioral IR
convert.sv_to_behavioral(file)          -- SystemVerilog → Behavioral IR
convert.verilator_to_behavioral(json)   -- Verilator JSON → Behavioral IR
```

**Examples:**
```lua
-- Convert VHDL to IR
convert.vhdl_to_behavioral("design.vhd")

-- Convert SystemVerilog to IR
convert.sv_to_behavioral("design.sv")

-- Convert Verilator output to IR
convert.verilator_to_behavioral("obj_dir/Vdesign.json")
```

### optimize.* - Optimization Operations

Apply optimization passes to designs.

```lua
optimize.quick(file)     -- Fast optimization (const prop + DCE)
optimize.full(file)      -- Comprehensive optimization (const prop + DCE + CSE)
```

**Examples:**
```lua
-- Quick optimization for rapid iteration
optimize.quick("design.vhd")

-- Full optimization for production
optimize.full("design.sv")
```

**What each does:**
- `quick`: Constant propagation + Dead code elimination
- `full`: Constant propagation + Dead code elimination + Common subexpression elimination

### dump.* - Statistics & Serialization

Extract information and serialize designs.

```lua
dump.stats(file)              -- Display IR statistics
dump.json(file, output)       -- Export design to JSON
```

**Examples:**
```lua
-- Analyze design statistics
dump.stats("design.vhd")

-- Export to JSON for external tools
dump.json("design.vhd", "output.json")
```

**Statistics shown:**
- Input/output/internal signals
- Number of processes and statements
- Register inference results
- Comparison with old VHDL approach (register count bug)

### liberty.* - Technology Libraries

Work with standard cell libraries.

```lua
liberty.load(file)            -- Load Liberty .lib file
```

**Examples:**
```lua
-- Load a technology library
lib = liberty.load("sky130_fd_sc_hd.lib")
```

**Future commands (not yet implemented):**
- `liberty.print_summary(lib)` - Display library information
- `liberty.get_cell(lib, name)` - Get cell information
- `liberty.is_flip_flop(lib, name)` - Check if cell is a flip-flop

### verify.* - Verification Methods

Formal verification using multiple methods.

```lua
verify.vhdl_regression(file)            -- Test VHDL frontend
verify.sv_regression(file)              -- Test SV frontend
verify.structural_equiv(vhdl, sv)       -- Compare IR structures
verify.sat_miter(vhdl, sv)              -- Z3 SAT proving
verify.hardcaml_equiv(vhdl, sv)         -- HardCaml interface validation
verify.hardcaml_sat(vhdl, sv)           -- HardCaml SAT (most reliable)
verify.verify_all(vhdl, sv)             -- Run all methods
```

**Examples:**
```lua
-- Single verification method (fastest, most reliable)
verify.hardcaml_sat("design.vhd", "design.sv")

-- Run all verification methods for comparison
verify.verify_all("design.vhd", "design.sv")

-- Frontend testing
verify.vhdl_regression("design.vhd")
verify.sv_regression("design.sv")
```

**Verification method comparison:**
- `hardcaml_sat`: **Most reliable** (11/11 success rate)
- `sat_miter`: Direct Z3 (1/11 proven, 2/11 false positives)
- `hardcaml_equiv`: Interface validation only
- `structural_equiv`: IR structure comparison

## Usage Modes

### Interactive Mode

Start the REPL:
```bash
./_build/default/interactive_client_v2.exe
```

```lua
hdl> help()
hdl> dump.stats("sysver_tests/slib_clock_div.vhd")
hdl> optimize.quick("sysver_tests/slib_clock_div.vhd")
hdl> verify.hardcaml_sat("sysver_tests/slib_clock_div.vhd",
                         "sysver_tests/slib_clock_div.sv")
```

### Batch Mode with Script

Execute a Lua script:
```bash
cat script.lua | ./_build/default/interactive_client_v2.exe
```

Or make the script executable:
```bash
chmod +x script.lua
./script.lua
```

Script shebang line:
```lua
#!/usr/bin/env -S ./_build/default/interactive_client_v2.exe
```

### One-liner Commands

```bash
echo 'dump.stats("design.vhd")' | ./_build/default/interactive_client_v2.exe
echo 'verify.hardcaml_sat("a.vhd", "a.sv")' | ./_build/default/interactive_client_v2.exe
```

## Full Lua 2.5 Syntax Support

The client supports complete Lua 2.5 syntax for scripting workflows.

### Variables
```lua
vhdl = "sysver_tests/uart_baudgen.vhd"
sv = "sysver_tests/uart_baudgen.sv"
```

### Tables
```lua
modules = {
    "slib_clock_div",
    "slib_input_sync",
    "slib_edge_detect"
}
```

### Loops
```lua
i = 1
while modules[i] do
    dump.stats("sysver_tests/" .. modules[i] .. ".vhd")
    i = i + 1
end
```

### Functions
```lua
function verify_module(name)
    vhdl = "sysver_tests/" .. name .. ".vhd"
    sv = "sysver_tests/" .. name .. ".sv"
    return verify.hardcaml_sat(vhdl, sv)
end
```

### Conditionals
```lua
if verify_module("slib_clock_div") then
    print("✓ PASSED")
else
    print("✗ FAILED")
end
```

### String Concatenation
```lua
path = "sysver_tests/" .. module_name .. ".vhd"
```

## Example Workflows

### Workflow 1: Analyze Multiple Designs

```lua
modules = {"uart_baudgen", "slib_clock_div", "slib_input_sync"}

i = 1
while modules[i] do
    print("Analyzing: " .. modules[i])
    dump.stats("sysver_tests/" .. modules[i] .. ".vhd")
    i = i + 1
end
```

### Workflow 2: Optimize and Verify

```lua
function full_flow(module_name)
    vhdl = "sysver_tests/" .. module_name .. ".vhd"
    sv = "sysver_tests/" .. module_name .. ".sv"

    -- Step 1: Analyze
    print("Step 1: Analyze")
    dump.stats(vhdl)

    -- Step 2: Optimize
    print("Step 2: Optimize")
    optimize.full(vhdl)

    -- Step 3: Verify
    print("Step 3: Verify")
    return verify.hardcaml_sat(vhdl, sv)
end

full_flow("slib_clock_div")
```

### Workflow 3: Batch Verification with Reporting

```lua
modules = {"slib_clock_div", "slib_input_sync", "slib_edge_detect"}

passed = 0
failed = 0

i = 1
while modules[i] do
    m = modules[i]
    vhdl = "sysver_tests/" .. m .. ".vhd"
    sv = "sysver_tests/" .. m .. ".sv"

    print("Testing: " .. m)

    if verify.hardcaml_sat(vhdl, sv) then
        print("✓ PASSED")
        passed = passed + 1
    else
        print("✗ FAILED")
        failed = failed + 1
    end

    i = i + 1
end

print("Results: " .. passed .. " passed, " .. failed .. " failed")
```

### Workflow 4: Technology Library Analysis

```lua
-- Load library
lib = liberty.load("sky130_fd_sc_hd.lib")

-- Convert design to IR
convert.vhdl_to_behavioral("design.vhd")

-- Optimize
optimize.full("design.vhd")

-- Export for gate mapping
dump.json("design.vhd", "design_optimized.json")
```

### Workflow 5: Custom Verification Strategy

```lua
function smart_verify(vhdl, sv)
    -- Try fastest method first
    print("Trying HardCaml SAT (most reliable)...")
    if verify.hardcaml_sat(vhdl, sv) then
        print("✓ Verified with HardCaml SAT")
        return true
    end

    -- Fallback to other methods
    print("Trying Z3 SAT miter...")
    if verify.sat_miter(vhdl, sv) then
        print("✓ Verified with Z3 SAT")
        return true
    end

    -- Last resort: structural comparison
    print("Trying structural equivalence...")
    if verify.structural_equiv(vhdl, sv) then
        print("✓ Structurally equivalent")
        return true
    end

    print("✗ All verification methods failed")
    return false
end

smart_verify("design.vhd", "design.sv")
```

## Building Blocks Philosophy

### Why Building Blocks?

**Traditional approach (recipes):**
```lua
-- Pre-packaged workflow
workflow.full_verification("design.vhd", "design.sv", {
    steps = {"analyze", "optimize", "verify"},
    method = "hardcaml_sat"
})
```

**Building blocks approach (this client):**
```lua
-- Compose your own workflow
dump.stats("design.vhd")
optimize.full("design.vhd")
verify.hardcaml_sat("design.vhd", "design.sv")
```

**Advantages:**
1. **Flexibility**: Any workflow imaginable, not limited by pre-defined commands
2. **Clarity**: Each step is explicit and understandable
3. **Debugging**: Easy to test each step independently
4. **Memorability**: Small set of fundamental operations vs many specific commands
5. **Composability**: Combine in Lua scripts for complex workflows

### Building Blocks vs Specific Cases

**Specific case (not provided):**
```lua
synthesis.map_to_liberty("design.vhd", "sky130.lib", "output.v")
```

**Building blocks (equivalent):**
```lua
lib = liberty.load("sky130.lib")
convert.vhdl_to_behavioral("design.vhd")
optimize.full("design.vhd")
-- Future: gatemap.map_to_liberty(ir, lib)
-- Future: gatemap.write_netlist(netlist, "output.v")
```

The building blocks approach requires more lines but provides:
- **Visibility**: See each step
- **Control**: Modify any step (e.g., use `optimize.quick` instead)
- **Reusability**: Save intermediate results for inspection
- **Flexibility**: Insert custom steps (e.g., dump stats between stages)

## Technical Implementation

### lua-ml Integration Pattern

Following the proven pattern from `../hardcaml-lua/myluaclient.ml`:

```ocaml
module T = Lua.Lib.Combine.T2
    (LuaChar)      -- Custom char type
    (Luaiolib.T)   -- I/O library type

module MakeHDLLib
    (CharV: Lua.Lib.TYPEVIEW with type 'a t = 'a LuaChar.t)
    : Lua.Lib.USERCODE with type 'a userdata' = 'a CharV.combined = struct

    module M (C: Lua.Lib.CORE with type 'a V.userdata' = 'a userdata') = struct
        let ( **-> ) = V.( **-> )
        let ( **->> ) x y = x **-> V.result y

        let init g =
            (* Register each module *)
            let g = C.register_module "convert" [...] g in
            let g = C.register_module "optimize" [...] g in
            let g = C.register_module "dump" [...] g in
            let g = C.register_module "liberty" [...] g in
            let g = C.register_module "verify" [...] g in
            g
    end
end
```

### Function Registration

```ocaml
C.register_module "dump" [
    "stats", V.efunc (V.string **->> V.bool) (wrap1 dump_stats);
    "json", V.efunc (V.string **-> V.string **->> V.bool) (wrap2 dump_json);
] g
```

Operators:
- `**->` : Function argument
- `**->>` : Last argument + return type

### Error Handling

```ocaml
let wrap1 f a =
    try f a
    with e ->
        Printexc.print_backtrace stdout;
        C.error (Printexc.to_string e)
```

Provides:
- Full OCaml stack traces
- Lua error integration
- Graceful REPL recovery

## Comparison with interactive_client.ml (v1)

| Feature | v1 (interactive_client) | v2 (interactive_client_v2) |
|---------|------------------------|----------------------------|
| Modules | 1 (`verify.*`) | 5 (`convert.*`, `optimize.*`, `dump.*`, `liberty.*`, `verify.*`) |
| Commands | 8 | 14+ |
| Philosophy | Verification-focused | Building blocks |
| IR Conversions | No | Yes (3 converters) |
| Optimization | No | Yes (2 levels) |
| Statistics | No | Yes (detailed) |
| JSON Export | No | Yes |
| Liberty Support | No | Yes (load) |

**When to use v1:**
- Quick verification tasks only
- Focus on `verify.*` commands

**When to use v2:**
- Custom workflows
- IR analysis and optimization
- Technology library integration
- Building complex verification scripts

## Future Enhancements

### Phase 1: Core Utilities (Implemented)
- ✅ IR conversions (convert.*)
- ✅ Optimization operations (optimize.*)
- ✅ Statistics and JSON (dump.*)
- ✅ Liberty loading (liberty.load)

### Phase 2: Synthesis Path (Planned)
- `gatemap.*` module for gate mapping
  - `gatemap.map_to_liberty(ir, lib)`
  - `gatemap.netlist_to_verilog(netlist)`
  - `gatemap.write_netlist(netlist, file)`

### Phase 3: Advanced Features (Planned)
- Individual optimization passes
  - `optimize.const_propagation(ir)`
  - `optimize.dead_code_elimination(ir)`
  - `optimize.common_subexpr_elim(ir)`
  - `optimize.ssa_conversion(ir)`
  - `optimize.register_inference(ir)`
- HardCaml circuit generation
  - `convert.behavioral_to_hardcaml(ir)`
  - `generate.hardcaml_verilog(circuit, file)`
- Code generation
  - `generate.sv_from_behavioral(ir, file)`
  - `generate.vhdl_from_behavioral(ir, file)`

## Acknowledgments

- Implementation pattern from `../hardcaml-lua/myluaclient.ml`
- lua-ml library by OCaml community
- Building blocks philosophy inspired by UNIX design principles

## See Also

- `INTERACTIVE_CLIENT_README.md` - Original v1 documentation
- `INTERACTIVE_CLIENT_SUMMARY.md` - Implementation history
- `MISSING_LUA_COMMANDS.md` - Analysis of potential commands
- `demo_modular_api.lua` - Comprehensive demonstration script
- `demo_lua25.lua` - Lua 2.5 syntax demonstration
