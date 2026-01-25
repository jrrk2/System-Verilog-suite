# Missing Lua Commands - Available Functionality Not Yet Exposed

## Analysis of Existing .ml Modules

This document identifies functionality in the codebase that could be exposed as Lua commands in the interactive client.

## Currently Exposed (for reference)

✅ `verify.vhdl_regression(file)` - VHDL to Behavioral IR
✅ `verify.sv_regression(file)` - SV to Behavioral IR via Verilator
✅ `verify.structural_equiv(vhdl, sv)` - Compare optimized IR structures
✅ `verify.sat_miter(vhdl, sv)` - Direct Z3 SAT proving
✅ `verify.hardcaml_equiv(vhdl, sv)` - HardCaml interface validation
✅ `verify.hardcaml_sat(vhdl, sv)` - HardCaml normalized equivalence
✅ `verify.verify_all(vhdl, sv)` - Run all verification methods
✅ `verify.help()` - Display help

---

## Missing Functionality - High Priority

### 1. IR Conversion and Generation

**Module**: `opt_ir_to_sv.ml`
```ocaml
let convert ?(verbose=false) ir : string
```
**Proposed Lua Command**:
```lua
convert.ir_to_sv(ir_data, "output.sv")
-- or
sv_code = convert.ir_to_sv(ir_data)
```

**Module**: `behavioural_to_opt_ir.ml`
```ocaml
let convert_to_opt_ir ast : Sv_opt_ir.program
```
**Proposed Lua Command**:
```lua
opt_ir = convert.behavioral_to_opt_ir(behavioral_ir)
```

**Module**: `opt_ir_to_behavioral.ml`
```ocaml
let convert_to_behavioral ir : Behavioral_ir.bprogram
```
**Proposed Lua Command**:
```lua
behavioral = convert.opt_ir_to_behavioral(opt_ir)
```

### 2. JSON Dumping and Serialization

**Module**: `sv_dump_json.ml`
```ocaml
let to_json ?(depth=0) vhd : Yojson.Basic.t
```
**Proposed Lua Command**:
```lua
dump.sv_to_json("input.sv", "output.json")
dump.vhdl_to_json("input.vhd", "output.json")
dump.ir_to_json(ir_data, "output.json")
```

**Module**: `dump_ir_pairs.ml`
```ocaml
let dump_pair vhdl_file sv_file module_name output_dir
```
**Proposed Lua Command**:
```lua
dump.ir_pair("file.vhd", "file.sv", "module_name", "output_dir")
```

### 3. Optimization Passes (Individual Control)

**Module**: `behavioral_optimize.ml`
```ocaml
let optimize_quick prog : bprogram
let optimize_full prog : bprogram
let optimize_custom config prog : bprogram * optimization_stats
```
**Proposed Lua Commands**:
```lua
optimize.quick(behavioral_ir)
optimize.full(behavioral_ir)
optimize.custom(behavioral_ir, {
    constant_prop = true,
    dce = true,
    cse = true,
    verbose = false
})
```

**Individual optimization passes**:
```lua
optimize.const_propagation(ir)
optimize.dead_code_elimination(ir)
optimize.common_subexpr_elimination(ir)
optimize.ssa_conversion(ir)
optimize.register_inference(ir)
```

### 4. Liberty Library and Gate Mapping

**Module**: `sv_liberty.ml`
```ocaml
let parse_liberty_file filename : liberty_library
let print_library_summary lib : unit
let get_cell lib cell_name : cell option
```
**Proposed Lua Commands**:
```lua
lib = liberty.load("sky130_fd_sc_hd.lib")
liberty.print_summary(lib)
cell = liberty.get_cell(lib, "AND2_X1")
is_ff = liberty.is_flip_flop(lib, "DFFQ_X1")
```

**Module**: `sv_gate_map.ml`
```ocaml
let map_operation lib op_type input_signals output_signal
let verilog_of_mapped_netlist netlist : string
```
**Proposed Lua Commands**:
```lua
netlist = gatemap.map_to_liberty(behavioral_ir, lib)
verilog = gatemap.netlist_to_verilog(netlist)
gatemap.write_netlist(netlist, "mapped.v")
```

### 5. Verilator JSON Conversion

**Module**: `verilator_to_behavioral.ml`
```ocaml
let convert_verilator_json_to_behavioral json_file : bprogram option
```
**Proposed Lua Command**:
```lua
behavioral = convert.verilator_json_to_behavioral("obj_dir/module.json")
```

### 6. IR Statistics and Analysis

**Proposed new module** (extract from test files):
```lua
stats = analyze.ir_stats(behavioral_ir)
-- Returns: {
--   inputs = 3,
--   outputs = 1,
--   registers = 2,
--   wires = 10,
--   operations = {
--     BAdd = 2,
--     BEq = 3,
--     ...
--   }
-- }

analyze.print_ir_summary(behavioral_ir)
analyze.compare_ir_structures(ir1, ir2)
```

---

## Missing Functionality - Medium Priority

### 7. Complete Verification Workflows

**Extract from**: `test_clock_div_full_flow.ml`
```lua
workflow.full_verification("module.vhd", "module.sv", {
    steps = {
        "vhdl_to_ir",
        "sv_to_ir",
        "optimize_both",
        "z3_verify",
        "hardcaml_verify"
    },
    output_dir = "results/",
    generate_reports = true
})
```

### 8. SV/VHDL Code Generation from IR

**Module**: `sv_gen.ml`, `sv_gen_struct.ml`
```lua
generate.sv_from_behavioral(behavioral_ir, "output.sv")
generate.structural_sv_from_ir(opt_ir, "output_struct.sv")
generate.vhdl_from_behavioral(behavioral_ir, "output.vhd")
```

### 9. Yosys RTLIL Integration

**Module**: `sv_rtlil_to_ir.ml`, `sv_rtlil_reader.ml`
```lua
ir = convert.rtlil_to_ir("design.il")
rtlil = convert.ir_to_rtlil(behavioral_ir)
```

### 10. HardCaml Circuit Generation

**Module**: `sv_gen_hardcaml.ml`, `behavioral_to_hardcaml.ml`
```lua
hardcaml_circuit = convert.behavioral_to_hardcaml(behavioral_ir)
verilog = hardcaml.generate_verilog(hardcaml_circuit)
```

---

## Missing Functionality - Low Priority

### 11. Token Dumping and Debug

**Module**: `sv_token_dumper.ml`
```lua
debug.dump_tokens("input.sv", "tokens.txt")
debug.dump_ast("input.sv", "ast.txt")
```

### 12. Memory Inference

**Module**: `sv_memory.ml`
```lua
memory_info = analyze.infer_memories(behavioral_ir)
```

### 13. Assignment Ordering

**Module**: Test files show assignment ordering analysis
```lua
ordering = analyze.assignment_order(behavioral_ir)
issues = analyze.find_ordering_issues(behavioral_ir)
```

---

## Proposed Module Organization in Lua

```lua
-- Conversion module
convert.vhdl_to_behavioral(file)
convert.sv_to_behavioral(file)
convert.behavioral_to_opt_ir(ir)
convert.opt_ir_to_behavioral(ir)
convert.opt_ir_to_sv(ir)
convert.verilator_json_to_behavioral(file)
convert.behavioral_to_hardcaml(ir)
convert.rtlil_to_ir(file)

-- Optimization module
optimize.quick(ir)
optimize.full(ir)
optimize.custom(ir, config)
optimize.const_propagation(ir)
optimize.dead_code_elimination(ir)
optimize.common_subexpr_elim(ir)
optimize.ssa_conversion(ir)
optimize.register_inference(ir)

-- Analysis module
analyze.ir_stats(ir)
analyze.print_summary(ir)
analyze.compare_structures(ir1, ir2)
analyze.infer_memories(ir)
analyze.assignment_order(ir)

-- Dump/Serialize module
dump.sv_to_json(file, output)
dump.vhdl_to_json(file, output)
dump.ir_to_json(ir, output)
dump.ir_pair(vhdl, sv, name, dir)
dump.tokens(file, output)

-- Liberty/Gate mapping module
liberty.load(file)
liberty.print_summary(lib)
liberty.get_cell(lib, name)
liberty.is_flip_flop(lib, name)

gatemap.map_to_liberty(ir, lib)
gatemap.netlist_to_verilog(netlist)
gatemap.write_netlist(netlist, file)

-- Generation module
generate.sv_from_behavioral(ir, file)
generate.vhdl_from_behavioral(ir, file)
generate.structural_sv(ir, file)
generate.hardcaml_verilog(circuit, file)

-- Debug module
debug.dump_tokens(file, output)
debug.dump_ast(file, output)
debug.verbose(true/false)

-- Workflow module (high-level)
workflow.full_verification(vhdl, sv, config)
workflow.synthesis_flow(ir, liberty, output)
```

---

## Priority Recommendations

### Immediate Additions (Week 1)
1. ✅ **Optimization control** - Individual passes (const prop, DCE, CSE)
2. ✅ **IR statistics** - Useful for debugging and understanding designs
3. ✅ **JSON dumping** - Critical for interoperability and debugging

### Short-term Additions (Week 2-3)
4. **Liberty library integration** - Essential for synthesis
5. **Gate mapping** - Complete the synthesis flow
6. **IR conversion** - Flexibility in IR transformations

### Long-term Additions
7. Workflow automation
8. HardCaml generation
9. Debug utilities
10. Memory inference

---

## Implementation Strategy

### Phase 1: Core Utilities (Already well-tested)
```ocaml
module MakeConversionLib : USERCODE = struct
    (* IR conversions, JSON dumps, stats *)
end

module MakeOptimizationLib : USERCODE = struct
    (* Individual optimization passes *)
end
```

### Phase 2: Synthesis Path
```ocaml
module MakeSynthesisLib : USERCODE = struct
    (* Liberty loading, gate mapping, netlist generation *)
end
```

### Phase 3: Workflow Automation
```ocaml
module MakeWorkflowLib : USERCODE = struct
    (* High-level verification/synthesis workflows *)
end
```

---

## Example Lua Script with New Commands

```lua
-- Load a Liberty library
lib = liberty.load("sky130_fd_sc_hd.lib")
liberty.print_summary(lib)

-- Convert VHDL to IR
vhdl_ir = convert.vhdl_to_behavioral("sysver_tests/uart_baudgen.vhd")

-- Get statistics
stats = analyze.ir_stats(vhdl_ir)
print("Registers: " .. stats.registers)
print("Operations: " .. stats.operations)

-- Optimize with custom settings
optimized = optimize.custom(vhdl_ir, {
    constant_prop = true,
    dce = true,
    cse = true,
    ssa = false,
    verbose = true
})

-- Map to gates
netlist = gatemap.map_to_liberty(optimized, lib)

-- Generate output
verilog = gatemap.netlist_to_verilog(netlist)
generate.write_file(verilog, "output_mapped.v")

-- Also dump JSON for external tools
dump.ir_to_json(optimized, "ir_optimized.json")

print("Synthesis complete!")
```

---

## Conclusion

There is significant functionality already implemented in .ml modules that could be exposed via Lua commands:

**High value additions**:
- Individual optimization passes
- IR statistics and analysis
- JSON dumping/serialization
- Liberty library integration
- Gate mapping

**Total potential new commands**: ~50-60 functions across 6-7 new modules

This would make the interactive client a comprehensive HDL development and verification environment, not just a verification tool.
