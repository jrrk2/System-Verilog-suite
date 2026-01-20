# sv_main_unified Quick Start Guide

Quick reference for using the SystemVerilog decompiler.

## Installation

```bash
# Install dependencies
opam install dune yojson

# Build
dune build

# Optional: view manual
man ./sv_main_unified.1
```

## Basic Usage

### 1. Generate Verilator JSON
```bash
verilator --json-only design.sv
# Creates: obj_dir/Vdesign.tree.json
```

### 2. Decompile with Default Backend
```bash
# Process all files in obj_dir/
_build/default/sv_main_unified.exe scan hardcaml results/

# Process single file
_build/default/sv_main_unified.exe file hardcaml \
    obj_dir/Vdesign.tree.json \
    output.sv
```

## Backends

| Backend | Use Case | Output |
|---------|----------|--------|
| **standard** | Code review, documentation | Clean SystemVerilog |
| **structural** | Synthesis, timing analysis | Primitives + memory inference |
| **yosys** | Open-source FPGA | Yosys-compatible |
| **hardcaml** | Formal verification | OCaml HardCaml |

### Standard - Readable Output
```bash
sv_main_unified scan standard results/
```
**Best for**: Understanding designs, code review

### Structural - Synthesis Ready
```bash
sv_main_unified scan structural results/
```
**Best for**: ASIC/FPGA synthesis, includes:
- Memory primitive detection
- Register inference reporting (Synopsys DC style)
- Conflict checking

### Yosys - Open Source Tools
```bash
sv_main_unified scan yosys results/
```
**Best for**: Open FPGA flows, Yosys formal

### HardCaml - Formal Methods
```bash
sv_main_unified scan hardcaml results/
```
**Best for**: Z3 verification, research

## Common Workflows

### Workflow 1: Design Review
```bash
# 1. Compile design
verilator --json-only mycpu.sv

# 2. Generate readable output
sv_main_unified scan standard results/

# 3. Review
cat results/decompile_Vmycpu.tree.json.sv
```

### Workflow 2: ASIC Synthesis
```bash
# 1. Compile with all modules
verilator --json-only --top-module top design.sv

# 2. Generate structural with register reporting
sv_main_unified scan structural results/

# 3. Check inference reports
grep "Total inferred" results/warnings.txt

# 4. Synthesize
dc_shell
read_verilog structural_primitives.sv
read_verilog results/decompile_Vtop.tree.json.sv
compile
```

### Workflow 3: Formal Verification
```bash
# 1. Generate with verification
sv_main_unified scan hardcaml results/ --verify

# 2. Check results
grep "Verification" results/warnings.txt

# ✓ VERIFIED: Functionally equivalent
```

### Workflow 4: Memory Analysis
```bash
# 1. Generate structural output
sv_main_unified scan structural results/

# 2. Check memory detection
grep "Memory:" results/warnings.txt
grep "Access pattern" results/warnings.txt

# Example output:
# Memory: regfile[32] (depth=32, width=64, size=2048 bits)
# Access pattern for regfile: 2 reads, 1 writes, sequential
```

## Features

### Memory Detection
Automatic detection and primitive inference:

```systemverilog
// Input: Large array
logic [31:0] mem [0:255];  // 8KB array

// Output: Memory primitive
memory_1w2r #(
    .DEPTH(256),
    .WIDTH(32)
) mem_inst (...);
```

**Threshold**: Arrays >128 bits use primitives

### Register Inference Reporting
Synopsys DC-style reports for each always block:

```
Inferred memory devices in process
    in routine always_seq_1
===============================================
| Register Name    | Type | Width | Bus | AR |
===============================================
| pipeline_reg1    | Flop |    32 |   Y | Y  |
| counter          | Flop |    16 |   Y | N  |
===============================================
| Total: 2 registers          Bits:     48   |
===============================================
```

### Memory Conflict Detection
Prevents invalid multi-port configurations:

```
Error:  Too many write ports to memory 'mem'
        3 write port(s) detected, maximum 2 supported
        Consider: arbitration logic or banking the memory
```

### 4-State Value Sanitization
Automatic handling of x/z values:

```systemverilog
// Input
reg [31:0] x = 32'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx;

// Output (sanitized)
reg [31:0] x = 32'h00000000;  // x/z → 0 for synthesis
```

## Command Reference

### Scan Mode
```bash
sv_main_unified scan <backend> <output_dir> [--verify]
```

**Arguments**:
- `backend`: standard|structural|yosys|hardcaml
- `output_dir`: Where to write results
- `--verify`: Optional Z3 verification

**Output**:
- `output_dir/decompile_*.sv` - Generated files
- `output_dir/warnings.txt` - Warnings and reports

**Example**:
```bash
sv_main_unified scan structural results/
# Processes all obj_dir/*.tree.json
# Writes to results/
```

### File Mode
```bash
sv_main_unified file <backend> <json_file> <output_file>
```

**Arguments**:
- `backend`: standard|structural|yosys|hardcaml
- `json_file`: Input Verilator JSON
- `output_file`: Output SystemVerilog

**Example**:
```bash
sv_main_unified file hardcaml \
    obj_dir/Vcpu.tree.json \
    cpu_hardcaml.sv
```

## Testing

### Run Test Suite
```bash
# All tests
./test/run_tests.sh

# Quick smoke test
make -C test quick

# Specific feature
make -C test memory    # Memory detection
make -C test register  # Register inference
make -C test backends  # All backends
```

### Manual Test
```bash
# Create simple test
cat > test.sv << 'EOF'
module test(input clk, output reg [7:0] q);
  always @(posedge clk) q <= q + 1;
endmodule
EOF

# Process
verilator --json-only test.sv
sv_main_unified file structural \
    obj_dir/Vtest.tree.json \
    test_out.sv

# View result
cat test_out.sv
```

## Troubleshooting

### "No files processed"
**Problem**: obj_dir/ is empty
**Solution**:
```bash
ls obj_dir/*.tree.json  # Check files exist
verilator --json-only design.sv  # Regenerate
```

### "Backend failed"
**Problem**: Complex design not supported
**Solution**: Try simpler backend
```bash
# Try standard first
sv_main_unified scan standard results/
# Then structural
sv_main_unified scan structural results/
```

### "Memory conflicts detected"
**Problem**: Too many memory ports (>2R/2W)
**Solution**: Redesign with banking or arbitration
```verilog
// BAD: 3 read ports
always @(posedge clk) begin
    out1 <= mem[addr1];
    out2 <= mem[addr2];
    out3 <= mem[addr3];  // Too many!
end

// GOOD: Time-multiplex
always @(posedge clk) begin
    if (sel)
        out <= mem[addr1];
    else
        out <= mem[addr2];
end
```

### "Verification failed"
**Problem**: Generated code not equivalent
**Solution**: Expected for optimizations
```bash
# Verification is conservative
# False failures can occur for:
# - Constant propagation
# - Dead code elimination
# - Optimization passes
```

## Tips

### Performance
- **Standard**: Fastest (~1000 LOC/sec)
- **Structural**: Moderate (~500 LOC/sec)
- **HardCaml**: Slower (~300 LOC/sec)
- **Verification**: Very slow (~50 LOC/sec)

### Memory Usage
- Typical: 50-200 MB per design
- Large designs (>10K LOC): up to 1 GB
- Verification: 2-3x base memory

### Best Practices
1. Start with `standard` backend for debugging
2. Use `structural` for production synthesis
3. Enable verification only for critical blocks
4. Check warnings.txt for issues
5. Review register inference reports

## Example Outputs

### Counter (Structural)
```verilog
module counter(input clk, input reset, output [7:0] count);
    dff_en #(.WIDTH(8), .RESET_VAL(0)) count_dff(
        .clk(clk),
        .d(count_next),
        .q(count),
        .en(1'b1),
        .rst(reset)
    );
    assign count_next = count + 8'd1;
endmodule
```

### Memory (Structural)
```verilog
module regfile(input clk, input [4:0] addr, output [31:0] data);
    memory_1w2r #(
        .DEPTH(32),
        .WIDTH(32)
    ) regfile_inst (
        .clk(clk),
        .we(we),
        .waddr(waddr),
        .wdata(wdata),
        .raddr1(addr),
        .rdata1(data),
        .raddr2(5'b0),
        .rdata2()
    );
endmodule
```

## Help

### Documentation
```bash
# Manual page
man ./sv_main_unified.1

# Built-in help
sv_main_unified

# Test suite docs
cat test/README.md
```

### Support
- Issues: https://github.com/anthropics/sv-decompiler/issues
- Documentation: See README.md and manual page
- Examples: See test/ directory

## Advanced Usage

### Batch Processing
```bash
# Process multiple designs
for design in design1 design2 design3; do
    verilator --json-only ${design}.sv
done
sv_main_unified scan structural results/
```

### Custom Primitives
```bash
# Use custom primitive library
cp my_primitives.sv structural_primitives.sv
sv_main_unified scan structural results/
```

### Integration with Synthesis
```bash
# For Yosys
sv_main_unified scan yosys results/
yosys -p "read_verilog results/*.sv; synth_xilinx; write_verilog out.v"

# For Design Compiler
sv_main_unified scan structural results/
dc_shell -f synth.tcl
```

## Quick Reference Card

| Task | Command |
|------|---------|
| Decompile all | `sv_main_unified scan hardcaml results/` |
| Single file | `sv_main_unified file backend in.json out.sv` |
| With verification | `sv_main_unified scan backend results/ --verify` |
| Run tests | `./test/run_tests.sh` |
| View manual | `man ./sv_main_unified.1` |
| Check memory | `grep "Memory:" results/warnings.txt` |
| Check registers | `grep "Total inferred" results/warnings.txt` |

---

**Version**: 1.0
**Last Updated**: January 2026
**License**: MIT
