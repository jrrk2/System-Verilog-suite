# SystemVerilog Decompiler - Unified Interface

A unified SystemVerilog decompiler that converts Verilator JSON AST to various output formats including SystemVerilog and HardCaml OCaml.

## Overview

This tool reads SystemVerilog designs synthesized by Verilator (in JSON format) and converts them to different target formats through a unified command-line interface.

## Features

- **Multiple Backend Support**: Choose from 4 different output backends
- **Command-line Flags**: Select backend via flags instead of separate executables
- **HardCaml Integration**: NEW - Generate HardCaml OCaml code
- **Batch Processing**: Process entire directories of JSON files
- **Single File Mode**: Process individual files

## Building

```bash
dune build
```

The build will produce two executables:
- `sv_main` - Original interface (legacy)
- `sv_main_unified` - New unified interface with backend selection

## Usage

### Unified Interface (Recommended)

```bash
# Scan directory with specific backend
./sv_main_unified scan <backend> <output_dir>

# Process single file
./sv_main_unified file <backend> <json_file> <output_file>
```

### Backends

The tool supports four backends:

1. **standard** (or **std**) - Original SystemVerilog output
   - Direct translation from Verilator AST
   - Behavioral SystemVerilog code
   - Extension: `.sv`

2. **structural** (or **struct**) - Structural SystemVerilog with primitives
   - Uses structural primitives from `structural_primitives.sv`
   - More gate-level representation
   - Extension: `.sv`

3. **yosys** - Yosys-compatible SystemVerilog
   - Output compatible with Yosys synthesis
   - Includes synthesis warnings
   - Extension: `.sv`

4. **hardcaml** (or **hc**) - HardCaml OCaml code (NEW!)
   - Generates OCaml modules using HardCaml library
   - Functional hardware description
   - Extension: `.ml`

## Examples

### Generate Yosys-compatible SystemVerilog

```bash
./sv_main_unified scan yosys results/
```

This processes all JSON files in `obj_dir/` and outputs Yosys-compatible SystemVerilog to `results/`.

### Generate HardCaml OCaml modules

```bash
./sv_main_unified scan hardcaml output_ml/
```

This generates HardCaml OCaml code from your designs.

### Process Single File

```bash
./sv_main_unified file hardcaml input.tree.json my_module.ml
```

### Using Standard Backend

```bash
./sv_main_unified scan standard output_sv/
```

## HardCaml Backend Details

The HardCaml backend **constructs HardCaml circuits directly** using the Signal/Always API, following the `Input_hardcaml.ml` pattern. The backend:

1. **Links against HardCaml** - Imports `open Hardcaml; open Signal; open Always`
2. **Extracts ports** - Parses `Var` nodes to find inputs/outputs with widths
3. **Creates signals** - `Signal.input name width` for each input port
4. **Translates expressions** - Converts AST expressions to `Signal.t` operations
5. **Builds always blocks** - Processes procedural blocks with `Always.compile`
6. **Creates circuits** - `Circuit.create_exn ~name outputs`
7. **Outputs Verilog** - `Rtl.output Verilog circuit`

### Architecture

```
SystemVerilog JSON → sv_parse → sv_node AST
                                     ↓
                          sv_gen_hardcaml (this backend)
                                     ↓
                    Extract ports, signals, expressions
                                     ↓
                Create HardCaml: Signal.input, Variable.wire
                                     ↓
                  Translate to: Signal.t operations
                                     ↓
                Process always blocks: Always.compile
                                     ↓
                  Build: Circuit.create_exn
                                     ↓
                  Output: Rtl.output Verilog
                                     ↓
                           Verilog (.sv file)
```

### Key Features

- ✅ **Direct circuit construction** - Builds `Circuit.t` in memory
- ✅ **Type-safe** - OCaml type system ensures correctness
- ✅ **Expression translation** - Binary ops, unary ops, concat, mux
- ✅ **Always blocks** - Blocking/non-blocking assignments
- ✅ **Control flow** - If/case statements
- ✅ **Width inference** - Automatic width matching for operations
- ✅ **Verilog output** - Standard synthesizable Verilog

### Example

Input (SystemVerilog):
```verilog
module counter(
  input clk,
  input [7:0] data_in,
  output reg [7:0] count
);
  always @(posedge clk)
    count <= count + 1;
endmodule
```

Backend processing:
```ocaml
(* Extract ports *)
let ports = [("clk", 1, `Input); ("data_in", 8, `Input); ("count", 8, `Output)]

(* Create signals *)
let clk = Signal.input "clk" 1 in
let data_in = Signal.input "data_in" 8 in
let count = Variable.wire ~default:(zero 8) in

(* Process always block *)
let alw = count <== (count.value +:. 1) in
Always.compile [alw];

(* Build circuit *)
let circuit = Circuit.create_exn ~name:"counter" 
  [output "count" count.value]

(* Output Verilog *)
Rtl.output Verilog circuit
```

### Installation

```bash
# Install HardCaml (required)
opam install hardcaml

# Build
dune build sv_main_unified.exe
```

### Usage

```bash
./sv_main_unified scan hardcaml output/
# Creates: output/decompile_*.sv (Verilog via HardCaml)
```

The output is **Verilog**, not OCaml code. The HardCaml circuits are constructed internally and then converted to Verilog.

See `HARDCAML_DIRECT.md` for detailed architecture.

## Directory Structure

```
.
├── sv_main_unified.ml      # New unified main with backend selection
├── sv_main.ml              # Original main (legacy)
├── sv_gen.ml               # Standard backend
├── sv_gen_struct.ml        # Structural backend
├── sv_gen_yosys.ml         # Yosys backend
├── sv_gen_hardcaml.ml      # HardCaml backend (NEW)
├── sv_parse.ml             # JSON AST parser
├── sv_transform.ml         # AST transformations
├── sv_tran_struct.ml       # Structural transformations
└── dune                    # Build configuration
```

## Workflow

1. **Synthesize with Verilator**: Convert SystemVerilog to JSON AST
   ```bash
   verilator --xml-only design.sv
   # This generates JSON tree files
   ```

2. **Choose Backend**: Select appropriate output format
   - Use `standard` for readable SystemVerilog
   - Use `yosys` for synthesis toolchain integration
   - Use `structural` for gate-level representation
   - Use `hardcaml` for functional OCaml hardware description

3. **Run Decompiler**: Process JSON with selected backend
   ```bash
   ./sv_main_unified scan hardcaml output/
   ```

4. **Use Output**: The generated files can be:
   - Re-synthesized with other tools (SystemVerilog outputs)
   - Simulated with HardCaml (OCaml outputs)
   - Further analyzed or transformed

## Integration with HardCaml

To use the generated HardCaml modules in your OCaml projects:

1. Add HardCaml dependency to your `dune` file:
   ```lisp
   (libraries hardcaml)
   ```

2. Use the generated module:
   ```ocaml
   open Hardcaml
   
   (* Create a circuit *)
   let scope = Scope.create ~flatten_design:true () in
   let module Circuit = Circuit.With_interface(I_counter)(O_counter) in
   let circuit = Circuit.create_exn ~name:"counter" (create_counter scope)
   
   (* Simulate *)
   let sim = Cyclesim.create circuit in
   (* ... *)
   ```

## Legacy Interface

The original separate executables are still available:

```bash
./sv_main              # Standard backend (scans obj_dir/)
./sv_main_struct       # Structural backend
./sv_main_yosys        # Yosys backend
./sv_main_opt          # Optimized backend
./sv_main_sat          # SAT solver backend
```

## Output Files

- All backends produce output files prefixed with `decompile_`
- Warnings are collected in `warnings.txt` (scan mode)
- File extensions match backend: `.sv` for SystemVerilog, `.ml` for HardCaml

## Compilation Flags

The tool processes JSON files from Verilator and generates compilable output. For SystemVerilog outputs, you may need:

- `structural_primitives.sv` (for structural backend)
- Standard synthesis libraries (for yosys backend)

For HardCaml outputs:
- HardCaml library (`hardcaml` opam package)
- Jane Street Base/Core libraries

## Troubleshooting

### "Unknown backend" error
Make sure you're using a valid backend name: `standard`, `structural`, `yosys`, or `hardcaml`.

### HardCaml compilation errors
Ensure HardCaml is installed:
```bash
opam install hardcaml
```

### Missing files in output
Check that:
1. Input JSON files are in `obj_dir/`
2. Output directory exists or can be created
3. You have write permissions

## Contributing

When adding new backends:
1. Create `sv_gen_<backend>.ml` with generation logic
2. Add backend variant to `sv_main_unified.ml`
3. Update dune file modules list
4. Update this README

## License

[Your License Here]

## See Also

- [Verilator](https://www.veripool.org/verilator/)
- [Yosys](https://yosyshq.net/yosys/)
- [HardCaml](https://github.com/janestreet/hardcaml)
