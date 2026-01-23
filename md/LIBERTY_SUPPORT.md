# Liberty File Support

## Overview

The SystemVerilog decompiler now includes support for reading Liberty (.lib) files for gate-level mapping. This enables technology-aware circuit generation using standard cell libraries.

## Features

### Sv_liberty Module

A new module `sv_liberty.ml` provides:

1. **Liberty File Parsing**
   - Reads standard Liberty format (.lib files)
   - Extracts cell definitions with pins, directions, and functions
   - Identifies cell types (combinational, flip-flop, latch)

2. **Cell Information Extraction**
   - Cell name and type
   - Pin names and directions (input/output/inout)
   - Boolean functions for outputs
   - FF/latch identification

3. **Query Functions**
   ```ocaml
   val parse_liberty_file : string -> library_info
   val get_cell : library_info -> string -> cell_info option
   val get_cell_pins : library_info -> string -> pin_info list
   val get_cell_function : library_info -> string -> string -> string option
   val is_flip_flop : library_info -> string -> bool
   val is_latch : library_info -> string -> bool
   ```

## Usage

### Basic Example

```ocaml
(* Load liberty file *)
let lib = Sv_liberty.parse_liberty_file "mylib.lib" in

(* Query a cell *)
match Sv_liberty.get_cell lib "AND2_X1" with
| Some cell ->
    Printf.printf "Found cell: %s\n" cell.cell_name;
    List.iter (fun pin ->
      Printf.printf "  Pin %s: %s\n"
        pin.name
        (Sv_liberty.string_of_direction pin.direction)
    ) cell.pins
| None -> Printf.printf "Cell not found\n"

(* Get cell function *)
match Sv_liberty.get_cell_function lib "AND2_X1" "Z" with
| Some func -> Printf.printf "Function: %s\n" func
| None -> Printf.printf "No function defined\n"
```

### Testing

A test program `test_liberty.ml` is provided:

```bash
# Build test program
ocamlopt -o test_liberty str.cmxa sv_liberty.ml test_liberty.ml

# Run on a liberty file
./test_liberty mylib.lib
```

### Example Liberty File Format

```
library (test_lib) {
  cell (AND2) {
    pin (A) {
      direction : input;
    }
    pin (B) {
      direction : input;
    }
    pin (Z) {
      direction : output;
      function : "(A & B)";
    }
  }

  cell (DFF) {
    ff(IQ, IQN) {
      clocked_on : "CLK";
      next_state : "D";
    }
    pin (CLK) {
      direction : input;
    }
    pin (D) {
      direction : input;
    }
    pin (Q) {
      direction : output;
    }
  }
}
```

## Integration with HardCaml Backend

The liberty file parser can be integrated with the HardCaml backend (`sv_gen_hardcaml.ml`) to:

1. **Map abstract operations to standard cells**
   - AND, OR, XOR, NOT gates
   - Flip-flops and latches
   - Multiplexers

2. **Generate technology-specific netlists**
   - Use actual cell names from the library
   - Respect pin naming conventions
   - Include timing/power information

3. **Enable optimization**
   - Cell selection based on drive strength
   - Area/power optimizations
   - Technology-specific optimizations

## Future Enhancements

Planned features:

1. **Enhanced parsing**
   - Support for more complex Liberty syntax
   - Timing arc extraction
   - Power model extraction

2. **Gate mapping**
   - Automatic operation-to-cell mapping
   - Multi-output cell support
   - Sequential cell mapping (FF, latches)

3. **Optimization**
   - Drive strength selection
   - Area minimization
   - Power optimization

4. **Backend integration**
   - Add --liberty flag to sv_main_unified
   - Automatic cell selection in HardCaml backend
   - Technology-aware circuit generation

## Dependencies

- OCaml str library (for regex parsing)
- No external dependencies required

## Testing

Tested with:
- Simple hand-written Liberty files
- Nangate Open Cell Library (partial support)
- Standard cell format conventions

## Known Limitations

1. Complex Liberty constructs may not be fully supported
2. Focus on combinational and basic sequential cells
3. Timing/power information parsed but not yet utilized
4. Large commercial libraries may require additional parsing robustness

## References

- Liberty format specification
- Example: `test_simple_liberty.lib` (included)
- Nangate Open Cell Library
- rtl_map.ml from hardcaml-lua project (inspiration)
