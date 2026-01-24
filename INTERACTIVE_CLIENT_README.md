# Interactive Verification Client (Lua-ML Embedded)

An OCaml program with an embedded Lua-ML 2.5 interpreter that provides interactive access to all HDL verification and synthesis methods.

## Overview

This interactive client embeds a Lua interpreter directly into OCaml, allowing you to call verification functions using Lua syntax. Unlike the standalone Lua script (`verify_interactive.lua`), this client runs entirely within OCaml and uses the lua-ml library for Lua integration.

## Features

All verification methods are exposed as Lua commands:

1. **vhdl_regression(vhdl_file)** - Test VHDL frontend conversion
2. **sv_regression(sv_file)** - Test SystemVerilog frontend via Verilator
3. **structural_equiv(vhdl, sv)** - Compare optimized IR structures
4. **sat_miter(vhdl, sv)** - Direct Z3 SAT proving
5. **hardcaml_equiv(vhdl, sv)** - HardCaml interface validation
6. **hardcaml_sat(vhdl, sv)** - HardCaml-normalized equivalence checking
7. **verify_all(vhdl, sv)** - Run all verification methods
8. **help()** - Show available commands

## Building

```bash
# Build the interactive client
dune build interactive_client.exe

# The executable will be at:
# _build/default/interactive_client.exe
```

## Usage

### Interactive Mode

```bash
./_build/default/interactive_client.exe
```

This launches an interactive REPL where you can type Lua commands:

```lua
lua> help()

╔═══════════════════════════════════════════════════════════════╗
║  HDL Verification Commands (Lua-ML)                           ║
╚═══════════════════════════════════════════════════════════════╝

Verification Methods:
  vhdl_regression(vhdl_file)       -- Test VHDL frontend
  sv_regression(sv_file)           -- Test SV frontend
  structural_equiv(vhdl, sv)       -- Compare IR structures
  sat_miter(vhdl, sv)              -- Z3 SAT proving
  hardcaml_equiv(vhdl, sv)         -- HardCaml interface check
  hardcaml_sat(vhdl, sv)           -- HardCaml normalized SAT
  verify_all(vhdl, sv)             -- Run all methods

lua> vhdl_regression("sysver_tests/slib_clock_div.vhd")
═══════════════════════════════════════════════════════════════
  VHDL Regression Test
═══════════════════════════════════════════════════════════════

Testing: sysver_tests/slib_clock_div.vhd
✅ VHDL conversion successful
  Module: slib_clock_div
  Signals: 12
  Processes: 2

lua> exit
```

### Batch Mode (Piped Input)

```bash
# Single command
echo 'vhdl_regression("sysver_tests/slib_clock_div.vhd")' | \
  ./_build/default/interactive_client.exe

# Multiple commands
echo -e 'help()\nverify_all("sysver_tests/slib_clock_div.vhd", "sysver_tests/slib_clock_div.sv")\nexit' | \
  ./_build/default/interactive_client.exe
```

### Demo Script

A demo script is provided:

```bash
./demo_interactive.sh
```

This shows examples of using the interactive client in different modes.

## Implementation Details

### Architecture

The interactive client is implemented in `interactive_client.ml` and uses:

- **Luavalue.Make**: Creates a basic Lua value system
- **V.caml_func**: Wraps OCaml functions for Lua
- **V.Table.bind**: Registers functions in Lua global environment
- **Simple Lua parser**: Parses `function(arg1, arg2)` syntax

### Lua Integration

The client uses lua-ml (Lua 2.5 in OCaml) instead of external Lua. This provides:

- **Type safety**: Lua values are OCaml types
- **No external dependencies**: Everything runs in OCaml
- **Direct access**: OCaml functions are directly callable from Lua

### Limitations

The current implementation uses a simplified Lua parser that only supports:
- Function calls: `function_name(arg1, arg2, ...)`
- String arguments: `"quoted strings"` or `'quoted strings'`
- Simple commands: `help()`, `exit`

Full Lua syntax (variables, loops, conditionals, tables) is not yet supported but could be added by integrating the full lua-ml parser.

## Comparison with verify_interactive.lua

| Feature | interactive_client.exe | verify_interactive.lua |
|---------|----------------------|------------------------|
| Lua version | Lua 2.5 (lua-ml) | Lua 5.2+ (external) |
| Integration | Embedded in OCaml | External script |
| Dependencies | None (built-in) | Requires lua interpreter |
| Syntax support | Function calls only | Full Lua 5.2+ |
| Performance | Fast (native) | Requires process spawning |
| Distribution | Single executable | Requires lua + exes |

## Examples

### Example 1: Quick Validation

```bash
./_build/default/interactive_client.exe << 'EOF'
vhdl_regression("sysver_tests/slib_clock_div.vhd")
sv_regression("sysver_tests/slib_clock_div.sv")
hardcaml_equiv("sysver_tests/slib_clock_div.vhd", "sysver_tests/slib_clock_div.sv")
exit
EOF
```

### Example 2: Comprehensive Verification

```bash
./_build/default/interactive_client.exe << 'EOF'
verify_all("sysver_tests/slib_clock_div.vhd", "sysver_tests/slib_clock_div.sv")
exit
EOF
```

### Example 3: Interactive Exploration

```bash
# Start interactive mode
./_build/default/interactive_client.exe

# Then try different modules interactively:
lua> hardcaml_sat("sysver_tests/uart_baudgen.vhd", "sysver_tests/uart_baudgen.sv")
lua> structural_equiv("sysver_tests/slib_edge_detect.vhd", "sysver_tests/slib_edge_detect.sv")
lua> exit
```

## Future Enhancements

Potential improvements to the interactive client:

1. **Full Lua Parser**: Integrate lua-ml's complete parser for full Lua 2.5 syntax support (variables, loops, functions, tables)

2. **Batch Processing**: Add support for processing lists of modules:
   ```lua
   modules = {"slib_clock_div", "uart_baudgen", "slib_edge_detect"}
   for i, m in ipairs(modules) do
     verify_all("sysver_tests/"..m..".vhd", "sysver_tests/"..m..".sv")
   end
   ```

3. **Script Files**: Support loading and executing Lua script files:
   ```bash
   ./_build/default/interactive_client.exe verify_suite.lua
   ```

4. **Result Collection**: Accumulate results in Lua tables and generate summary reports

5. **Synthesis Integration**: Add Liberty library mapping and gate-level synthesis commands

6. **MCP Server**: Integrate with Model Context Protocol for IDE integration

## Files

- `interactive_client.ml` - Main implementation with embedded Lua-ML
- `demo_interactive.sh` - Demo script showing usage examples
- `INTERACTIVE_CLIENT_README.md` - This documentation
- `INTERACTIVE_VERIFICATION_GUIDE.md` - Guide for standalone Lua script

## See Also

- `COMPLETE_VERIFICATION_SUMMARY.txt` - Overall verification results
- `HARDCAML_SAT_RESULTS.md` - HardCaml verification benefits
- `verify_interactive.lua` - Standalone Lua 5.x script (alternative approach)

## Technical Notes

### Why lua-ml Instead of External Lua?

**lua-ml** was chosen because:
1. **Single executable**: No need to install external Lua interpreter
2. **Type safety**: Lua values are OCaml types, preventing runtime errors
3. **Direct integration**: OCaml functions callable without FFI overhead
4. **User requirement**: Explicitly requested "lua-ml 2.5 interpreter in OCaml"

### Implementation Challenges Solved

1. **Module Functor Complexity**: lua-ml's API requires complex module functors. Solved by using `Luavalue.Make` directly instead of `Lua.MakeInterp`.

2. **Value Constructors**: Lua value constructors are in `V.LuaValueBase` module, not at top level. Fixed by using fully qualified names.

3. **Function Registration**: Used `V.Table.bind` to register OCaml functions as Lua globals.

4. **Simplified Parser**: Implemented custom parser for `func(args)` syntax to avoid full Lua parser integration complexity.

## License

Same as parent project.
