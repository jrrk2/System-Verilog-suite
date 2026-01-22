# Per-Module Symbol Tables Implementation

**Date:** 2026-01-21  
**Issue:** Symbol table was global, causing warnings when converting individual modules

## Problem

The initial symbol table implementation used a single global hash table for ALL modules in a file. When converting a multi-module file like apb_uart.sv:

1. Elaboration processed all 12 modules and mixed their data together
2. `elab_ctx.ports`, `elab_ctx.assigns`, `elab_ctx.symbol_table` contained data from ALL modules
3. When converting `apb_uart` module to IR, it included ports/signals from `uart_transmitter`, `uart_receiver`, etc.
4. Signal `iFinished` declared in `uart_transmitter` showed as "unknown" when referenced in `uart_transmitter` itself

## Solution

Implemented per-module data structures:

### 1. Per-Module Data Type
```ocaml
type module_data = {
  mutable mod_ports: port_info list;
  mutable mod_assigns: assign_info list;
  mutable mod_always_blocks: always_info list;
}
```

### 2. Updated Elaboration Context
```ocaml
type elab_context = {
  (* ... *)
  module_symbol_tables: (string, (string, signal_info) Hashtbl.t) Hashtbl.t;
  module_data: (string, module_data) Hashtbl.t;  (* NEW *)
  current_module: string option;
  (* ... *)
}
```

### 3. Module-Scoped Extraction
- `extract_port_decl` → adds to `module_data.mod_ports`
- `extract_continuous_assign` → adds to `module_data.mod_assigns`
- `extract_always_comb/ff` → adds to `module_data.mod_always_blocks`
- All additions scoped to `ctx.current_module`

### 4. IR Conversion Uses Module-Specific Data
```ocaml
let module_data = Sv_elaborate.get_module_data elab_ctx module_name in
(* Use module_data.mod_ports instead of elab_ctx.ports *)
(* Use module_data.mod_assigns instead of elab_ctx.assigns *)
(* Use module_data.mod_always_blocks instead of elab_ctx.always_blocks *)
```

## Results

### Before Per-Module Tables
```
Test: apb_uart.sv (12 modules)
Unknown identifier warnings: 10
- iFinished (in uart_transmitter)
- iRXFinished (in uart_receiver)
- iFE, iBI, iDOUT (in uart_receiver)
- iIIR (in uart_interrupt)
- iQ, iUSAGE, iFULL, iEMPTY (in slib_fifo)
```

### After Per-Module Tables
```
Test: apb_uart.sv (12 modules)
Unknown identifier warnings: 0
✓ Each module's symbol table contains only its own signals
✓ No cross-contamination between modules
✓ Converting apb_uart module uses only apb_uart's ports/assigns
```

## Test Results

```bash
=== Symbol Table Implementation Test ===

Test 1: Simple DFF (should have 0 unknown identifier warnings)
--------------------------------------------------------------
Unknown identifiers: 0

Test 2: APB UART (should have ~10 unknown identifier warnings)  
--------------------------------------------------------------
Unknown identifiers: 0  ← Was 10, now 0!

Test 3: Counter (should have 0 unknown identifier warnings)
--------------------------------------------------------------
Unknown identifiers: 0

✓ All tests pass with zero warnings
```

## Files Modified

1. **sv_elaborate.ml**
   - Added `module_data` type
   - Added `module_data` hashtable to `elab_context`
   - Added `get_current_module_data` and `get_module_data` functions
   - Updated port/assign/always extraction to use per-module data
   - Set `ctx.current_module` during module processing

2. **sv_verible_to_ir.ml**
   - Modified `verible_to_ir` to fetch module-specific data
   - Use `module_data.mod_ports` instead of `elab_ctx.ports`
   - Use `module_data.mod_assigns` instead of `elab_ctx.assigns`
   - Use `module_data.mod_always_blocks` instead of `elab_ctx.always_blocks`

3. **test_verible_elab.ml**
   - Use actual module name from `elab_ctx.module_name` instead of filename

## Architecture

```
File: apb_uart.sv
├── Module: uart_transmitter
│   ├── Symbol Table: {iFinished, TXFINISHED, SOUT, ...}
│   ├── Ports: [...]
│   └── Assigns: [...]
├── Module: uart_receiver
│   ├── Symbol Table: {iRXFinished, iFE, iBI, ...}
│   ├── Ports: [...]
│   └── Assigns: [...]
└── Module: apb_uart
    ├── Symbol Table: {PADDR, PWDATA, PRDATA, ...}
    ├── Ports: [...]
    └── Assigns: [...]

When converting "apb_uart" to IR:
✓ Use ONLY apb_uart's symbol table
✓ Use ONLY apb_uart's ports/assigns
✗ Don't mix in data from uart_transmitter or uart_receiver
```

## Conclusion

Per-module symbol tables completely eliminate false warnings by ensuring each module's elaboration data is isolated. Each module can now be converted to IR independently using only its own signals, ports, and assignments.

**Result:** 100% of "Unknown identifier" warnings eliminated for properly scoped signals.
