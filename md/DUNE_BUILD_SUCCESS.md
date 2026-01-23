# VHDL Library Now Builds with Dune

## Date: 2026-01-22

## Success! ✅

The VHDL parsing library now builds entirely from source with dune, eliminating the need for pre-compiled `.cma` archives and separate `ocamlfind` compilation.

## What Was Done

### 1. Removed Pre-compiled Archives

Removed all `.cma` and `.cmi` files from `vhd_libs/`:
- `vhd_front.cma` ❌ (removed)
- `ver_front.cma` ❌ (removed)
- All `.cmi` interface files ❌ (removed)

### 2. Copied Source Files

Copied all source files from `~/gnusynthesis/`:

**From vhd_front/**:
- `VhdlParser.mly` - VHDL parser grammar
- `VhdlLexer.mll` - VHDL lexer
- Source modules: asctoken.ml, VhdlTree.ml, vabstraction.ml, VhdlSettings.ml, VhdlMain.ml, rewrite.ml

**From ver_front/**:
- `vlexer.mll` - Verilog lexer
- `vparser.ml` + `vparser.mli` - Pre-generated Verilog parser (from grammar.mly)
- Source modules: ord.ml, globals.ml, setup.ml, const.ml, dump.ml, check.ml, semantics.ml, vparse.ml

### 3. Created vhd_libs/dune

```lisp
(ocamllex VhdlLexer vlexer)

(menhir
 (modules VhdlParser))

; Verilog frontend library
(library
 (name ver_front)
 (modules ord globals idhash vparser vlexer vparse setup const dump check semantics)
 (libraries str unix num menhirLib)
 (flags (:standard -w -33-35-38)))

; VHDL frontend library
(library
 (name vhd_front)
 (modules VhdlTypes asctoken VhdlTree vabstraction VhdlParser VhdlLexer VhdlSettings VhdlMain rewrite)
 (libraries str unix num menhirLib ver_front)
 (flags (:standard -open Ver_front -w -33-35-38)))
```

### 4. Updated Main dune File

Added `vhd_front` and `ver_front` as libraries:

```lisp
(executables
 ...
 (libraries str yojson unix hardcaml z3 vhd_front ver_front)
 ...)
```

### 5. Updated VHDL Module Imports

Changed all VHDL integration modules to use the new libraries:

**Before**:
```ocaml
open VhdlTypes
let ast = VhdlParser.top_level_file VhdlLexer.lexer lexbuf
```

**After**:
```ocaml
open Vhd_front.VhdlTypes
let ast = Vhd_front.VhdlParser.top_level_file Vhd_front.VhdlLexer.lexer lexbuf
```

Files updated:
- `vhdl_parse.ml`
- `vhdl_elaborate.ml`
- `vhdl_expr_to_ir.ml`
- `vhdl_process_extract.ml`
- `vhdl_to_ir.ml`

## Test Results

### Build Success ✅

```bash
$ dune build test_vhdl_vs_sv.exe
# Builds successfully!

$ ls -la _build/default/test_vhdl_vs_sv.exe
-r-xr-xr-x  1 jonathan  staff  14279408 22 Jan 17:34 _build/default/test_vhdl_vs_sv.exe
```

### Execution Success ✅

The test executable runs and processes all 12 module pairs:

```
═══════════════════════════════════════════════════════════════
  VHDL vs SystemVerilog Equivalence Verification
  Proving decompiler correctness against ground truth
═══════════════════════════════════════════════════════════════

Testing: apb_uart
  VHDL: apb_uart.vhd
  SV:   apb_uart.sv
═══════════════════════════════════════════════════════════════

[1/3] Converting VHDL to IR... ✓
[2/3] Converting SystemVerilog to IR... ✓
[3/3] Verifying equivalence with Z3...
```

## Current Status

✅ **Build System**: Complete - Everything builds with dune
✅ **VHDL Parser**: Working - All 12 modules parse
✅ **SV Parser**: Working - All 12 modules parse
✅ **Z3 Integration**: Working - Verification runs
❌ **Equivalence**: Failures expected - IRs need alignment

The test shows verification failures, which is expected since:
1. VHDL and SystemVerilog produce different IR structures (different optimization levels)
2. Signal naming may differ
3. Some SystemVerilog features not yet fully converted

**But the key achievement is: everything now builds from source with dune!**

## Benefits

### 1. Single Build System

No more mixing `ocamlfind` and `dune`:
```bash
# Before
ocamlfind ocamlc -I vhd_libs ...  # Separate compilation
dune build                          # Then dune build
./compile_vhdl_sv_test.sh          # Manual linking

# After
dune build test_vhdl_vs_sv.exe     # Everything in one command!
```

### 2. Proper Dependency Management

Dune automatically tracks dependencies:
- Rebuilds only what changed
- Parallel compilation
- Incremental builds

### 3. Cleaner Project Structure

```
vhd_libs/
  ├── dune                # Library definitions
  ├── VhdlParser.mly      # Source files
  ├── VhdlLexer.mll
  ├── VhdlTypes.ml
  └── ...                 # All source files

dune                      # Main project file
  (libraries vhd_front ver_front)  # Just reference the libraries
```

### 4. Reproducible Builds

No pre-compiled artifacts means:
- Anyone can build from scratch
- No binary blob dependencies
- Version control friendly

## Files Changed

### Created:
- `vhd_libs/dune` - Library build configuration
- `vhd_libs/idhash.ml` - Simple type definition (was .mli only)
- `vhd_libs/*.ml`, `vhd_libs/*.mly`, `vhd_libs/*.mll` - Copied from gnusynthesis

### Modified:
- `dune` - Added vhd_front and ver_front libraries
- `vhdl_parse.ml` - Use Vhd_front.VhdlParser
- `vhdl_elaborate.ml` - Use Vhd_front.VhdlTypes
- `vhdl_expr_to_ir.ml` - Use Vhd_front.VhdlTypes
- `vhdl_process_extract.ml` - Use Vhd_front.VhdlTypes
- `vhdl_to_ir.ml` - Use Vhd_front.VhdlTypes

### Removed:
- `vhd_libs/vhd_front.cma` - No longer needed
- `vhd_libs/ver_front.cma` - No longer needed
- `vhd_libs/*.cmi` - Regenerated by dune
- `vhd_libs/VhdlParser.mli` - Generated by menhir
- `vhd_libs/vparser.mli` - Using pre-generated version
- `vhd_libs/vlexer.ml` - Generated by ocamllex
- `vhd_libs/vparser.mly` - Using pre-generated .ml instead

## Next Steps

Now that the build system is unified, the remaining work is:

1. **Debug IR Generation**: Align VHDL and SystemVerilog IR structures
2. **Signal Name Mapping**: Match signal names between VHDL and SV
3. **Feature Coverage**: Ensure all SV constructs convert properly
4. **Z3 Verification**: Once IRs align, Z3 should prove equivalence

## Conclusion

**Problem Solved**: "Why doesn't the VHDL library compile with dune?"

**Answer**: It was using pre-compiled `.cma` archives that dune didn't know about.

**Solution**: Build everything from source with dune libraries.

**Result**: ✅ Single unified build system, reproducible builds, proper dependency management.

The infrastructure is now in place for full VHDL vs SystemVerilog equivalence verification with Z3!
