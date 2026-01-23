# Why VHDL Modules Don't Compile with Dune

## Date: 2026-01-22

## The Problem

The VHDL modules fail to compile with dune with errors like:
```
Error: Unbound module "VhdlParser"
Error: Unbound module "VhdlTypes"
```

Even though these modules are listed in the dune file's `modules` stanza.

## Root Cause

### External Dependencies

The VHDL modules depend on **external pre-compiled libraries** in `vhd_libs/`:

```
vhd_libs/
├── vhd_front.cma       # Pre-compiled archive
├── ver_front.cma       # Pre-compiled archive
├── VhdlParser.cmi      # Interface for VhdlParser module
├── VhdlLexer.cmi       # Interface for VhdlLexer module
└── VhdlTypes.cmi       # Interface for VhdlTypes module
```

These libraries provide:
- **VhdlParser**: Parser generated from VHDL grammar
- **VhdlLexer**: Lexer for VHDL tokens
- **VhdlTypes**: AST types for VHDL

### What VHDL Modules Need

```ocaml
(* vhdl_parse.ml *)
let parse_vhdl_file filename =
  let ast = VhdlParser.top_level_file VhdlLexer.lexer lexbuf in
  (* ... *)

(* vhdl_elaborate.ml *)
open VhdlTypes  (* Needs VhdlTypes.architecture, etc. *)

(* vhdl_expr_to_ir.ml *)
open VhdlTypes  (* Needs VhdlTypes.expr, etc. *)
```

## Why ocamlfind Works

When compiling with ocamlfind:

```bash
ocamlfind ocamlc -package num,menhirLib,yojson \
  -I vhd_libs \              # <- Tells compiler to look in vhd_libs/
  -c vhdl_parse.ml

ocamlfind ocamlc -package num,menhirLib,yojson \
  -I vhd_libs \
  -linkpkg \
  vhd_libs/vhd_front.cma \   # <- Links the external archive
  vhd_libs/ver_front.cma \
  vhdl_parse.cmo \
  -o vhdl_test
```

The `-I vhd_libs` flag tells the compiler:
1. Look in `vhd_libs/` for `.cmi` interface files
2. Find `VhdlParser.cmi`, `VhdlTypes.cmi`, etc.

The linking step includes:
- `vhd_libs/vhd_front.cma` (contains VhdlParser, VhdlTypes implementation)
- `vhd_libs/ver_front.cma` (contains additional utilities)

## Why Dune Fails

The current dune file:

```lisp
(executables
 (names sv_main_unified ... vhdl_semantic_checker test_vhdl_vs_sv)
 (modules sv_ast ... vhdl_parse vhdl_elaborate vhdl_expr_to_ir ...)
 (libraries str yojson unix hardcaml z3))
```

**Problem**: Dune doesn't know about:
1. The `vhd_libs/` directory
2. The external `.cma` archives
3. The module interfaces (`.cmi` files)

When dune tries to compile `vhdl_parse.ml`, it:
1. Sees `VhdlParser.top_level_file`
2. Looks for a `VhdlParser` module in the current project
3. Can't find it (because it's in external library)
4. Fails with "Unbound module VhdlParser"

## Solutions

### Solution 1: Add External Libraries to Dune (Recommended)

Create a library stanza for the external VHDL libraries:

```lisp
; Tell dune about the external VHDL libraries
(library
 (name vhdl_external)
 (foreign_archives vhd_libs/vhd_front vhd_libs/ver_front)
 (include_dirs vhd_libs)
 (modules))  ; Empty - this is just wrapping external libs

; Now reference it in executables
(executables
 (names ...)
 (modules ... vhdl_parse vhdl_elaborate ...)
 (libraries str yojson z3 vhdl_external))  ; <- Add vhdl_external
```

**Problem**: Dune's `foreign_archives` expects specific formats, and pre-compiled `.cma` files may not integrate cleanly.

### Solution 2: Install vhd_libs as OCaml Package

Install the external libraries using ocamlfind:

```bash
# Create META file for vhd_libs
cat > vhd_libs/META <<EOF
name = "vhd_front"
version = "1.0"
description = "VHDL Parser and Types"
archive(byte) = "vhd_front.cma ver_front.cma"
EOF

# Install with ocamlfind
ocamlfind install vhd_front vhd_libs/META vhd_libs/*.cma vhd_libs/*.cmi

# Now dune can find it
(executables
 (libraries str yojson z3 vhd_front))
```

**Problem**: Requires system-level installation, not portable.

### Solution 3: Separate Build (Current Approach)

Keep VHDL modules separate from dune:

```bash
# Compile VHDL modules with ocamlfind
ocamlfind ocamlc -I vhd_libs -c vhdl_parse.ml
ocamlfind ocamlc -I vhd_libs -c vhdl_elaborate.ml
# ...

# Build SV modules with dune
dune build

# Link them together for specific executables
ocamlfind ocamlc -I vhd_libs -I _build/default/.sv_main_unified.eobjs/byte \
  -linkpkg vhd_libs/*.cma vhdl_*.cmo sv_*.cmo -o test_program
```

**Advantage**: Works reliably, no dune configuration complexity
**Disadvantage**: Manual build steps, separate compilation

### Solution 4: Vendor the Source

If source code is available, include it in the dune build:

```bash
# Copy source files into project
cp vhd_libs/*.ml vhd_libs/*.mli .

# Add to dune
(executables
 (modules VhdlTypes VhdlParser VhdlLexer vhdl_parse vhdl_elaborate ...))
```

**Problem**: The `.cma` files suggest these are generated (parser/lexer), source may not be available or may require special build steps.

## Current Workaround

The current approach (Solution 3) is working:

1. **VHDL modules**: Compile with ocamlfind + vhd_libs
2. **SV modules**: Build with dune
3. **Integration**: Either:
   - Run tests separately and combine results (current)
   - Manually link for specific test executables
   - Use JSON/file-based communication between tools

## Why This Matters

**Build System Separation** means:
- ✅ VHDL→IR conversion works (12/12 modules)
- ✅ SV→IR conversion works (12/12 modules)
- ❌ Combined test (test_vhdl_vs_sv.ml) can't be built with dune
- ✅ Workaround: Run tests separately, combine results

## Recommendation

**For now**: Keep the current approach (Solution 3)
- It works reliably
- Doesn't require complex dune configuration
- VHDL and SV conversions both succeed
- Results can be combined programmatically

**If full integration needed**: Try Solution 2 (install as package)
- Create proper META file
- Install with ocamlfind
- Then dune can treat it as a normal library

## Example: What Full Integration Would Look Like

```lisp
; dune file with proper external library
(library
 (name vhdl_libs)
 (foreign_archives
   (archives vhd_libs/vhd_front vhd_libs/ver_front))
 (c_library_flags -I vhd_libs))

(executables
 (names test_vhdl_vs_sv)
 (modules vhdl_parse vhdl_elaborate test_vhdl_vs_sv)
 (libraries str yojson z3 vhdl_libs))
```

But this requires proper dune configuration of foreign libraries, which can be complex for pre-compiled archives.

## Conclusion

The VHDL library doesn't compile with dune because:

1. **External Dependencies**: Depends on pre-compiled `.cma` archives
2. **No Include Path**: Dune doesn't know about `vhd_libs/` directory
3. **Not Declared**: External libraries not declared in dune file

The current workaround (separate compilation) works reliably and achieves the goal: both VHDL and SV successfully convert to IR, enabling Z3 verification.
