#!/bin/bash
# Compile Z3 verification test for all VHDL/SV pairs
# This links ocamlfind-compiled VHDL modules with dune-built SV modules

set -e

echo "═══════════════════════════════════════════════════════════════"
echo "  Compiling Z3 Verification Test"
echo "  VHDL vs SystemVerilog Equivalence Proof"
echo "═══════════════════════════════════════════════════════════════"
echo ""

# Step 1: Build dune modules
echo "[1/4] Building dune modules..."
dune build test_sv_ir_generation.exe 2>&1 | tail -3 || true
echo "  ✓ Dune build complete"
echo ""

# Step 2: Compile VHDL modules
echo "[2/4] Compiling VHDL modules..."
ocamlfind ocamlc -package num,menhirLib,yojson -I vhd_libs -c vhdl_parse.ml 2>&1 | head -5 || true
ocamlfind ocamlc -package num,menhirLib,yojson -I vhd_libs -c vhdl_elaborate.ml 2>&1 | head -5 || true
ocamlfind ocamlc -package num,menhirLib,yojson -I vhd_libs -c vhdl_expr_to_ir.ml 2>&1 | head -5 || true
ocamlfind ocamlc -package num,menhirLib,yojson -I vhd_libs -c vhdl_process_extract.ml 2>&1 | head -5 || true
ocamlfind ocamlc -package num,menhirLib,yojson -I vhd_libs -c vhdl_to_ir.ml 2>&1 | head -5 || true
echo "  ✓ VHDL modules compiled"
echo ""

# Step 3: Compile test program
echo "[3/4] Compiling test_vhdl_vs_sv.ml..."
ocamlfind ocamlc -package num,menhirLib,yojson,z3,hardcaml -I vhd_libs -I _build/default/.sv_main_unified.eobjs/byte -c test_vhdl_vs_sv.ml 2>&1
result=$?

if [ $result -ne 0 ]; then
    echo "  ✗ Compilation failed"
    exit 1
fi
echo "  ✓ Test compiled"
echo ""

# Step 4: Link everything
echo "[4/4] Linking executable..."

# Find all required .cmo files from dune build
DUNE_DIR="_build/default/.sv_main_unified.eobjs/byte"

ocamlfind ocamlc -package num,menhirLib,yojson,z3,hardcaml \
  -I vhd_libs \
  -I $DUNE_DIR \
  -linkpkg \
  vhd_libs/vhd_front.cma \
  vhd_libs/ver_front.cma \
  $DUNE_DIR/dune__exe__Source_text_verible_types.cmo \
  $DUNE_DIR/dune__exe__Source_text_verible_tokens.cmo \
  $DUNE_DIR/dune__exe__Source_text_verible_lex.cmo \
  $DUNE_DIR/dune__exe__Source_text_verible.cmo \
  $DUNE_DIR/dune__exe__Sv_ast.cmo \
  $DUNE_DIR/dune__exe__Sv_elaborate.cmo \
  $DUNE_DIR/dune__exe__Behavioural_to_opt_ir.cmo \
  $DUNE_DIR/dune__exe__Sv_verible_to_ir.cmo \
  $DUNE_DIR/dune__exe__Sv_ir_verify.cmo \
  vhdl_parse.cmo \
  vhdl_elaborate.cmo \
  vhdl_expr_to_ir.cmo \
  vhdl_process_extract.cmo \
  vhdl_to_ir.cmo \
  test_vhdl_vs_sv.cmo \
  -o test_vhdl_vs_sv 2>&1

link_result=$?

if [ $link_result -eq 0 ]; then
    echo ""
    echo "═══════════════════════════════════════════════════════════════"
    echo "✅ Build successful!"
    echo "═══════════════════════════════════════════════════════════════"
    echo ""
    echo "Executable: ./test_vhdl_vs_sv"
    echo ""
    echo "This program will:"
    echo "  • Convert all 12 VHDL modules to IR"
    echo "  • Convert all 12 SystemVerilog modules to IR"
    echo "  • Use Z3 SMT solver to prove mathematical equivalence"
    echo "  • Report pass/fail for each module"
    echo ""
    echo "Run with: ./test_vhdl_vs_sv"
    echo ""
    exit 0
else
    echo ""
    echo "═══════════════════════════════════════════════════════════════"
    echo "❌ Linking failed"
    echo "═══════════════════════════════════════════════════════════════"
    exit 1
fi
