#!/bin/bash
# Build Z3 verification test for all VHDL/SV pairs

set -e

echo "Building Z3 verification test..."
echo ""

# Step 1: Build with dune first to get all the SV modules
echo "Step 1: Building with dune..."
dune build 2>&1 | tail -5 || true
echo ""

# Step 2: Compile VHDL modules
echo "Step 2: Compiling VHDL modules..."
ocamlfind ocamlc -package num,menhirLib,yojson -I vhd_libs -c vhdl_parse.ml
ocamlfind ocamlc -package num,menhirLib,yojson -I vhd_libs -c vhdl_elaborate.ml
ocamlfind ocamlc -package num,menhirLib,yojson -I vhd_libs -c vhdl_expr_to_ir.ml
ocamlfind ocamlc -package num,menhirLib,yojson -I vhd_libs -c vhdl_process_extract.ml
ocamlfind ocamlc -package num,menhirLib,yojson -I vhd_libs -c vhdl_to_ir.ml
echo "  ✓ VHDL modules compiled"
echo ""

# Step 3: Try to compile test
echo "Step 3: Compiling Z3 test..."
ocamlfind ocamlc -package num,menhirLib,yojson,z3 -I vhd_libs -I _build/default -c test_z3_all_pairs.ml 2>&1 | head -20
result=$?

if [ $result -eq 0 ]; then
    echo "  ✓ Test compiled"
    echo ""
    echo "Step 4: Linking..."

    # Try to link everything together
    ocamlfind ocamlc -package num,menhirLib,yojson,z3,hardcaml -I vhd_libs -I _build/default -linkpkg \
      vhd_libs/vhd_front.cma \
      vhd_libs/ver_front.cma \
      _build/default/.sv_main_unified.eobjs/byte/sv_ast.cmo \
      _build/default/.sv_main_unified.eobjs/byte/sv_elaborate.cmo \
      _build/default/.sv_main_unified.eobjs/byte/behavioural_to_opt_ir.cmo \
      _build/default/.sv_main_unified.eobjs/byte/sv_verible_to_ir.cmo \
      _build/default/.sv_main_unified.eobjs/byte/sv_ir_verify.cmo \
      vhdl_parse.cmo \
      vhdl_elaborate.cmo \
      vhdl_expr_to_ir.cmo \
      vhdl_process_extract.cmo \
      vhdl_to_ir.cmo \
      test_z3_all_pairs.cmo \
      -o test_z3_all_pairs 2>&1 | head -30

    link_result=$?

    if [ $link_result -eq 0 ]; then
        echo ""
        echo "✅ Built successfully: ./test_z3_all_pairs"
        exit 0
    else
        echo ""
        echo "❌ Linking failed"
        exit 1
    fi
else
    echo "  ✗ Compilation failed"
    echo ""
    echo "This is expected - need to integrate build systems."
    echo ""
    echo "Alternative approach: Create separate test that:"
    echo "  1. Runs VHDL→IR conversion (already working)"
    echo "  2. Runs SV→IR conversion (need to test)"
    echo "  3. Compares IRs manually or with separate tool"
    exit 1
fi
