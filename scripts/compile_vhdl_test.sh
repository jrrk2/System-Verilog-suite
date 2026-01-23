#!/bin/bash
# Compile and link test_vhdl_vs_sv with VHDL libraries and dune-built modules

set -e

echo "Building dune modules first..."
dune build 2>&1 | tail -5 || true

echo ""
echo "Compiling VHDL modules..."

# Compile VHDL modules
ocamlfind ocamlc -package num,menhirLib,yojson -I vhd_libs -c vhdl_parse.ml
ocamlfind ocamlc -package num,menhirLib,yojson -I vhd_libs -c vhdl_elaborate.ml
ocamlfind ocamlc -package num,menhirLib,yojson -I vhd_libs -c vhdl_expr_to_ir.ml
ocamlfind ocamlc -package num,menhirLib,yojson -I vhd_libs -c vhdl_process_extract.ml
ocamlfind ocamlc -package num,menhirLib,yojson -I vhd_libs -c vhdl_to_ir.ml

echo ""
echo "Compiling test_vhdl_vs_sv..."
ocamlfind ocamlc -package num,menhirLib,yojson,z3 -I vhd_libs -I _build/default -c test_vhdl_vs_sv.ml

echo ""
echo "Linking test_vhdl_vs_sv..."
ocamlfind ocamlc -package num,menhirLib,yojson,z3 -I vhd_libs -I _build/default -linkpkg \
  vhd_libs/vhd_front.cma \
  vhd_libs/ver_front.cma \
  _build/default/sv_ast.cmo \
  _build/default/sv_elaborate.cmo \
  _build/default/sv_verible_to_ir.cmo \
  _build/default/behavioural_to_opt_ir.cmo \
  _build/default/sv_ir_verify.cmo \
  vhdl_parse.cmo \
  vhdl_elaborate.cmo \
  vhdl_expr_to_ir.cmo \
  vhdl_process_extract.cmo \
  vhdl_to_ir.cmo \
  test_vhdl_vs_sv.cmo \
  -o test_vhdl_vs_sv

echo ""
echo "✓ Compiled successfully: ./test_vhdl_vs_sv"
