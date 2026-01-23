#!/bin/bash
set -e

echo "Testing all blocking assignment modules from blocking.sv"
echo "==========================================================="

MODULES=("test_add" "test_sub" "test_mul" "test_bit_and" "test_bit_or" "test_bit_xor" "test_shl" "test_shr")

for mod in "${MODULES[@]}"; do
    echo ""
    echo "Testing $mod..."
    verilator --lint-only --dump-tree-json -Wno-fatal --top-module "$mod" sysver_tests/blocking.sv > /dev/null 2>&1
    ./_build/default/sv_main_unified.exe file hc "obj_dir/V${mod}_015_const.tree.json" "test_${mod}_out" 2>&1 | grep -E "(Success|Error|WARNING)" | head -3
    
    # Show the generated module
    echo "Generated Verilog:"
    cat "test_${mod}_out" | grep -A 20 "^module"
    echo "---"
done

echo ""
echo "All tests completed!"
