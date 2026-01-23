#!/bin/bash

# Test all SystemVerilog files with HardCaml backend
# Usage: ./test_all.sh

results_file="test_results.txt"
> "$results_file"

echo "================================================"
echo "Testing SystemVerilog Files with HardCaml Backend"
echo "================================================"
echo ""

# Test files to process (sequential and combinational)
test_files=(
    "test_01_simple_dff.sv"
    "test_02_dff_async_reset_high.sv"
    "test_03_dff_async_reset_low.sv"
    "test_04_dff_sync_reset.sv"
    "test_05_dff_enable.sv"
    "test_06_counter.sv"
    "test_07_shift_register.sv"
    "test_08_fsm.sv"
    "test_13_negedge_clock.sv"
    "test_14_multi_reg.sv"
    "test_09_always_comb_simple.sv"
    "test_10_always_comb_mux.sv"
    "test_11_always_comb_case.sv"
    "test_12_always_star.sv"
    "test_15_priority_encoder.sv"
    "alu.sv"
    "continuous_assign.sv"
)

pass_count=0
fail_count=0

for file in "${test_files[@]}"; do
    echo "Testing: $file"
    echo "----------------------------------------"
    
    # Step 1: Parse with Verilator
    echo "  [1/3] Parsing with Verilator..."
    verilator --json-only --dump-tree-json --json-only-output "obj_dir/V${file%.sv}.tree.json" "sysver_tests/$file" 2>&1 > /dev/null
    if [ $? -ne 0 ]; then
        echo "  ✗ FAIL: Verilator parse failed"
        echo "$file: FAIL (parse)" >> "$results_file"
        ((fail_count++))
        echo ""
        continue
    fi
    
    # Step 2: Generate with HardCaml
    echo "  [2/3] Generating with HardCaml..."
    ./hard "sysver_tests/$file" 2>&1 > /dev/null
    if [ $? -ne 0 ]; then
        echo "  ✗ FAIL: HardCaml generation failed"
        echo "$file: FAIL (generate)" >> "$results_file"
        ((fail_count++))
        echo ""
        continue
    fi
    
    # Step 3: Lint generated Verilog
    echo "  [3/3] Linting generated Verilog..."
    output_file="results/decompile_V${file%.sv}.tree.json.sv"
    if [ ! -f "$output_file" ]; then
        echo "  ✗ FAIL: Output file not found"
        echo "$file: FAIL (no output)" >> "$results_file"
        ((fail_count++))
        echo ""
        continue
    fi
    
    verilator --lint-only "$output_file" 2>&1 > /dev/null
    if [ $? -eq 0 ]; then
        echo "  ✓ PASS"
        echo "$file: PASS" >> "$results_file"
        ((pass_count++))
    else
        echo "  ✗ FAIL: Verilator lint failed"
        echo "$file: FAIL (lint)" >> "$results_file"
        ((fail_count++))
    fi
    
    echo ""
done

echo "================================================"
echo "Summary"
echo "================================================"
echo "Total tests: $((pass_count + fail_count))"
echo "Passed: $pass_count"
echo "Failed: $fail_count"
echo ""
echo "Detailed results written to: $results_file"
