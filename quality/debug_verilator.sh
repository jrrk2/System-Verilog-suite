#!/bin/bash
# debug_verilator.sh - Debug why Verilator fails to compile decompiled SV

RESULTS_DIR="results"
TEST_FILE=${1:-""}

echo "=========================================="
echo "Verilator Compilation Debugger"
echo "=========================================="
echo ""

if [ -n "$TEST_FILE" ]; then
    # Test single file
    echo "Testing: $TEST_FILE"
    echo ""
    echo "File contents:"
    echo "----------------------------------------"
    cat "$TEST_FILE"
    echo ""
    echo "----------------------------------------"
    echo ""
    echo "Verilator output:"
    verilator --lint-only -Wall "$TEST_FILE" 2>&1
else
    # Test all files
    echo "Testing all .sv files in $RESULTS_DIR/"
    echo ""
    
    for sv_file in "$RESULTS_DIR"/*.sv; do
        if [ -f "$sv_file" ]; then
            basename=$(basename "$sv_file")
            echo "=========================================="
            echo "File: $basename"
            echo "=========================================="
            
            # Show first 20 lines
            echo "First 20 lines:"
            head -20 "$sv_file"
            echo ""
            
            # Try to compile with Verilator
            echo "Verilator errors:"
            verilator --lint-only -Wall "$sv_file" 2>&1 | head -30
            echo ""
            echo ""
        fi
    done
fi

echo "=========================================="
echo "Common Issues to Check:"
echo "=========================================="
echo ""
echo "1. Missing module names in file vs module declaration"
echo "2. Incomplete port declarations"
echo "3. Missing semicolons"
echo "4. Incorrect SystemVerilog syntax"
echo "5. Missing type definitions"
echo "6. Interface/modport issues"
echo ""
echo "To test a specific file:"
echo "  $0 results/your_file.sv"
