#!/bin/bash
# test_unified.sh - Test script for unified decompiler

echo "SystemVerilog Decompiler - Unified Interface Test"
echo "=================================================="
echo

# Check if executable exists
if [ ! -f "_build/default/sv_main_unified.exe" ]; then
    echo "Building sv_main_unified..."
    dune build
fi

EXEC="_build/default/sv_main_unified.exe"

# Test 1: Show help
echo "Test 1: Display help"
echo "-------------------"
$EXEC
echo

# Test 2: Test with different backends (if obj_dir exists)
if [ -d "obj_dir" ]; then
    echo "Test 2: Scan with all backends"
    echo "-------------------------------"
    
    for backend in standard structural yosys hardcaml; do
        echo "Testing backend: $backend"
        mkdir -p "test_output_$backend"
        $EXEC scan $backend "test_output_$backend/" 2>&1 | head -20
        echo
    done
else
    echo "Test 2: Skipped (no obj_dir/ directory)"
    echo "To test scanning, create obj_dir/ with JSON files from Verilator"
    echo
fi

# Test 3: Single file processing (example only - won't run without actual file)
echo "Test 3: Single file example command"
echo "-----------------------------------"
echo "Command: $EXEC file hardcaml input.json output.ml"
echo "(This is an example - requires actual JSON file to run)"
echo

echo "Test complete!"
echo
echo "Usage examples:"
echo "  $EXEC scan yosys results/"
echo "  $EXEC file hardcaml design.json design.ml"
echo "  $EXEC scan standard output_sv/"
