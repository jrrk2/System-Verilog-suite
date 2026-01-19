#!/bin/bash
# test_verilator_output.sh - Check what Verilator actually generates

TEST_FILE=${1:-"results/alu.sv"}

echo "Testing Verilator output for: $TEST_FILE"
echo ""

# Clean obj_dir
rm -rf obj_dir_test
mkdir -p obj_dir_test

# Run Verilator with different flags to see what it generates
echo "=========================================="
echo "Test 1: --dump-tree"
echo "=========================================="
verilator --dump-tree -Mdir obj_dir_test "$TEST_FILE" 2>&1 | head -20
echo ""
echo "Generated files:"
ls -lh obj_dir_test/ 2>/dev/null || echo "No files generated"
echo ""

# Clean for next test
rm -rf obj_dir_test
mkdir -p obj_dir_test

echo "=========================================="
echo "Test 2: --dump-tree-json (if supported)"
echo "=========================================="
verilator --dump-tree-json -Mdir obj_dir_test "$TEST_FILE" 2>&1 | head -20
echo ""
echo "Generated files:"
ls -lh obj_dir_test/ 2>/dev/null || echo "No files generated"
echo ""

# Clean for next test
rm -rf obj_dir_test
mkdir -p obj_dir_test

echo "=========================================="
echo "Test 3: --dump-tree with --json-only"
echo "=========================================="
verilator --dump-tree --json-only -Mdir obj_dir_test "$TEST_FILE" 2>&1 | head -20
echo ""
echo "Generated files:"
ls -lh obj_dir_test/ 2>/dev/null || echo "No files generated"
echo ""

# Show contents of any .tree or .json files
echo "=========================================="
echo "Checking for tree/json files:"
echo "=========================================="
find obj_dir_test -type f \( -name "*.tree" -o -name "*.json" -o -name "*tree*" \) -exec echo "Found: {}" \; -exec head -5 {} \; 2>/dev/null

echo ""
echo "=========================================="
echo "Verilator version:"
verilator --version

echo ""
echo "For your version, the correct flags are likely:"
verilator --help 2>&1 | grep -i "dump\|tree\|json" | head -10
