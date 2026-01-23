#!/bin/bash
# Comprehensive test suite for all HardCaml test files

set -e

TESTDIR="sysver_tests/hardcaml_tests"
OUTDIR="test_results/hardcaml"
mkdir -p "$OUTDIR"

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Counters
TOTAL=0
PASSED=0
FAILED=0
SKIPPED=0

# Arrays to track results
declare -a PASSED_TESTS
declare -a FAILED_TESTS
declare -a SKIPPED_TESTS

echo "=========================================="
echo "HardCaml Test Suite"
echo "=========================================="
echo ""

# Process each .sv file
for svfile in "$TESTDIR"/*.sv; do
    basename=$(basename "$svfile" .sv)
    TOTAL=$((TOTAL + 1))
    
    # Skip testbench files
    if [[ "$basename" == *"_tb" ]]; then
        echo -e "${YELLOW}SKIP${NC} $basename (testbench)"
        SKIPPED=$((SKIPPED + 1))
        SKIPPED_TESTS+=("$basename")
        continue
    fi
    
    # Run verilator to generate JSON
    if ! verilator --lint-only --dump-tree-json -Wno-fatal "$svfile" > /dev/null 2>&1; then
        echo -e "${RED}FAIL${NC} $basename (verilator failed)"
        FAILED=$((FAILED + 1))
        FAILED_TESTS+=("$basename (verilator)")
        continue
    fi
    
    # Find the generated JSON file (use 015_const stage which is early and clean)
    jsonfile=""
    for stage in "015_const" "019_const" "024_begin" "032_const"; do
        candidate="obj_dir/V${basename}_${stage}.tree.json"
        if [ -f "$candidate" ]; then
            jsonfile="$candidate"
            break
        fi
    done
    
    if [ -z "$jsonfile" ]; then
        echo -e "${YELLOW}SKIP${NC} $basename (no suitable JSON stage found)"
        SKIPPED=$((SKIPPED + 1))
        SKIPPED_TESTS+=("$basename")
        continue
    fi
    
    # Run HardCaml backend
    outfile="$OUTDIR/${basename}_out.sv"
    if ./_build/default/sv_main_unified.exe file hc "$jsonfile" "$outfile" > /dev/null 2>&1; then
        # Check if output was generated
        if [ -f "$outfile" ] && [ -s "$outfile" ]; then
            echo -e "${GREEN}PASS${NC} $basename"
            PASSED=$((PASSED + 1))
            PASSED_TESTS+=("$basename")
        else
            echo -e "${RED}FAIL${NC} $basename (empty output)"
            FAILED=$((FAILED + 1))
            FAILED_TESTS+=("$basename (empty)")
        fi
    else
        echo -e "${RED}FAIL${NC} $basename (backend error)"
        FAILED=$((FAILED + 1))
        FAILED_TESTS+=("$basename (backend)")
    fi
done

echo ""
echo "=========================================="
echo "Test Summary"
echo "=========================================="
echo "Total:   $TOTAL"
echo -e "${GREEN}Passed:  $PASSED${NC}"
echo -e "${RED}Failed:  $FAILED${NC}"
echo -e "${YELLOW}Skipped: $SKIPPED${NC}"
echo ""
echo "Success Rate: $(awk "BEGIN {printf \"%.1f%%\", ($PASSED/($TOTAL-$SKIPPED))*100}")"

# Show details if there are failures
if [ $FAILED -gt 0 ]; then
    echo ""
    echo "Failed tests:"
    for test in "${FAILED_TESTS[@]}"; do
        echo "  - $test"
    done
fi

# Write detailed report
REPORT="$OUTDIR/test_report.txt"
{
    echo "HardCaml Test Suite Report"
    echo "Generated: $(date)"
    echo "=========================================="
    echo ""
    echo "Summary:"
    echo "  Total:   $TOTAL"
    echo "  Passed:  $PASSED"
    echo "  Failed:  $FAILED"
    echo "  Skipped: $SKIPPED"
    echo ""
    echo "Passed Tests:"
    for test in "${PASSED_TESTS[@]}"; do
        echo "  ✓ $test"
    done
    echo ""
    echo "Failed Tests:"
    for test in "${FAILED_TESTS[@]}"; do
        echo "  ✗ $test"
    done
    echo ""
    echo "Skipped Tests:"
    for test in "${SKIPPED_TESTS[@]}"; do
        echo "  - $test"
    done
} > "$REPORT"

echo ""
echo "Detailed report written to: $REPORT"

# Exit with error if any tests failed
if [ $FAILED -gt 0 ]; then
    exit 1
fi
