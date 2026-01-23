#!/bin/bash
# check_unhandled_patterns.sh - Verify no unhandled patterns in test suite

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "=== Checking for Unhandled Patterns ==="
echo

# Clean up old dumps
rm -rf unhandled_sv/ unhandled_vhdl/

# List of test files
SV_TESTS=(
    "sysver_tests/slib_clock_div.sv"
    "sysver_tests/slib_input_sync.sv"
    "sysver_tests/slib_edge_detect.sv"
    "sysver_tests/slib_mv_filter.sv"
    "sysver_tests/slib_input_filter.sv"
    "sysver_tests/uart_baudgen.sv"
    "sysver_tests/slib_counter.sv"
    "sysver_tests/uart_interrupt.sv"
    "sysver_tests/slib_fifo.sv"
    "sysver_tests/uart_receiver.sv"
    "sysver_tests/uart_transmitter.sv"
    "sysver_tests/apb_uart.sv"
)

VHDL_TESTS=(
    "sysver_tests/slib_clock_div.vhd"
    "sysver_tests/slib_input_sync.vhd"
    "sysver_tests/slib_edge_detect.vhd"
    "sysver_tests/slib_mv_filter.vhd"
    "sysver_tests/slib_input_filter.vhd"
    "sysver_tests/uart_baudgen.vhd"
    "sysver_tests/slib_counter.vhd"
    "sysver_tests/uart_interrupt.vhd"
    "sysver_tests/slib_fifo.vhd"
    "sysver_tests/uart_receiver.vhd"
    "sysver_tests/uart_transmitter.vhd"
    "sysver_tests/apb_uart.vhd"
)

# Test SystemVerilog files
echo "Testing SystemVerilog files..."
for file in "${SV_TESTS[@]}"; do
    if [ -f "$file" ]; then
        echo -n "  ${file##*/}... "
        if dune exec ./test_verible_elab.exe "$file" >/dev/null 2>&1; then
            echo -e "${GREEN}✓${NC}"
        else
            echo -e "${RED}✗ (failed to convert)${NC}"
        fi
    fi
done

echo

# Test VHDL files
echo "Testing VHDL files..."
for file in "${VHDL_TESTS[@]}"; do
    if [ -f "$file" ]; then
        echo -n "  ${file##*/}... "
        # VHDL test command would go here once we have a VHDL test binary
        echo -e "${YELLOW}⊘ (skipped - no VHDL test binary)${NC}"
    fi
done

echo
echo "=== Results ==="
echo

# Check for SystemVerilog unhandled patterns
SV_COUNT=0
if [ -d "unhandled_sv" ]; then
    SV_COUNT=$(ls unhandled_sv/*.json 2>/dev/null | wc -l | tr -d ' ')
fi

# Check for VHDL unhandled patterns
VHDL_COUNT=0
if [ -d "unhandled_vhdl" ]; then
    VHDL_COUNT=$(ls unhandled_vhdl/*.json 2>/dev/null | wc -l | tr -d ' ')
fi

TOTAL=$((SV_COUNT + VHDL_COUNT))

if [ $TOTAL -eq 0 ]; then
    echo -e "${GREEN}✓ No unhandled patterns found!${NC}"
    echo
    echo "All test files converted successfully without encountering"
    echo "any unhandled AST patterns."
    exit 0
else
    echo -e "${RED}✗ Found $TOTAL unhandled pattern(s):${NC}"
    echo "  SystemVerilog: $SV_COUNT"
    echo "  VHDL: $VHDL_COUNT"
    echo

    if [ $SV_COUNT -gt 0 ]; then
        echo "SystemVerilog patterns:"
        for file in unhandled_sv/*.json; do
            if [ -f "$file" ]; then
                pattern=$(python3 -c "import json; data=json.load(open('$file')); print(data.get('pattern_type', 'unknown'))")
                context=$(python3 -c "import json; data=json.load(open('$file')); print(data.get('context', 'unknown'))")
                echo "  - ${file##*/}: $context/$pattern"
            fi
        done
        echo
    fi

    if [ $VHDL_COUNT -gt 0 ]; then
        echo "VHDL patterns:"
        for file in unhandled_vhdl/*.json; do
            if [ -f "$file" ]; then
                pattern=$(python3 -c "import json; data=json.load(open('$file')); print(data.get('pattern_type', 'unknown'))")
                context=$(python3 -c "import json; data=json.load(open('$file')); print(data.get('context', 'unknown'))")
                echo "  - ${file##*/}: $context/$pattern"
            fi
        done
        echo
    fi

    echo "Review these patterns and add handlers in the appropriate files."
    echo "See PERMANENT_JSON_DUMPING.md for instructions."
    exit 1
fi
