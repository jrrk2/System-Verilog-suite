#!/bin/bash

# VHDL UART Regression Test Runner

echo "╔═══════════════════════════════════════════════════════════════╗"
echo "║  VHDL UART Regression Test Suite                             ║"
echo "╚═══════════════════════════════════════════════════════════════╝"
echo ""

# List of VHDL files to test
VHDL_FILES=(
    "sysver_tests/slib_clock_div.vhd"
    "sysver_tests/slib_counter.vhd"
    "sysver_tests/slib_edge_detect.vhd"
    "sysver_tests/slib_fifo.vhd"
    "sysver_tests/slib_input_filter.vhd"
    "sysver_tests/slib_input_sync.vhd"
    "sysver_tests/slib_mv_filter.vhd"
    "sysver_tests/uart_baudgen.vhd"
    "sysver_tests/uart_interrupt.vhd"
    "sysver_tests/uart_receiver.vhd"
    "sysver_tests/uart_transmitter.vhd"
)

PASSED=0
FAILED=0
TOTAL=${#VHDL_FILES[@]}

# Results arrays
PASS_LIST=()
FAIL_LIST=()

echo "Testing $TOTAL VHDL modules"
echo ""

# Test each file
for vhdl_file in "${VHDL_FILES[@]}"; do
    module_name=$(basename "$vhdl_file" .vhd)

    echo "═══════════════════════════════════════════════════════════════"
    echo "Testing: $module_name"
    echo "═══════════════════════════════════════════════════════════════"

    if [ ! -f "$vhdl_file" ]; then
        echo "  ❌ File not found: $vhdl_file"
        FAIL_LIST+=("$module_name (file not found)")
        ((FAILED++))
        echo ""
        continue
    fi

    # Run the test
    if _build/default/sv_suite.exe script recipes/vhdl_sv_equiv.lua "$vhdl_file" > /tmp/vhdl_test_$module_name.log 2>&1; then
        echo "  ✓ PASS"
        PASS_LIST+=("$module_name")
        ((PASSED++))
    else
        echo "  ❌ FAIL"
        # Show last few lines of error
        echo "  Error details:"
        tail -5 /tmp/vhdl_test_$module_name.log | sed 's/^/    /'
        FAIL_LIST+=("$module_name")
        ((FAILED++))
    fi
    echo ""
done

# Print summary
echo ""
echo "╔═══════════════════════════════════════════════════════════════╗"
echo "║  TEST SUMMARY                                                 ║"
echo "╚═══════════════════════════════════════════════════════════════╝"
echo ""
echo "Results: $PASSED/$TOTAL passed"
echo ""

if [ $PASSED -gt 0 ]; then
    echo "✓ PASSED ($PASSED):"
    for module in "${PASS_LIST[@]}"; do
        echo "  ✓ $module"
    done
    echo ""
fi

if [ $FAILED -gt 0 ]; then
    echo "❌ FAILED ($FAILED):"
    for module in "${FAIL_LIST[@]}"; do
        echo "  ❌ $module"
    done
    echo ""
fi

if [ $FAILED -eq 0 ]; then
    echo "═══════════════════════════════════════════════════════════════"
    echo "All tests passed! 🎉"
    echo "═══════════════════════════════════════════════════════════════"
    exit 0
else
    echo "═══════════════════════════════════════════════════════════════"
    echo "Some tests failed. Check logs in /tmp/vhdl_test_*.log"
    echo "═══════════════════════════════════════════════════════════════"
    exit 1
fi
