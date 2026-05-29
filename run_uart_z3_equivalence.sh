#!/bin/bash

# Z3 Equivalence Verification: VHDL ≡ SystemVerilog for UART Modules
# Uses formal methods to prove behavioral equivalence

echo "╔═══════════════════════════════════════════════════════════════╗"
echo "║  Z3 Equivalence Verification: VHDL ≡ SystemVerilog           ║"
echo "║  UART Module Suite                                            ║"
echo "╚═══════════════════════════════════════════════════════════════╝"
echo ""

# Module pairs to test
declare -a MODULES=(
    "slib_clock_div"
    "slib_counter"
    "slib_edge_detect"
    "slib_fifo"
    "slib_input_filter"
    "slib_input_sync"
    "slib_mv_filter"
    "uart_baudgen"
    "uart_interrupt"
    "uart_receiver"
    "uart_transmitter"
)

PASSED=0
FAILED=0
TOTAL=${#MODULES[@]}

# Results
PASS_LIST=()
FAIL_LIST=()

echo "Testing $TOTAL module pairs with Z3 formal verification"
echo ""

for module in "${MODULES[@]}"; do
    vhdl_file="sysver_tests/${module}.vhd"
    sv_file="sysver_tests/${module}.sv"

    echo "═══════════════════════════════════════════════════════════════"
    echo "Module: $module"
    echo "═══════════════════════════════════════════════════════════════"

    # Check files exist
    if [ ! -f "$vhdl_file" ]; then
        echo "  ❌ VHDL file not found: $vhdl_file"
        FAIL_LIST+=("$module (VHDL missing)")
        ((FAILED++))
        echo ""
        continue
    fi

    if [ ! -f "$sv_file" ]; then
        echo "  ❌ SV file not found: $sv_file"
        FAIL_LIST+=("$module (SV missing)")
        ((FAILED++))
        echo ""
        continue
    fi

    # Run Z3 equivalence verification
    if _build/default/sv_suite.exe script recipes/vhdl_sv_equiv.lua "$vhdl_file" "$sv_file" > "/tmp/z3_${module}.log" 2>&1; then
        echo "  ✓ EQUIVALENT - Z3 proof successful"
        PASS_LIST+=("$module")
        ((PASSED++))
    else
        echo "  ❌ FAILED - Equivalence check failed"
        echo "  Details:"
        tail -10 "/tmp/z3_${module}.log" | sed 's/^/    /'
        FAIL_LIST+=("$module")
        ((FAILED++))
    fi
    echo ""
done

# Summary
echo ""
echo "╔═══════════════════════════════════════════════════════════════╗"
echo "║  Z3 EQUIVALENCE VERIFICATION SUMMARY                          ║"
echo "╚═══════════════════════════════════════════════════════════════╝"
echo ""
echo "Results: $PASSED/$TOTAL proven equivalent"
echo ""

if [ $PASSED -gt 0 ]; then
    echo "✓ PROVEN EQUIVALENT ($PASSED):"
    for module in "${PASS_LIST[@]}"; do
        echo "  ✓ $module - VHDL ≡ SV (formally verified)"
    done
    echo ""
fi

if [ $FAILED -gt 0 ]; then
    echo "❌ VERIFICATION FAILED ($FAILED):"
    for module in "${FAIL_LIST[@]}"; do
        echo "  ❌ $module"
    done
    echo ""
    echo "Check logs in /tmp/z3_*.log for details"
    echo ""
fi

if [ $FAILED -eq 0 ]; then
    echo "═══════════════════════════════════════════════════════════════"
    echo "All modules proven equivalent! 🎉"
    echo "VHDL and SystemVerilog produce identical behavioral semantics"
    echo "═══════════════════════════════════════════════════════════════"
    exit 0
else
    echo "═══════════════════════════════════════════════════════════════"
    echo "Some equivalence proofs failed"
    echo "═══════════════════════════════════════════════════════════════"
    exit 1
fi
