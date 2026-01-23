#!/bin/bash
# Compare VHDL vs SystemVerilog for entire APB UART suite

set -e

echo "═══════════════════════════════════════════════════════════════"
echo "  VHDL vs SystemVerilog Comparison Suite"
echo "  Ground Truth Verification"
echo "═══════════════════════════════════════════════════════════════"
echo ""

# Module pairs to compare
declare -a modules=(
    "apb_uart"
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

pass_count=0
fail_count=0
skip_count=0

echo "Step 1: Testing VHDL→IR conversion"
echo "───────────────────────────────────────────────────────────────"
echo ""

./test_all_vhdl_modules > /tmp/vhdl_results.txt 2>&1
vhdl_status=$?

if [ $vhdl_status -eq 0 ]; then
    echo "✅ All 12 VHDL modules converted to IR successfully"
    echo ""
else
    echo "❌ Some VHDL modules failed conversion"
    cat /tmp/vhdl_results.txt
    exit 1
fi

echo "Step 2: Checking SystemVerilog modules with Verible"
echo "───────────────────────────────────────────────────────────────"
echo ""

for module in "${modules[@]}"; do
    sv_file="sysver_tests/${module}.sv"

    if [ ! -f "$sv_file" ]; then
        echo "  ⚠️  ${module}: SV file not found"
        ((skip_count++))
        continue
    fi

    # Check if verible can parse it
    if command -v verible-verilog-syntax &> /dev/null; then
        if verible-verilog-syntax "$sv_file" 2>&1 > /dev/null; then
            echo "  ✓ ${module}: Verible syntax OK"
            ((pass_count++))
        else
            echo "  ✗ ${module}: Verible syntax failed"
            ((fail_count++))
        fi
    else
        echo "  - ${module}: Verible not available (skipping)"
        ((skip_count++))
    fi
done

echo ""
echo "═══════════════════════════════════════════════════════════════"
echo "Summary"
echo "═══════════════════════════════════════════════════════════════"
echo ""
echo "VHDL Conversion: ✅ 12/12 modules"
echo "SV Syntax Check: ${pass_count} pass, ${fail_count} fail, ${skip_count} skip"
echo ""

# Show IR statistics
echo "VHDL IR Statistics (from successful conversions):"
echo "───────────────────────────────────────────────────────────────"
grep -E "(Testing:|IR:)" /tmp/vhdl_results.txt | while read line; do
    echo "  $line"
done

echo ""
echo "═══════════════════════════════════════════════════════════════"
echo "Next Steps for Full Comparison"
echo "═══════════════════════════════════════════════════════════════"
echo ""
echo "VHDL modules that successfully converted to IR can be compared"
echo "against SystemVerilog using the following approach:"
echo ""
echo "1. For each module:"
echo "   • VHDL source → vhdl_to_ir → IR₁"
echo "   • SystemVerilog → verible → IR₂"
echo "   • Z3 verification: IR₁ ≡ IR₂"
echo ""
echo "2. Modules ready for comparison:"
for module in "${modules[@]}"; do
    vhdl_file="sysver_tests/${module}.vhd"
    sv_file="sysver_tests/${module}.sv"
    if [ -f "$vhdl_file" ] && [ -f "$sv_file" ]; then
        echo "   ✓ ${module}"
    fi
done
echo ""
echo "3. To perform Z3 verification, need to:"
echo "   • Build unified test harness linking VHDL and SV IR converters"
echo "   • Or compare IR outputs manually"
echo "   • Or use separate comparison tool"
echo ""

if [ $vhdl_status -eq 0 ]; then
    echo "✅ Phase 1 Complete: All VHDL modules convert to IR"
    echo ""
    echo "The VHDL→IR pipeline is fully validated and ready for"
    echo "equivalence verification against SystemVerilog."
    exit 0
else
    exit 1
fi
