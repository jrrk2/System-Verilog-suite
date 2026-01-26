#!/bin/bash
# Full EDIF vs VHDL equivalence test flow (Linux only with Vivado)
#
# This script:
# 1. Synthesizes VHDL sources to EDIF using Vivado
# 2. Converts EDIF to Behavioral Verilog
# 3. Converts VHDL to Behavioral IR
# 4. Compares the two representations
# 5. Performs Z3 formal equivalence verification

set -e  # Exit on error

echo "═══════════════════════════════════════════════════════════════"
echo "  EDIF vs VHDL Equivalence Test Flow"
echo "═══════════════════════════════════════════════════════════════"
echo ""

# Check if running on Linux
if [[ "$OSTYPE" != "linux-gnu"* ]]; then
    echo "⚠ Warning: This script is designed for Linux with Vivado"
    echo "  Current OS: $OSTYPE"
    echo ""
fi

# Check for Vivado
if ! command -v vivado &> /dev/null; then
    echo "Error: Vivado not found in PATH"
    echo "  Please install Vivado and source the settings script:"
    echo "  source /path/to/Vivado/settings64.sh"
    exit 1
fi

echo "✓ Vivado found: $(which vivado)"
echo ""

# Check for VHDL sources
if [ ! -d "sysver_tests" ]; then
    echo "Error: sysver_tests directory not found"
    echo "  Please ensure VHDL source files are in sysver_tests/"
    exit 1
fi

echo "✓ VHDL sources found"
echo ""

# Step 1: Synthesize VHDL to EDIF using Vivado
echo "═══════════════════════════════════════════════════════════════"
echo "Step 1: Synthesizing VHDL to EDIF netlist..."
echo "═══════════════════════════════════════════════════════════════"
echo ""

if [ ! -f "synth_uart_to_edif.tcl" ]; then
    echo "Error: synth_uart_to_edif.tcl not found"
    exit 1
fi

echo "Running Vivado synthesis..."
vivado -mode tcl < synth_uart_to_edif.tcl 2>&1 | tee vivado_synth.log

if [ ! -f "uart_synthesized.edf" ]; then
    echo "Error: Synthesis failed - uart_synthesized.edf not created"
    exit 1
fi

echo ""
echo "✓ EDIF netlist created: uart_synthesized.edf"
echo ""

# Step 2: Convert EDIF to Behavioral Verilog
echo "═══════════════════════════════════════════════════════════════"
echo "Step 2: Converting EDIF to Behavioral Verilog..."
echo "═══════════════════════════════════════════════════════════════"
echo ""

if [ ! -f "_build/default/edif_to_verilog.exe" ]; then
    echo "Building EDIF converter..."
    dune build edif_to_verilog.exe
fi

echo "Running EDIF to Verilog converter..."
_build/default/edif_to_verilog.exe uart_synthesized.edf apb_uart_from_edif.v

if [ ! -f "apb_uart_from_edif.v" ]; then
    echo "Error: EDIF conversion failed"
    exit 1
fi

echo ""
echo "✓ Behavioral Verilog created: apb_uart_from_edif.v"
echo ""

# Step 3: Run equivalence checker with Z3 formal verification
echo "═══════════════════════════════════════════════════════════════"
echo "Step 3: Running equivalence checker with Z3 verification..."
echo "═══════════════════════════════════════════════════════════════"
echo ""

echo "Building equivalence checker..."
dune build test_edif_vhdl_equivalence.exe

echo "Running equivalence checker..."
_build/default/test_edif_vhdl_equivalence.exe

# Capture exit code
TEST_RESULT=$?

echo ""
echo "═══════════════════════════════════════════════════════════════"
echo "  Test Flow Complete"
echo "═══════════════════════════════════════════════════════════════"
echo ""
echo "Generated files:"
echo "  - uart_synthesized.edf         : EDIF netlist from Vivado"
echo "  - apb_uart_from_edif.v         : Behavioral Verilog from EDIF"
echo "  - uart_post_synth.dcp          : Vivado checkpoint"
echo "  - vivado_synth.log             : Vivado synthesis log"
echo ""
echo "Verification performed:"
echo "  ✓ Structural comparison (ports, instances)"
echo "  ✓ Z3 formal equivalence verification (miter circuit + SAT)"
echo ""

if [ $TEST_RESULT -eq 0 ]; then
    echo "🎉 EQUIVALENCE VERIFIED - Designs are formally equivalent!"
    echo ""
    exit 0
else
    echo "❌ VERIFICATION FAILED - Designs differ or incomplete"
    echo ""
    exit 1
fi
