#!/bin/bash

# Demo script for the interactive verification client
# This shows how to use the lua-ml embedded interpreter

echo "╔═══════════════════════════════════════════════════════════════╗"
echo "║  Interactive Verification Client Demo                         ║"
echo "╚═══════════════════════════════════════════════════════════════╝"
echo ""

# Build the client if needed
if [ ! -f "_build/default/interactive_client.exe" ]; then
    echo "Building interactive client..."
    dune build interactive_client.exe
    echo ""
fi

echo "Example 1: Show help"
echo "-------------------"
echo 'help()' | ./_build/default/interactive_client.exe
echo ""

echo "Example 2: Run VHDL regression test"
echo "------------------------------------"
echo 'vhdl_regression("sysver_tests/slib_clock_div.vhd")' | ./_build/default/interactive_client.exe | grep -A 10 "VHDL Regression"
echo ""

echo "Example 3: Run HardCaml SAT verification"
echo "-----------------------------------------"
echo 'hardcaml_sat("sysver_tests/slib_clock_div.vhd", "sysver_tests/slib_clock_div.sv")' | ./_build/default/interactive_client.exe | grep -E "(HardCaml SAT|Interface Match|PROVEN)"
echo ""

echo "Example 4: Interactive mode"
echo "---------------------------"
echo "To use interactively, run:"
echo "  ./_build/default/interactive_client.exe"
echo ""
echo "Then you can type commands like:"
echo '  > help()'
echo '  > vhdl_regression("sysver_tests/slib_clock_div.vhd")'
echo '  > hardcaml_sat("sysver_tests/slib_clock_div.vhd", "sysver_tests/slib_clock_div.sv")'
echo '  > verify_all("sysver_tests/slib_clock_div.vhd", "sysver_tests/slib_clock_div.sv")'
echo ""
