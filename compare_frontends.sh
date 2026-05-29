#!/bin/bash
# Compare VHDL and SystemVerilog frontends on the same module.
# Replaces direct calls to test_behavioral_optimization.exe and
# test_sv_behavioral.exe with a single sv_suite recipe that runs both
# frontends and prints register-inference results for each.

echo "══════════════════════════════════════════════════════════════════"
echo "  Comparing VHDL vs SystemVerilog Frontends"
echo "  Both using SHARED Behavioral IR infrastructure"
echo "══════════════════════════════════════════════════════════════════"
echo ""

VHDL=sysver_tests/slib_clock_div.vhd
SV=sysver_tests/slib_clock_div.sv

echo "Test Module: slib_clock_div"
echo "Expected Result: 2 registers (iCounter, iQ)"
echo ""

_build/default/sv_suite.exe script recipes/vhdl_sv_equiv.lua \
    "$VHDL" "$SV" 2>&1 | grep -A 2 "Register Inference"

echo ""
echo "══════════════════════════════════════════════════════════════════"
echo "Summary"
echo "══════════════════════════════════════════════════════════════════"
echo ""
echo "Both frontends share Behavioral IR + the same optimisation passes."
echo "Register-inference fixes apply to all languages simultaneously."
