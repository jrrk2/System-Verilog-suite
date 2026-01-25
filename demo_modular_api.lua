#!/usr/bin/env -S ./_build/default/interactive_client_v2.exe

print("╔═══════════════════════════════════════════════════════════════╗")
print("║  HDL Modular API Demo - Building Blocks Approach              ║")
print("╚═══════════════════════════════════════════════════════════════╝")
print("")

print("═══════════════════════════════════════════════════════════════")
print("Example 1: IR Statistics")
print("═══════════════════════════════════════════════════════════════")
print("")

dump.stats("sysver_tests/slib_clock_div.vhd")
print("")

print("═══════════════════════════════════════════════════════════════")
print("Example 2: Conversion Workflow")
print("═══════════════════════════════════════════════════════════════")
print("")

print("Converting VHDL to Behavioral IR:")
convert.vhdl_to_behavioral("sysver_tests/slib_clock_div.vhd")
print("")

print("Converting SystemVerilog to Behavioral IR:")
convert.sv_to_behavioral("sysver_tests/slib_clock_div.sv")
print("")

print("═══════════════════════════════════════════════════════════════")
print("Example 3: Optimization Comparison")
print("═══════════════════════════════════════════════════════════════")
print("")

print("Quick optimization (const prop + DCE):")
optimize.quick("sysver_tests/slib_edge_detect.vhd")
print("")

print("Full optimization (const prop + DCE + CSE):")
optimize.full("sysver_tests/slib_input_sync.vhd")
print("")

print("═══════════════════════════════════════════════════════════════")
print("Example 4: Verification")
print("═══════════════════════════════════════════════════════════════")
print("")

print("Verifying slib_clock_div with HardCaml SAT:")
result = verify.hardcaml_sat("sysver_tests/slib_clock_div.vhd", "sysver_tests/slib_clock_div.sv")
if result then
    print("✓ PASSED")
else
    print("✗ FAILED")
end
print("")

print("═══════════════════════════════════════════════════════════════")
print("Example 5: Custom Workflow")
print("═══════════════════════════════════════════════════════════════")
print("")

print("Custom workflow: Analyze → Optimize → Verify")
print("")

target_vhdl = "sysver_tests/slib_input_sync.vhd"
target_sv = "sysver_tests/slib_input_sync.sv"

print("Step 1: Analyze")
dump.stats(target_vhdl)
print("")

print("Step 2: Optimize")
optimize.quick(target_vhdl)
print("")

print("Step 3: Verify")
result2 = verify.hardcaml_sat(target_vhdl, target_sv)
if result2 then
    print("✓ Verification PASSED")
else
    print("✗ Verification FAILED")
end
print("")

print("═══════════════════════════════════════════════════════════════")
print("Demo complete!")
print("═══════════════════════════════════════════════════════════════")
print("")
print("Key takeaway: Building blocks can be combined in any workflow")
print("rather than having pre-packaged commands.")
print("")
