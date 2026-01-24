#!/usr/bin/env -S ./_build/default/interactive_client.exe

-- HDL Verification Demo Using Lua 2.5 Syntax
-- This demonstrates full Lua capabilities in the interactive client

print("╔═══════════════════════════════════════════════════════════════╗")
print("║  HDL Verification Demo - Full Lua 2.5 Syntax                  ║")
print("╚═══════════════════════════════════════════════════════════════╝")
print("")

-- Define test modules as a Lua table
modules = {
    "slib_clock_div",
    "slib_input_sync",
    "slib_edge_detect"
}

-- Helper function to build file paths
function verify_module(module_name)
    vhdl_path = "sysver_tests/"
    sv_path = "sysver_tests/"
    vhdl_ext = ".vhd"
    sv_ext = ".sv"

    vhdl_file = vhdl_path .. module_name .. vhdl_ext
    sv_file = sv_path .. module_name .. sv_ext

    return verify.hardcaml_sat(vhdl_file, sv_file)
end

-- Iterate through modules using Lua 2.5 style
print("Testing modules...")
print("")

i = 1
while modules[i] do
    m = modules[i]

    print("═══════════════════════════════════════════════════════════════")
    print("Testing module:")
    print(m)
    print("═══════════════════════════════════════════════════════════════")

    -- Run verification
    result = verify_module(m)

    -- Print result
    if result then
        print("✓ PASSED")
    else
        print("✗ FAILED")
    end

    print("")
    i = i + 1
end

-- Print summary
print("═══════════════════════════════════════════════════════════════")
print("All module tests complete!")
print("═══════════════════════════════════════════════════════════════")
print("")
print("Demo complete!")
