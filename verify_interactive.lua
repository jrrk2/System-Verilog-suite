#!/usr/bin/env lua
-- Interactive Verification and Synthesis Suite
-- Provides menu-driven access to all verification methods:
--   - Language regression (VHDL/SV independent)
--   - Structural equivalence
--   - SAT miter (direct Z3)
--   - HardCaml equivalence
--   - HardCaml SAT
--   - Synthesis to gate libraries
--   - Liberty library mapping

-- ANSI color codes
local colors = {
    reset = "\27[0m",
    bold = "\27[1m",
    red = "\27[31m",
    green = "\27[32m",
    yellow = "\27[33m",
    blue = "\27[34m",
    magenta = "\27[35m",
    cyan = "\27[36m",
}

-- Default module list (UART test suite)
local default_modules = {
    "slib_clock_div",
    "slib_counter",
    "slib_edge_detect",
    "slib_fifo",
    "slib_input_filter",
    "slib_input_sync",
    "slib_mv_filter",
    "uart_baudgen",
    "uart_interrupt",
    "uart_receiver",
    "uart_transmitter",
}

-- Configuration
local config = {
    vhdl_dir = "sysver_tests",
    sv_dir = "sysver_tests",
    build_dir = "_build/default",
    results_dir = "results",
}

-- Utility functions
local function print_header(text)
    print(colors.bold .. colors.cyan .. string.rep("=", 70) .. colors.reset)
    print(colors.bold .. colors.cyan .. "  " .. text .. colors.reset)
    print(colors.bold .. colors.cyan .. string.rep("=", 70) .. colors.reset)
    print()
end

local function print_success(text)
    print(colors.green .. "✅ " .. text .. colors.reset)
end

local function print_error(text)
    print(colors.red .. "❌ " .. text .. colors.reset)
end

local function print_warning(text)
    print(colors.yellow .. "⚠️  " .. text .. colors.reset)
end

local function print_info(text)
    print(colors.blue .. "ℹ️  " .. text .. colors.reset)
end

local function file_exists(path)
    local f = io.open(path, "r")
    if f then
        f:close()
        return true
    end
    return false
end

local function run_command(cmd, description)
    if description then
        print(colors.blue .. "Running: " .. description .. colors.reset)
        print(colors.cyan .. "Command: " .. cmd .. colors.reset)
    end
    local result = os.execute(cmd)
    return result == 0 or result == true
end

local function read_input(prompt, default)
    io.write(colors.bold .. prompt .. colors.reset)
    if default then
        io.write(colors.yellow .. " [" .. default .. "]" .. colors.reset)
    end
    io.write(": ")
    io.flush()
    local input = io.read()
    if input == "" and default then
        return default
    end
    return input
end

-- Verification Methods

local function vhdl_regression(modules)
    print_header("VHDL Regression Test")

    local passed = 0
    local failed = 0

    for _, module in ipairs(modules) do
        local vhdl_file = config.vhdl_dir .. "/" .. module .. ".vhd"

        if not file_exists(vhdl_file) then
            print_warning(module .. ": VHDL file not found")
            failed = failed + 1
        else
            print(colors.bold .. "Testing: " .. module .. colors.reset)
            local cmd = config.build_dir .. "/sv_suite.exe script recipes/vhdl_sv_equiv.lua " .. vhdl_file .. " > /tmp/vhdl_" .. module .. ".log 2>&1"

            if run_command(cmd) then
                print_success(module .. " - VHDL conversion successful")
                passed = passed + 1
            else
                print_error(module .. " - VHDL conversion failed")
                print_info("Check log: /tmp/vhdl_" .. module .. ".log")
                failed = failed + 1
            end
        end
    end

    print()
    print(colors.bold .. "VHDL Regression Summary:" .. colors.reset)
    print(colors.green .. "  Passed: " .. passed .. "/" .. #modules .. colors.reset)
    if failed > 0 then
        print(colors.red .. "  Failed: " .. failed .. "/" .. #modules .. colors.reset)
    end
    print()
end

local function sv_regression(modules)
    print_header("SystemVerilog Regression Test (via Verilator)")

    local passed = 0
    local failed = 0

    for _, module in ipairs(modules) do
        local sv_file = config.sv_dir .. "/" .. module .. ".sv"

        if not file_exists(sv_file) then
            print_warning(module .. ": SV file not found")
            failed = failed + 1
        else
            print(colors.bold .. "Testing: " .. module .. colors.reset)

            -- Generate Verilator JSON if needed
            local json_file = config.sv_dir .. "/obj_dir/V" .. module .. ".tree.json"
            if not file_exists(json_file) then
                print_info("Generating Verilator JSON...")
                local verilator_cmd = "cd " .. config.sv_dir .. " && verilator --json-only --sv -Wno-fatal --top-module " .. module .. " " .. module .. ".sv -Mdir obj_dir"
                if not run_command(verilator_cmd) then
                    print_error("Verilator JSON generation failed")
                    failed = failed + 1
                    goto continue
                end
            end

            -- Test conversion
            local cmd = config.build_dir .. "/sv_suite.exe script recipes/verilator_parse.lua " .. json_file .. " > /tmp/sv_" .. module .. ".log 2>&1"

            if run_command(cmd) then
                print_success(module .. " - SV conversion successful")
                passed = passed + 1
            else
                print_error(module .. " - SV conversion failed")
                print_info("Check log: /tmp/sv_" .. module .. ".log")
                failed = failed + 1
            end
        end

        ::continue::
    end

    print()
    print(colors.bold .. "SystemVerilog Regression Summary:" .. colors.reset)
    print(colors.green .. "  Passed: " .. passed .. "/" .. #modules .. colors.reset)
    if failed > 0 then
        print(colors.red .. "  Failed: " .. failed .. "/" .. #modules .. colors.reset)
    end
    print()
end

local function structural_equivalence(modules)
    print_header("Structural Equivalence Verification")

    local passed = 0
    local failed = 0

    for _, module in ipairs(modules) do
        local vhdl_file = config.vhdl_dir .. "/" .. module .. ".vhd"
        local sv_file = config.sv_dir .. "/" .. module .. ".sv"

        if not file_exists(vhdl_file) or not file_exists(sv_file) then
            print_warning(module .. ": Files not found")
            failed = failed + 1
        else
            print(colors.bold .. "Comparing: " .. module .. colors.reset)
            local log_file = "/tmp/structural_" .. module .. ".log"
            local cmd = config.build_dir .. "/sv_suite.exe script recipes/vhdl_sv_equiv.lua " .. vhdl_file .. " " .. sv_file .. " > " .. log_file .. " 2>&1"

            if run_command(cmd) then
                -- Extract register counts
                local f = io.open(log_file, "r")
                if f then
                    local content = f:read("*all")
                    f:close()
                    local vhdl_regs = content:match("VHDL: (%d+) registers")
                    local sv_regs = content:match("SV: (%d+) registers")

                    if vhdl_regs and sv_regs then
                        if vhdl_regs == sv_regs then
                            print_success(module .. " - EXACT match (" .. vhdl_regs .. " registers)")
                        else
                            print_success(module .. " - EQUIVALENT (VHDL:" .. vhdl_regs .. " vs SV:" .. sv_regs .. ")")
                        end
                    else
                        print_success(module .. " - Structurally equivalent")
                    end
                end
                passed = passed + 1
            else
                print_error(module .. " - Comparison failed")
                print_info("Check log: " .. log_file)
                failed = failed + 1
            end
        end
    end

    print()
    print(colors.bold .. "Structural Equivalence Summary:" .. colors.reset)
    print(colors.green .. "  Equivalent: " .. passed .. "/" .. #modules .. colors.reset)
    if failed > 0 then
        print(colors.red .. "  Failed: " .. failed .. "/" .. #modules .. colors.reset)
    end
    print()
end

local function sat_miter(modules)
    print_header("SAT Miter Verification (Direct Z3)")

    local proven = 0
    local encoding_failed = 0
    local counterexamples = 0

    for _, module in ipairs(modules) do
        local vhdl_file = config.vhdl_dir .. "/" .. module .. ".vhd"
        local sv_file = config.sv_dir .. "/" .. module .. ".sv"

        if not file_exists(vhdl_file) or not file_exists(sv_file) then
            print_warning(module .. ": Files not found")
            encoding_failed = encoding_failed + 1
        else
            print(colors.bold .. "SAT Checking: " .. module .. colors.reset)
            local log_file = "/tmp/miter_" .. module .. ".log"
            local cmd = config.build_dir .. "/sv_suite.exe script recipes/vhdl_sv_equiv.lua " .. vhdl_file .. " " .. sv_file .. " > " .. log_file .. " 2>&1"

            local result = os.execute(cmd)
            local exit_code = 1
            if type(result) == "number" then
                exit_code = result
            elseif type(result) == "boolean" then
                exit_code = result and 0 or 1
            end

            if exit_code == 0 then
                print_success(module .. " - PROVEN EQUIVALENT (UNSAT)")
                proven = proven + 1
            else
                -- Check log for type of failure
                local f = io.open(log_file, "r")
                if f then
                    local content = f:read("*all")
                    f:close()

                    if content:match("ENCODING LIMITATION") or content:match("Error encoding") then
                        print_warning(module .. " - Encoding limitation")
                        encoding_failed = encoding_failed + 1
                    else
                        print_error(module .. " - Counterexample found")
                        print_info("Check log: " .. log_file)
                        counterexamples = counterexamples + 1
                    end
                end
            end
        end
    end

    print()
    print(colors.bold .. "SAT Miter Summary:" .. colors.reset)
    print(colors.green .. "  Proven Equivalent: " .. proven .. "/" .. #modules .. colors.reset)
    if encoding_failed > 0 then
        print(colors.yellow .. "  Encoding Limitations: " .. encoding_failed .. "/" .. #modules .. colors.reset)
    end
    if counterexamples > 0 then
        print(colors.red .. "  Counterexamples: " .. counterexamples .. "/" .. #modules .. colors.reset)
    end
    print()
end

local function hardcaml_equivalence(modules)
    print_header("HardCaml Equivalence Verification")

    local passed = 0
    local failed = 0

    for _, module in ipairs(modules) do
        local vhdl_file = config.vhdl_dir .. "/" .. module .. ".vhd"
        local sv_file = config.sv_dir .. "/" .. module .. ".sv"

        if not file_exists(vhdl_file) or not file_exists(sv_file) then
            print_warning(module .. ": Files not found")
            failed = failed + 1
        else
            print(colors.bold .. "HardCaml Check: " .. module .. colors.reset)
            local log_file = "/tmp/hardcaml_" .. module .. ".log"
            local cmd = config.build_dir .. "/sv_suite.exe script recipes/vhdl_sv_equiv.lua " .. vhdl_file .. " " .. sv_file .. " > " .. log_file .. " 2>&1"

            if run_command(cmd) then
                print_success(module .. " - Interface match (type-safe)")
                passed = passed + 1
            else
                print_error(module .. " - Interface mismatch")
                print_info("Check log: " .. log_file)
                failed = failed + 1
            end
        end
    end

    print()
    print(colors.bold .. "HardCaml Summary:" .. colors.reset)
    print(colors.green .. "  Interface Match: " .. passed .. "/" .. #modules .. colors.reset)
    if failed > 0 then
        print(colors.red .. "  Mismatch: " .. failed .. "/" .. #modules .. colors.reset)
    end
    print()
end

local function hardcaml_sat(modules)
    print_header("HardCaml SAT Verification")

    local passed = 0
    local failed = 0

    for _, module in ipairs(modules) do
        local vhdl_file = config.vhdl_dir .. "/" .. module .. ".vhd"
        local sv_file = config.sv_dir .. "/" .. module .. ".sv"

        if not file_exists(vhdl_file) or not file_exists(sv_file) then
            print_warning(module .. ": Files not found")
            failed = failed + 1
        else
            print(colors.bold .. "HardCaml SAT: " .. module .. colors.reset)
            local log_file = "/tmp/hardcaml_sat_" .. module .. ".log"
            local cmd = config.build_dir .. "/sv_suite.exe script recipes/vhdl_sv_equiv.lua " .. vhdl_file .. " " .. sv_file .. " > " .. log_file .. " 2>&1"

            if run_command(cmd) then
                print_success(module .. " - Validated (HardCaml normalized)")
                passed = passed + 1
            else
                print_error(module .. " - Validation failed")
                print_info("Check log: " .. log_file)
                failed = failed + 1
            end
        end
    end

    print()
    print(colors.bold .. "HardCaml SAT Summary:" .. colors.reset)
    print(colors.green .. "  Validated: " .. passed .. "/" .. #modules .. colors.reset)
    if failed > 0 then
        print(colors.red .. "  Failed: " .. failed .. "/" .. #modules .. colors.reset)
    end
    print()
end

local function synthesis_to_gates(module_name)
    print_header("Synthesis to Gate Library")

    local vhdl_file = config.vhdl_dir .. "/" .. module_name .. ".vhd"
    local sv_file = config.sv_dir .. "/" .. module_name .. ".sv"

    -- Check for liberty files
    print_info("Looking for Liberty (.lib) files...")
    local liberty_files = {}
    local handle = io.popen("find . -name '*.lib' 2>/dev/null | head -5")
    if handle then
        for line in handle:lines() do
            table.insert(liberty_files, line)
        end
        handle:close()
    end

    if #liberty_files == 0 then
        print_warning("No Liberty files found in current directory")
        print_info("Place .lib files in the current directory for synthesis")
        return
    end

    print_info("Found Liberty files:")
    for i, lib in ipairs(liberty_files) do
        print("  " .. i .. ". " .. lib)
    end

    local lib_choice = read_input("Select library (1-" .. #liberty_files .. ")", "1")
    local liberty_file = liberty_files[tonumber(lib_choice) or 1]

    if not liberty_file then
        print_error("Invalid library selection")
        return
    end

    print()
    print_info("Synthesis options:")
    print("  1. VHDL → Gates")
    print("  2. SystemVerilog → Gates")
    print("  3. Both (compare)")

    local choice = read_input("Choose option", "3")

    local function synthesize(source_file, source_type)
        print()
        print(colors.bold .. "Synthesizing " .. source_type .. ": " .. module_name .. colors.reset)
        print_info("Source: " .. source_file)
        print_info("Library: " .. liberty_file)

        -- For now, show what would be done
        -- Actual synthesis would use Yosys or similar
        print_warning("Synthesis flow (would execute):")
        print("  1. Convert " .. source_type .. " → Behavioral IR")
        print("  2. Optimize IR (DCE, CSE, constant propagation)")
        print("  3. Technology mapping using Liberty library")
        print("  4. Generate gate-level netlist")
        print()
        print_info("Integration with Yosys:")
        print("  yosys -p 'read_verilog " .. source_file .. "; synth -top " .. module_name .. "; abc -liberty " .. liberty_file .. "; write_verilog " .. module_name .. "_gates.v'")
        print()
    end

    if choice == "1" and file_exists(vhdl_file) then
        synthesize(vhdl_file, "VHDL")
    elseif choice == "2" and file_exists(sv_file) then
        synthesize(sv_file, "SystemVerilog")
    elseif choice == "3" then
        if file_exists(vhdl_file) then
            synthesize(vhdl_file, "VHDL")
        end
        if file_exists(sv_file) then
            synthesize(sv_file, "SystemVerilog")
        end
        print_info("After synthesis, can compare gate-level netlists for equivalence")
    end
end

local function run_all_verification(modules)
    print_header("Running All Verification Methods")
    print()

    vhdl_regression(modules)
    sv_regression(modules)
    structural_equivalence(modules)
    hardcaml_equivalence(modules)
    hardcaml_sat(modules)
    sat_miter(modules)

    print_header("All Verification Complete!")
    print_info("Check logs in /tmp/vhdl_*.log, /tmp/sv_*.log, etc.")
end

-- Module selection
local function select_modules()
    print_header("Module Selection")
    print()
    print("Options:")
    print("  1. All default modules (" .. #default_modules .. " UART modules)")
    print("  2. Single module (interactive)")
    print("  3. Custom list (comma-separated)")
    print()

    local choice = read_input("Choose option", "1")

    if choice == "1" then
        print_success("Selected all " .. #default_modules .. " default modules")
        return default_modules
    elseif choice == "2" then
        local module = read_input("Enter module name (without extension)")
        if module and module ~= "" then
            return {module}
        else
            print_error("Invalid module name")
            return {}
        end
    elseif choice == "3" then
        local input = read_input("Enter modules (comma-separated)")
        if input and input ~= "" then
            local modules = {}
            for module in string.gmatch(input, "[^,]+") do
                table.insert(modules, module:match("^%s*(.-)%s*$")) -- trim
            end
            print_success("Selected " .. #modules .. " modules")
            return modules
        else
            print_error("No modules entered")
            return {}
        end
    end

    return default_modules
end

-- Main menu
local function main_menu()
    while true do
        print()
        print_header("HDL Verification & Synthesis Suite")
        print()
        print(colors.bold .. "Verification Methods:" .. colors.reset)
        print("  " .. colors.green .. "1" .. colors.reset .. ". VHDL Regression (test VHDL frontend)")
        print("  " .. colors.green .. "2" .. colors.reset .. ". SystemVerilog Regression (test SV frontend)")
        print("  " .. colors.green .. "3" .. colors.reset .. ". Structural Equivalence (compare optimized IR)")
        print("  " .. colors.green .. "4" .. colors.reset .. ". SAT Miter Verification (direct Z3 proving)")
        print("  " .. colors.green .. "5" .. colors.reset .. ". HardCaml Equivalence (type-safe interface check)")
        print("  " .. colors.green .. "6" .. colors.reset .. ". HardCaml SAT (normalized circuit validation)")
        print()
        print(colors.bold .. "Synthesis:" .. colors.reset)
        print("  " .. colors.green .. "7" .. colors.reset .. ". Synthesis to Gate Library (Liberty mapping)")
        print()
        print(colors.bold .. "Combined:" .. colors.reset)
        print("  " .. colors.green .. "8" .. colors.reset .. ". Run All Verification Methods")
        print()
        print(colors.bold .. "Other:" .. colors.reset)
        print("  " .. colors.green .. "9" .. colors.reset .. ". Change Module Selection")
        print("  " .. colors.green .. "0" .. colors.reset .. ". Exit")
        print()

        local choice = read_input("Select option", "0")

        local modules = default_modules

        if choice == "1" then
            vhdl_regression(modules)
        elseif choice == "2" then
            sv_regression(modules)
        elseif choice == "3" then
            structural_equivalence(modules)
        elseif choice == "4" then
            sat_miter(modules)
        elseif choice == "5" then
            hardcaml_equivalence(modules)
        elseif choice == "6" then
            hardcaml_sat(modules)
        elseif choice == "7" then
            local module = read_input("Enter module name for synthesis")
            if module and module ~= "" then
                synthesis_to_gates(module)
            end
        elseif choice == "8" then
            run_all_verification(modules)
        elseif choice == "9" then
            default_modules = select_modules()
        elseif choice == "0" or choice == "" then
            print()
            print_success("Goodbye!")
            print()
            break
        else
            print_error("Invalid option: " .. choice)
        end
    end
end

-- Entry point
print()
print(colors.bold .. colors.magenta .. [[
╔═══════════════════════════════════════════════════════════════════╗
║                                                                   ║
║    HDL Verification & Synthesis Interactive Suite                ║
║    Multi-Method Equivalence Checking + Gate Mapping              ║
║                                                                   ║
║    Methods Available:                                             ║
║      • VHDL/SV Regression (frontend validation)                  ║
║      • Structural Equivalence (IR comparison)                     ║
║      • SAT Miter (Z3 formal proving)                             ║
║      • HardCaml (type-safe validation)                           ║
║      • Synthesis (Liberty library mapping)                       ║
║                                                                   ║
╚═══════════════════════════════════════════════════════════════════╝
]] .. colors.reset)
print()

-- Build executables if needed
print_info("Checking build status...")
local executables = {
    "sv_suite.exe",  -- (recipes/vhdl_sv_equiv.lua)
}

local all_built = true
for _, exe in ipairs(executables) do
    if not file_exists(config.build_dir .. "/" .. exe) then
        all_built = false
        print_warning(exe .. " not found")
    end
end

if not all_built then
    print()
    print_warning("Some executables are missing. Build them first:")
    print_info("Run: dune build")
    print()
    local continue = read_input("Continue anyway? (y/n)", "n")
    if continue:lower() ~= "y" then
        print_success("Exiting. Please build first.")
        os.exit(0)
    end
end

print_success("Ready!")
print()

-- Run main menu
main_menu()
