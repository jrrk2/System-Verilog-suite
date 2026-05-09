-- hdl_convert_demo: exercise the new svd Lua API end-to-end.
--
-- Run with:
--   sv_decompiler script test/lua/hdl_convert_demo.lua
--
-- Demonstrates:
--   1. parse VHDL via vhdl frontend → BIR program handle
--   2. emit Verilog and VHDL from the same handle
--   3. parse Verilog via verible frontend → BIR
--   4. emit VHDL from it
--   5. high-level convert_hdl with header preservation
--   6. miter two BIR modules

local serializer = "/home/jonathan/grlib_corpus/grlib-gpl-2025.2-b4298/designs/leon3-altera-ep3c25-eek/serializer.vhd"
local counter_sv = "/tmp/c1.sv"

print("[1] parse VHDL serializer")
local p_vhdl = svd.parse("vhdl", "serializer", {serializer})
print("    handle = " .. p_vhdl)
print("    name   = " .. svd.name(p_vhdl))

print("[2] emit Verilog from VHDL-derived BIR")
local v_text = svd.emit_verilog(p_vhdl)
print("    Verilog body generated (returned as string)")

print("[3] write Verilog file")
svd.write_verilog(p_vhdl, "/tmp/serializer_from_lua.v")
print("    wrote /tmp/serializer_from_lua.v")

print("[4] parse SV counter via verible")
local p_sv = svd.parse("verible", "counter", {counter_sv})
print("    handle = " .. p_sv)

print("[5] emit VHDL from Verilog-derived BIR")
svd.write_vhdl(p_sv, "/tmp/counter_from_lua.vhd")
print("    wrote /tmp/counter_from_lua.vhd")

print("[6] convert_hdl pipeline (license header preserved)")
local out = svd.convert_hdl(serializer, "/tmp/serializer_lua_pipeline.v")
print("    convert_hdl returned: " .. out)

print("[7] miter the VHDL→BIR vs verible→BIR of the SAME module")
-- a self-equivalence sanity check on counter
local p_a = svd.parse("verible", "counter", {counter_sv})
local p_b = svd.parse("verible", "counter", {counter_sv})
local m_a = svd.pick(p_a, "counter")
local m_b = svd.pick(p_b, "counter")
print("    miter(counter,counter) = " .. svd.miter(m_a, m_b))

print("[8] items in handle table")
print(svd.items())
