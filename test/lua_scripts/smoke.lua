-- Smoke test for the sv_decompiler Lua API.
--
-- Exercises every callable in the `svd` module:
--   parse, pick, miter, liberty, expand, gate_miter, bir, name, items.
-- Run with:  sv_decompiler script test/lua_scripts/smoke.lua

print("=== svd Lua API smoke ===")

-- 1. Parse a small SV file with two frontends and miter them.
local p_v = svd.parse("verible", "and8", {"test/gate_miter/and8.sv"})
local p_s = svd.parse("slang",   "and8", {"test/gate_miter/and8.sv"})
print("verible handle:", p_v, " name:", svd.name(p_v))
print("slang   handle:", p_s, " name:", svd.name(p_s))

local m_v = svd.pick(p_v, "and8")
local m_s = svd.pick(p_s, "and8")
print("miter slang vs verible:", svd.miter(m_v, m_s))

-- 2. Liberty load + per-cell function expansion (gate-miter all in
--    one Lua call, default Liberty).
print("gate-miter and8:",
      svd.gate_miter("and8",
                     "test/gate_miter/and8.sv",
                     "test/gate_miter/and8_gate.v",
                     ""))           -- "" → default simcells.lib

-- 3. Same, with explicit Liberty handle for re-use across calls.
-- (lua-ml ships without the standard `os.getenv`; pass full path or
--  read from a Pipe/Sys helper if you wire one in.)
local lib = svd.liberty("/home/jonathan/hardcaml-lua.0.0.1/liberty/simcells.lib")
print("library handle:", lib, " name:", svd.name(lib))

-- 4. Item listing.
print("--- items ---")
print(svd.items())
