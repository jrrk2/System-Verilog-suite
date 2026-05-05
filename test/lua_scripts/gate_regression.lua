-- Gate-level miter regression suite via the Lua API.
-- Drives the same cases that test/gate_miter/run_regressions.sh runs.

print("=== gate-level Lua regressions ===")

LIB = "/home/jonathan/hardcaml-lua.0.0.1/liberty/simcells.lib"
lib_h = svd.liberty(LIB)
print("loaded " .. svd.name(lib_h))

pass = 0
fail = 0

function check(top, beh, gate, want)
  local got = svd.gate_miter(top, beh, gate, LIB)
  if got == want then
    print("  OK   " .. top .. "  " .. got)
    pass = pass + 1
  else
    print("  FAIL " .. top .. "  got=" .. got .. " want=" .. want)
    fail = fail + 1
  end
end

check("and2",       "test/gate_miter/and2_beh.sv",    "test/gate_miter/and2_gate.sv",      "EQUIVALENT")
check("full_adder", "test/gate_miter/full_adder.sv",  "test/gate_miter/full_adder_gate.v", "EQUIVALENT")
check("and8",       "test/gate_miter/and8.sv",        "test/gate_miter/and8_gate.v",       "EQUIVALENT")
check("dff1",       "test/gate_miter/dff1.sv",        "test/gate_miter/dff1_gate.v",       "EQUIVALENT")
-- counter: was expected-fail until #74 (Behavioral_ffpack) re-packed
-- the four 1-bit bit-blasted FFs into a single bus-level BSequential.
check("counter",    "test/gate_miter/counter.sv",     "test/gate_miter/counter_gate.v",    "EQUIVALENT")

print("-----  pass=" .. pass .. "  fail=" .. fail)
