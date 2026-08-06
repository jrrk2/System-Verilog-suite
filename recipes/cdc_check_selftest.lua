-- Selftest for the clock-domain BOUNDARY checker (Behavioral_cdc_check).
--
-- Four cuts in test/cdc/cdc_cases.sv, each isolating one construct the checker
-- must get right.  Two of them must be REJECTED: a checker that only ever
-- returns "safe" passes any calibration against a known-good boundary, which
-- is how the process-local version of this pass looked correct while seeing
-- nothing at all.
--
--   sv_suite script recipes/cdc_check_selftest.lua

-- global, not local: Lua 4 needs %upvalue syntax to see a chunk local
-- from inside a function, so plain globals are the least surprising choice.
F = { "test/cdc/cdc_cases.sv" }

pass = 0
fail = 0

function expect(top, macro, want)
  local p = svd.parse("verible", top, F)
  local got = svd.cdc_check(p, macro)
  if strfind(got, want, 1, 1) then
    print("PASS  " .. macro .. "  " .. got)
    pass = pass + 1
  else
    print("FAIL  " .. macro .. "  want '" .. want .. "'  got '" .. got .. "'")
    fail = fail + 1
  end
end

-- same-clock FF->FF both directions across the cut: both ports must be unsafe
expect("t_bad", "m_bad", "unsafe=2")
-- genuine CDC through a 2-flop synchroniser: nothing to report
expect("t_good", "m_good", "unsafe=0 unproven=0")
-- combinational feed-through, both flops outside: both ports unsafe
expect("t_thru", "m_thru", "unsafe=2")
-- the offending register lives in a CHILD of the macro
expect("t_deep", "m_deep", "unsafe=2")

print("== pass=" .. pass .. " fail=" .. fail .. " ==")
