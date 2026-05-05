-- Timing-pass regression suite. Lua-ml subset — flat checks, no
-- closures over outer locals (lua-ml functions only capture args).

print("=== timing pass regressions ===")

pass = 0
fail = 0

function chk(label, report, needle)
  if strfind(report, needle) then
    print("  OK   " .. label)
    pass = pass + 1
  else
    print("  FAIL " .. label .. "  (expected " .. needle .. ")")
    fail = fail + 1
  end
end

-- 8-bit ripple add: arrival=8 (W levels)
p = svd.parse("verible", "add8", {"test/timing/add8.sv"})
m = svd.pick(p, "add8")
r = svd.timing(m, 0)
chk("add8 ripple arrival=8", r, "arrival=8")

r = svd.timing(m, 4)
chk("add8 target=4 upgrade hint", r, "kogge_stone")

-- chain of 3 ripple adds: 8+8+8=24
p = svd.parse("verible", "chain", {"test/timing/chain3.sv"})
m = svd.pick(p, "chain")
r = svd.timing(m, 0)
chk("chain3 arrival=24", r, "arrival=24")

-- mixed-arch chain
p = svd.parse("verible", "mixed", {"test/timing/mixed_arch.sv"})
m = svd.pick(p, "mixed")
r = svd.timing(m, 0)
chk("mixed arrival=16",          r, "arrival=16")
chk("mixed has kogge_stone tag", r, "arch=kogge_stone")
chk("mixed has ripple tag",      r, "arch=ripple")

print("-----  pass=" .. pass .. "  fail=" .. fail)
