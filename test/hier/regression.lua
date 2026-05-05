-- Hierarchical-encode regression suite (#79).

print("=== boundary-preserving miter regressions ===")

pass = 0
fail = 0

function check(label, top, file_a, file_b, want)
  local pa = svd.parse("verible", top, {file_a})
  local pb = svd.parse("verible", top, {file_b})
  local ma = svd.pick(pa, top)
  local mb = svd.pick(pb, top)
  local got = svd.miter(ma, mb)
  if got == want then
    print("  OK   " .. label .. "  " .. got)
    pass = pass + 1
  else
    print("  FAIL " .. label .. "  got=" .. got .. " want=" .. want)
    fail = fail + 1
  end
end

-- 1-level hierarchy
check("add: flat ↔ flat",  "add_top",
      "test/hier/add_top_flat.sv", "test/hier/add_top_flat.sv",
      "EQUIVALENT")
check("add: flat ↔ hier",  "add_top",
      "test/hier/add_top_flat.sv", "test/hier/add_top_hier.sv",
      "EQUIVALENT")
check("add: hier ↔ hier",  "add_top",
      "test/hier/add_top_hier.sv", "test/hier/add_top_hier.sv",
      "EQUIVALENT")
-- 2-level hierarchy
check("add: flat ↔ deep",  "add_top",
      "test/hier/add_top_flat.sv", "test/hier/add_top_deep.sv",
      "EQUIVALENT")
-- sequential
check("dff: flat ↔ hier",  "dff_top",
      "test/hier/dff_flat.sv", "test/hier/dff_hier.sv",
      "EQUIVALENT")

print("-----  pass=" .. pass .. "  fail=" .. fail)
