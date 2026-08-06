-- Selftest for the per-clock-domain flattener (Behavioral_domain_split).
--
--   sv_suite script recipes/domain_split_selftest.lua
--
-- svd.domain_split returns "" when it refuses the cut, so the expected result
-- of each case is either a handle (accepted) or the empty string (refused).
-- The refusal cases matter as much as the acceptance ones: a splitter that
-- accepts everything has not checked anything.

F = { "test/cdc/split_cases.sv" }
OUTDIR = "/tmp/domain_split"

pass = 0
fail = 0

function check(name, cond, detail)
  if cond then
    print("PASS  " .. name .. "  " .. detail)
    pass = pass + 1
  else
    print("FAIL  " .. name .. "  " .. detail)
    fail = fail + 1
  end
end

function do_split(top, want_ok, periods)
  local p = svd.parse("verible", top, F)
  -- Emit the UNSPLIT top from the same BIR, so the equivalence miter compares
  -- the split against the design as this pass received it.  Comparing against
  -- the .sv instead would fold every frontend difference into the result and
  -- stop telling us anything about the split.
  svd.write_verilog(svd.pick(p, top), OUTDIR .. "/" .. top .. "_gold.v")
  local h = svd.domain_split(p, top, periods or "")
  if want_ok then
    check(top, h ~= "" and h ~= nil, "cut accepted")
    return h
  else
    check(top, h == "" or h == nil, "cut refused, as it must be")
    return nil
  end
end

-- two domains, joined by a synchroniser
h = do_split("s_two", 1)
if h then
  local names = svd.module_names(h)
  check("s_two/modules", strfind(names, "s_two_clk_a", 1, 1) ~= nil
        and strfind(names, "s_two_clk_b", 1, 1) ~= nil, names)
  svd.write_verilog(svd.pick(h, "s_two"), OUTDIR .. "/s_two_split_top.v")
  svd.write_verilog(svd.pick(h, "s_two_clk_a"), OUTDIR .. "/s_two_clk_a.v")
  svd.write_verilog(svd.pick(h, "s_two_clk_b"), OUTDIR .. "/s_two_clk_b.v")
  print("wrote " .. OUTDIR .. "/s_two_*.v")
end

-- A cone feeding BOTH domains, with periods given: it must land in the faster
-- one (clk_a at 8 ns), not be replicated -- the cone has to fit 8 ns wherever
-- it lives, and only the fast module constrains it to that.
h2 = do_split("s_shared", 1, "clk_a=8.0,clk_b=40.0")
if h2 then
  svd.write_verilog(svd.pick(h2, "s_shared"), OUTDIR .. "/s_shared_split_top.v")
  svd.write_verilog(svd.pick(h2, "s_shared_clk_a"), OUTDIR .. "/s_shared_clk_a.v")
  svd.write_verilog(svd.pick(h2, "s_shared_clk_b"), OUTDIR .. "/s_shared_clk_b.v")
end

-- the same cone with no periods supplied: replicated into both, and said so
do_split("s_shared", 1)

-- a register written from two clocks: no module to put the driver in
do_split("s_multi", nil)

print("== pass=" .. pass .. " fail=" .. fail .. " ==")
