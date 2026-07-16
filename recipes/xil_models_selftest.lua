-- recipes/xil_models_selftest.lua
-- Validate Xil_prim_models (no-Vivado FPGA primitive formal models) by
-- Z3-mitering a behavioral spec against a primitive-instance impl per cell
-- class.  Also exercises verible instance-parameter (INIT) capture.
pass = 0
fail = 0
function expect_equiv(spec_file, impl_file, top)
  local spec = svd.parse("verible",     top, {spec_file})
  local impl = svd.parse("verible-ext", top, {impl_file})
  local cov  = svd.xil_models_coverage(impl)
  impl = svd.augment_xil_models(impl)
  local v = svd.miter(svd.pick(spec, top), svd.pick(impl, top))
  if v == "EQUIVALENT" then pass = pass + 1 else fail = fail + 1 end
  print("  " .. top .. "  [" .. cov .. "]  ->  " .. v)
end
D = "recipes/xil_selftest/"
print("== xil_prim_models self-test ==")
expect_equiv(D .. "lut2_spec.v", D .. "lut2_impl.v", "top")
expect_equiv(D .. "lut6_spec.v", D .. "lut6_impl.v", "nor6")
expect_equiv(D .. "lut6_spec.v", D .. "lut6_impl.v", "and6")
expect_equiv(D .. "fdre_spec.v", D .. "fdre_impl.v", "top")
expect_equiv(D .. "fdse_spec.v", D .. "fdse_impl.v", "top")
-- SRL16E / SRLC32E: shift-register-LUT next-state + static address decode.
-- The spec names its register ff__sr so it aligns with the flattened impl's
-- internal <inst>__sr under Behavioral_ffrip's by-name state matching; the
-- impl ties the A pins to the same static address srl_infer emits.
expect_equiv(D .. "srl16a_spec.v", D .. "srl16a_impl.v", "top")  -- tap 15
expect_equiv(D .. "srl16b_spec.v", D .. "srl16b_impl.v", "top")  -- tap 6
expect_equiv(D .. "srlc32_spec.v", D .. "srlc32_impl.v", "top")  -- tap 31 + Q31

-- End-to-end: srl_infer(FF-chain) with the SRL model expanded must equal the
-- original FF-chain.  Golden renames its register to the flattened SRL name so
-- ffrip state matching aligns.
p = svd.parse("verible", "srl", {D .. "srl_e2e_src.v"})
p = svd.unroll(p); p = svd.inline(p); p = svd.iflift(p)
p = svd.blocking_subst(p); p = svd.meminfer(p); p = svd.memlower(p)
p = svd.srl_infer(p); p = svd.augment_xil_models(p)
gold = svd.parse("verible", "srl", {D .. "srl_e2e_gold.v"})
v = svd.miter(svd.pick(gold, "srl"), svd.pick(p, "srl"))
if v == "EQUIVALENT" then pass = pass + 1 else fail = fail + 1 end
print("  srl_infer end-to-end  ->  " .. v)

print("== pass=" .. pass .. " fail=" .. fail .. " ==")
