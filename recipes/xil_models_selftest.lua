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
print("== pass=" .. pass .. " fail=" .. fail .. " ==")
