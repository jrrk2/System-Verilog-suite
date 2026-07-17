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

-- ── Synthesis-path regressions ────────────────────────────────────────────
-- Wide-constant bit-exactness through behavioral const-prop: a 128-bit literal
-- must survive the pipeline + optimize with NO Z.to_int overflow and NO
-- truncation.  Guards behavioral_const.expr_to_const (Z.Overflow -> CUnknown) and
-- proves the constant is preserved exactly (a truncated literal would make the
-- optimized side's XOR disagree on the high bits -> DIFFER).  optimize is the
-- pass that crashed before the fix.
spec = svd.parse("verible", "widek", {D .. "widek.sv"})
o = svd.parse("verible", "widek", {D .. "widek.sv"})
o = svd.unroll(o); o = svd.inline(o); o = svd.iflift(o)
o = svd.blocking_subst(o); o = svd.meminfer(o); o = svd.memlower(o)
o = svd.optimize(o)
v = svd.miter(svd.pick(spec, "widek"), svd.pick(o, "widek"))
if v == "EQUIVALENT" then pass = pass + 1 else fail = fail + 1 end
print("  wide-const 128b: behavioral optimize bit-exact  ->  " .. v)

-- Wide gate_map crash-guards: a 128-bit const XOR and a 96-bit register with a
-- wide init must gate_map without Z.Overflow — exercises behavioral_to_hardcaml
-- signal_of_z (wide BConst) and the initial_values Z.t path.  The write also
-- exercises write_cellmapped_v's completeness guard on the positive (fully
-- mapped) path.  Reaching the print => no crash / no false abort.
p = svd.parse("verible", "widek", {D .. "widek.sv"})
p = svd.unroll(p); p = svd.inline(p); p = svd.iflift(p)
p = svd.blocking_subst(p); p = svd.meminfer(p); p = svd.memlower(p)
svd.write_cellmapped_v(svd.gate_map(svd.pick(p, "widek"), 6, 0), "/tmp/widek_cells.v")
p = svd.parse("verible", "wreg", {D .. "wreg.sv"})
p = svd.unroll(p); p = svd.inline(p); p = svd.iflift(p)
p = svd.blocking_subst(p); p = svd.meminfer(p); p = svd.memlower(p)
svd.write_cellmapped_v(svd.gate_map(svd.pick(p, "wreg"), 6, 0), "/tmp/wreg_cells.v")
pass = pass + 1
print("  wide gate_map 128b/96b + cellmapped write  ->  OK (no overflow)")

-- Vector-port bitbus: a multi-bit input/output design must be EQUIVALENT through
-- gate_map -> mapped_to_prog -> miter.  Guards prep_for_z3.resolve_input_bitbus
-- (input `a__i` -> a[i]) and the output-buffer reconstruction (y <- {obuf_y_i__O}).
-- Before the fix the per-bit input nets and the whole output bus floated free and
-- ANY vector design DIFFERed spuriously.
spec = svd.parse("verible", "widek", {D .. "widek.sv"})
p = svd.parse("verible", "widek", {D .. "widek.sv"})
p = svd.unroll(p); p = svd.inline(p); p = svd.iflift(p)
p = svd.blocking_subst(p); p = svd.meminfer(p); p = svd.memlower(p)
impl = svd.augment_xil_models(svd.mapped_to_prog(svd.gate_map(svd.pick(p, "widek"), 6, 0)))
v = svd.miter(svd.pick(spec, "widek"), svd.pick(impl, "widek"))
if v == "EQUIVALENT" then pass = pass + 1 else fail = fail + 1 end
print("  vector bitbus 128b: behavioral==gatemap  ->  " .. v)

-- Sequential FF-state-naming alignment (needs FPGA_LEC_NAMES=1): fpga_map splits
-- a multi-bit register into `<reg>__b<i>` FDREs; prep_for_z3 iflift-collapses the
-- FDRE bodies, ffpack re-packs them into a bus FF, the reg-bitbus resolver maps
-- `<reg>__b<i>` -> reg[i], and the undriven-net tie grounds the floating FDRE.R /
-- GND -- so ffrip's state cones line up by name with the behavioural bus reg.
function seq_equiv(name)
  local s = svd.parse("verible", name, {D .. name .. ".sv"})
  local p = svd.parse("verible", name, {D .. name .. ".sv"})
  p = svd.unroll(p); p = svd.inline(p); p = svd.iflift(p)
  p = svd.blocking_subst(p); p = svd.meminfer(p); p = svd.memlower(p)
  local im = svd.augment_xil_models(svd.mapped_to_prog(svd.gate_map(svd.pick(p, name), 6, 0)))
  local vv = svd.miter(svd.pick(s, name), svd.pick(im, name))
  if vv == "EQUIVALENT" then pass = pass + 1 else fail = fail + 1 end
  print("  seq FF-align " .. name .. "  ->  " .. vv)
end
seq_equiv("seqreg")
seq_equiv("seqmux")

-- CARRY4 arithmetic: guards (a) behavioral_to_hardcaml widening the add to the
-- result width so the carry-out isn't dropped (y[8] of [8:0]=a+b), and (b)
-- flatten_for_z3 fanning out a CARRY4's concat .O/.CO ports to their per-bit nets
-- (else the sum/carry float and the undriven-tie zeros them).
seq_equiv("addcarry")   -- combinational (no FFs; seq_equiv still works)
seq_equiv("counter")    -- FF alignment + CARRY4 increment together
seq_equiv("inout_bidir")-- inout pin -> primary I/O linked var (behavioral==gatemap)

-- RAM64M functional model vs a behavioral reference using the NATURAL memory
-- semantics (mem_x[ADDRD]<=DIx / mem_x[ADDR]).  Proves the model (4x 64x1 mems,
-- common write addr, async reads) AND ffrip's @mem_write -> read-modify-write
-- lowering.  Ref regs are named u__mem_* to align with the flattened instance.
sp = svd.parse("verible", "ram64m_top", {D .. "ram64m_ref.v"})
sp = svd.unroll(sp); sp = svd.inline(sp); sp = svd.iflift(sp)
sp = svd.blocking_subst(sp); sp = svd.meminfer(sp); sp = svd.memlower(sp)
im = svd.augment_xil_models(svd.parse("verible-ext", "ram64m_top", {D .. "ram64m_impl.v"}))
v = svd.miter(svd.pick(sp, "ram64m_top"), svd.pick(im, "ram64m_top"))
if v == "EQUIVALENT" then pass = pass + 1 else fail = fail + 1 end
print("  RAM64M model == behavioral RAM64M  ->  " .. v)

-- Distributed-RAM inference (needs MEMLOWER_FPGA=1): a 1W+2R depth-32 async RAM
-- must map to RAM32M, NOT bit-blast to FFs.  Structural check (no RAM32M
-- functional model to miter against): RAM32M present AND no FDRE.  Guards BUG1
-- (read-address collection across the two separate read assigns).
p = svd.parse("verible", "ram1w2r32", {D .. "ram1w2r32.sv"})
p = svd.unroll(p); p = svd.inline(p); p = svd.iflift(p)
p = svd.blocking_subst(p); p = svd.meminfer(p); p = svd.memlower(p)
rtxt = svd.insts(svd.pick(svd.mapped_to_prog(svd.gate_map(svd.pick(p, "ram1w2r32"), 6, 0)), "ram1w2r32"))
if strfind(rtxt, "RAM32M", 1, 1) and not strfind(rtxt, "FDRE", 1, 1) then
  pass = pass + 1; print("  RAM 1W2R depth-32 -> RAM32M  ->  OK")
else
  fail = fail + 1; print("  RAM 1W2R depth-32 -> RAM32M  ->  FAIL (bit-blast?)")
end

-- BUG2 regression: 1W+2R depth-64 async RAM must map to RAM64M (Vivado's choice
-- for the SGMII rx_elastic_buffer), not bit-blast.  Structural check.
p = svd.parse("verible", "ram1w2r64", {D .. "ram1w2r64.sv"})
p = svd.unroll(p); p = svd.inline(p); p = svd.iflift(p)
p = svd.blocking_subst(p); p = svd.meminfer(p); p = svd.memlower(p)
rtxt = svd.insts(svd.pick(svd.mapped_to_prog(svd.gate_map(svd.pick(p, "ram1w2r64"), 6, 0)), "ram1w2r64"))
if strfind(rtxt, "RAM64M", 1, 1) and not strfind(rtxt, "FDRE", 1, 1) then
  pass = pass + 1; print("  RAM 1W2R depth-64 -> RAM64M  ->  OK")
else
  fail = fail + 1; print("  RAM 1W2R depth-64 -> RAM64M  ->  FAIL (bit-blast?)")
end

print("== pass=" .. pass .. " fail=" .. fail .. " ==")
