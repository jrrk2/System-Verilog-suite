-- End-to-end test for #78 + #81:
--   1. Parse parent_with_arch.sv (parent → bk_adder8 leaf with
--      `(* sv_decomp_adder = "brent_kung" *)`)
--   2. Parse parent_spec.sv (behavioural `s = a + b`)
--   3. Miter — the substitution pass should abstract u_add via the
--      brent_kung@8 certificate from `verify-arch` and the result
--      should be EQUIVALENT.

print("=== arch substitution end-to-end ===")

p_arch = svd.parse("verible", "top", {"test/hier/parent_with_arch.sv"})
p_spec = svd.parse("verible", "top", {"test/hier/parent_spec.sv"})

m_arch = svd.pick(p_arch, "top")
m_spec = svd.pick(p_spec, "top")

-- Print the BIR of the arch side so we can see attrs landed.
print("--- arch BIR ---")
print(svd.bir(m_arch))

print("--- miter ---")
print("result: " .. svd.miter(m_arch, m_spec))
