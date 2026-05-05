-- Confirm: flat vs hier currently MITER-FAILS because the encoder
-- ignores instances. After the boundary-preserving miter (#79) lands,
-- this should print EQUIVALENT.

local p_flat = svd.parse("verible", "add_top", {"test/hier/add_top_flat.sv"})
local p_hier = svd.parse("verible", "add_top", {"test/hier/add_top_hier.sv"})
local m_flat = svd.pick(p_flat, "add_top")
local m_hier = svd.pick(p_hier, "add_top")

print("flat BIR:")
print(svd.bir(m_flat))
print("---")
print("hier BIR:")
print(svd.bir(m_hier))
print("---")
print("miter result: " .. svd.miter(m_flat, m_hier))
