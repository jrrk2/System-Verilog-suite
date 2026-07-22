sv = ARGV[1]; edif = ARGV[2]; top = ARGV[3]
vp = svd.parse("verible", top, {sv})
ep = svd.read_edif(edif)
vm = svd.prep_for_z3(svd.pick(vp, top))
em = svd.prep_for_z3(svd.pick(ep, top))
print("MITER[" .. top .. "] verible <-> vivado_edif : " .. svd.miter(vm, em))
