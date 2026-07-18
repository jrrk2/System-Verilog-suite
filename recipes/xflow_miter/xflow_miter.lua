-- Cross-flow module-by-module equivalence: Vivado post-synth netlist (A) vs
-- SVS from-source gate-mapped netlist (B).  miter_hier abstracts user
-- submodules as UFs and cuts GT/MMCM/RAMB black boxes (assume-guarantee).
local D="/home/jonathan/v7-johnson-demo/ethsoc/"
local FILES = {D.."arp_ctrl.sv",D.."framing_top_sgmii.sv",D.."sgmii_soc.sv",D.."eth_mac_1g.sv",
  D.."axis_gmii_rx.sv",D.."axis_gmii_tx.sv",D.."rgmii_lfsr.sv",D.."dualmem_widen.sv",D.."dualmem_widen8.sv",
  D.."eth_macro.sv",D.."async_fifo.v",D.."dualmem64.sv",D.."ramb16_compat.v",D.."eth_stream_conv.v",D.."pcs_pma_flat.v",D.."vc707_arp.v"}

-- Side A: Vivado's synthesized primitive netlist
local A = svd.parse("verible", "top", {D.."vc707_arp_netlist.v"})

-- Side B: SVS synth, gate_map every non-PCS module, hierarchy preserved (NO flatten)
local B = svd.parse("verible", "top", FILES)
B = svd.unroll(B); B=svd.inline(B); B=svd.iflift(B)
B = svd.blocking_subst(B); B=svd.meminfer(B); B=svd.memlower(B); B=svd.srl_infer(B)
local names = svd.module_names(B)
NT={}; local cnt=0; local i=1; local n=strlen(names)
while i<=n do local j=strfind(names,",",i,1); local nm
  if j then nm=strsub(names,i,j-1); i=j+1 else nm=strsub(names,i,n); i=n+1 end
  if strlen(nm)>0 then cnt=cnt+1; NT[cnt]=nm end
end
local k=1
while k<=cnt do
  local M=NT[k]
  if not strfind(M, "gig_ethernet_pcs_pma", 1, 1) then
    B = svd.splice(B, M, svd.mapped_to_prog(svd.gate_map(svd.pick(B,M),6,0)))
  end
  k=k+1
end

print("=== CROSS-FLOW MITER: Vivado netlist vs SVS gate-map ===")
print(svd.miter_hier(A, B, "top"))
