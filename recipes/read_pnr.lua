print("== read actual nextpnr P&R netlist ==")
impl = svd.read_nextpnr_json("/Users/jonathan/nextpnr-xilinx/xilinx/examples/counter25/routed.json")
print("modules:  " .. svd.module_names(impl))
print("coverage: " .. svd.xil_models_coverage(impl))
