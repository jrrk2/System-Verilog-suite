// Distributed dual-port RAM (1 sync write + 2 async reads).
// Pattern lifted from picorv32_regs — Vivado infers RAM32M / LUT-RAM
// here. The Verible→BIR converter must produce a `BArray` for the
// reg, emit `@mem_write(...)` for the indexed assignment, and emit
// `BSelect{array;index}` (not BSlice) for the indexed reads — so
// `behavioral_meminfer` can categorise it as `distributed_async_1w2r`.
//
// Note: ports are declared one-per-line because the Verible→BIR
// converter currently mishandles comma-grouped port declarations
// (only the first name keeps its declared width/direction).
module distributed_dual_port_ram (
  input         clk,
  input         wen,
  input  [4:0]  waddr,
  input  [4:0]  raddr1,
  input  [4:0]  raddr2,
  input  [31:0] wdata,
  output [31:0] rdata1,
  output [31:0] rdata2
);
  reg [31:0] mem [0:31];
  always @(posedge clk)
    if (wen) mem[waddr] <= wdata;
  assign rdata1 = mem[raddr1];
  assign rdata2 = mem[raddr2];
endmodule
