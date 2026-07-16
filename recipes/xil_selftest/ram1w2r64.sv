// 1W+2R async-read RAM depth 64 -> must map to RAM64M (Vivado maps the SGMII
// rx_elastic_buffer this way: 6 RAM64M for width 8).  Guards BUG2 (the depth>32
// 1W+2R path). Needs MEMLOWER_FPGA=1.
module ram1w2r64 (
  input clk, input we, input [5:0] wa, input [7:0] wd,
  input [5:0] ra0, input [5:0] ra1, output [7:0] rd0, output [7:0] rd1);
  reg [7:0] mem [0:63];
  always @(posedge clk) if (we) mem[wa] <= wd;
  assign rd0 = mem[ra0];
  assign rd1 = mem[ra1];
endmodule
