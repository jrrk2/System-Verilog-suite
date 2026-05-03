// Single-port BRAM with registered output and read-during-write
// don't-care semantics. This is the canonical Xilinx-BRAM idiom and
// the pattern every SweRV `ram_NxM` macro uses.
//
// Vivado infers a RAMB18/RAMB36 here. The Behavioral_meminfer pass
// must produce `single_port_bram` (1W/1R sync) for it.
module single_port_bram (
  input              CLK,
  input              WE,
  input  [4:0]       ADR,
  input  [7:0]       D,
  output reg [7:0]   Q
);
  reg [7:0] mem [0:31];
  always @(posedge CLK) begin
    if (WE) begin
      mem[ADR] <= D;
      Q <= 'x;
    end else
      Q <= mem[ADR];
  end
endmodule
