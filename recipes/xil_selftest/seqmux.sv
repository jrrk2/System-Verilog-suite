// Registered output with a mux/select next-state (no arithmetic).
module seqmux (input clk, input sel, input [3:0] a, input [3:0] b, output reg [3:0] q);
  always @(posedge clk) q <= sel ? (a & b) : (a | b);
endmodule
