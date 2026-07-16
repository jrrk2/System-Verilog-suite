// Registered counter with sync reset + enable: combines FF-state alignment with
// the CARRY4 increment (q <= q + 3).
module counter (input clk, input rst, input en, output reg [11:0] q);
  always @(posedge clk) if (rst) q <= 0; else if (en) q <= q + 12'd3;
endmodule
