module t00_baseline(input clk, input [7:0] a, input [7:0] b, output reg [7:0] y);
  always_ff @(posedge clk) y <= a + b;
endmodule
