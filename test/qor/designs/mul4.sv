module mul4u (input [3:0] a, input [3:0] b, output [7:0] y); assign y = a * b; endmodule
module mul4s (input signed [4:0] a, input signed [3:0] b, output signed [8:0] y); assign y = a * b; endmodule
module macc4 (input clk, input rst, input [3:0] a, input [3:0] b, output logic [11:0] acc);
  always_ff @(posedge clk) if (rst) acc <= 0; else acc <= acc + a * b;
endmodule
