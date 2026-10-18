module mul4u (input [3:0] a, input [3:0] b, output [7:0] y); assign y = a * b; endmodule
module mul4s (input signed [4:0] a, input signed [3:0] b, output signed [8:0] y); assign y = a * b; endmodule
module macc4 (input clk, input rst, input [3:0] a, input [3:0] b, output logic [11:0] acc);
  always_ff @(posedge clk) if (rst) acc <= 0; else acc <= acc + a * b;
endmodule
// split cases (run with DSP_SPLIT_LIMBS=5,4): too wide for one "DSP", and a
// signed product whose upper half nobody reads
module mulw_u (input [8:0] a, input [6:0] b, output [15:0] y); assign y = a * b; endmodule
module mulw_s (input clk, input signed [7:0] a, input signed [7:0] b, output logic [9:0] y);
  wire signed [15:0] p = a * b;
  always_ff @(posedge clk) y <= p[9:0];
endmodule
