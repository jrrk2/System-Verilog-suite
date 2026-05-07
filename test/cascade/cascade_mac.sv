// 24x24 MAC built from 8-bit multipliers and 48-bit adders, with
// explicit single-driver buffering on every off-module wire so
// hier_synth's phantom-IO promotion sees each net unambiguously.
module mul8 (input [7:0] a, input [7:0] b, output [15:0] y);
  assign y = a * b;
endmodule

module add48 (input [47:0] a, input [47:0] b, output [47:0] y);
  assign y = a + b;
endmodule

module cascade_mac (
  input         clk,
  input         ena,
  input         dclr,
  input  [23:0] din,
  input  [23:0] coef,
  output reg [50:0] result
);
  // input slices (phantom OUT for instances)
  wire [7:0] d0, d1, d2, c0, c1, c2;
  assign d0 = din[7:0];   assign d1 = din[15:8];  assign d2 = din[23:16];
  assign c0 = coef[7:0];  assign c1 = coef[15:8]; assign c2 = coef[23:16];

  // Each instance output goes to its own dedicated wire (phantom IN).
  // For wires that feed another instance, we add an explicit
  // assign-through buffer to a *_buf wire whose direction is
  // unambiguously phantom OUT.
  wire [15:0] p00_q, p01_q, p02_q, p10_q, p11_q, p12_q, p20_q, p21_q, p22_q;
  mul8 m00 (.a(d0), .b(c0), .y(p00_q));
  mul8 m01 (.a(d0), .b(c1), .y(p01_q));
  mul8 m02 (.a(d0), .b(c2), .y(p02_q));
  mul8 m10 (.a(d1), .b(c0), .y(p10_q));
  mul8 m11 (.a(d1), .b(c1), .y(p11_q));
  mul8 m12 (.a(d1), .b(c2), .y(p12_q));
  mul8 m20 (.a(d2), .b(c0), .y(p20_q));
  mul8 m21 (.a(d2), .b(c1), .y(p21_q));
  mul8 m22 (.a(d2), .b(c2), .y(p22_q));

  // Pad each partial product to 48 bits.
  wire [47:0] e00, e01, e02, e10, e11, e12, e20, e21, e22;
  assign e00 = {32'b0, p00_q};
  assign e01 = {24'b0, p01_q,  8'b0};
  assign e02 = {16'b0, p02_q, 16'b0};
  assign e10 = {24'b0, p10_q,  8'b0};
  assign e11 = {16'b0, p11_q, 16'b0};
  assign e12 = { 8'b0, p12_q, 24'b0};
  assign e20 = {16'b0, p20_q, 16'b0};
  assign e21 = { 8'b0, p21_q, 24'b0};
  assign e22 = {       p22_q, 32'b0};

  // Adder chain.  Each add48 output is captured in a *_q wire
  // (phantom IN); a sibling *_b wire copies it for the next stage's
  // input (phantom OUT).
  wire [47:0] s1_q, s2_q, s3_q, s4_q, s5_q, s6_q, s7_q, sum_q;
  wire [47:0] s1_b, s2_b, s3_b, s4_b, s5_b, s6_b, s7_b;
  assign s1_b = s1_q; assign s2_b = s2_q; assign s3_b = s3_q;
  assign s4_b = s4_q; assign s5_b = s5_q; assign s6_b = s6_q;
  assign s7_b = s7_q;

  add48 a1 (.a(e00),  .b(e01), .y(s1_q));
  add48 a2 (.a(s1_b), .b(e02), .y(s2_q));
  add48 a3 (.a(s2_b), .b(e10), .y(s3_q));
  add48 a4 (.a(s3_b), .b(e11), .y(s4_q));
  add48 a5 (.a(s4_b), .b(e12), .y(s5_q));
  add48 a6 (.a(s5_b), .b(e20), .y(s6_q));
  add48 a7 (.a(s6_b), .b(e21), .y(s7_q));
  add48 a8 (.a(s7_b), .b(e22), .y(sum_q));

  always @(posedge clk)
    if (ena)
      if (dclr) result <= {3'b0, sum_q};
      else      result <= {3'b0, sum_q} + result;
endmodule
