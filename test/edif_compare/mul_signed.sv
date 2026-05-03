// 4×4 → 8 signed multiplier. Both operands and the result are signed.
module mul_signed (
  input  logic signed [3:0] a,
  input  logic signed [3:0] b,
  output logic signed [7:0] y
);
  assign y = a * b;
endmodule
