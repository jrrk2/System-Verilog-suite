// 4×4 → 8 unsigned multiplier.
module mul_unsigned (
  input  logic [3:0] a,
  input  logic [3:0] b,
  output logic [7:0] y
);
  assign y = a * b;
endmodule
