// SV: `if (a && b)` with mixed widths a=1-bit, b=5-bit.
// Logical && reduces each operand to "non-zero", so result is 1 when
// BOTH are non-zero — including b = 5'b01010 (which is 10, non-zero).
module op_logand (input a, input [4:0] b, output reg y);
  always @* y = (a && b) ? 1'b1 : 1'b0;
endmodule
