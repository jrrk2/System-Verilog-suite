// Mixed-signedness multiplier — `a` is unsigned, `b` is signed. By LRM
// rules the mixed multiplication is unsigned (a wins by being unsigned),
// but the user has reason to believe Vivado annotates this incorrectly
// even when the bit-level output is right. The formal miter will tell.
module mul_mixed (
  input  logic        [3:0] a,
  input  logic signed [3:0] b,
  output logic        [7:0] y
);
  assign y = a * b;
endmodule
