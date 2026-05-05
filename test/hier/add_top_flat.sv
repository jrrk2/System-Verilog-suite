// Reference (flat) version: 8-bit ripple add, single module.
module add_top (
  input  [7:0] a,
  input  [7:0] b,
  output [7:0] s
);
  assign s = a + b;
endmodule
