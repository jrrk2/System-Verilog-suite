// Behavioural reference: equivalent to `parent_with_arch.sv` once
// the abstraction kicks in.
module top (
  input  [7:0] a, input [7:0] b, output [7:0] s
);
  assign s = a + b;
endmodule
