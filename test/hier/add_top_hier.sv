// Hierarchical version: top instantiates a child `adder` module.
// Functionally identical to add_top_flat.sv but the boundary
// between top and child should be preserved through the BIR
// pipeline — only flattened transiently at Z3-encode time.

module adder (
  input  [7:0] x,
  input  [7:0] y,
  output [7:0] z
);
  assign z = x + y;
endmodule

module add_top (
  input  [7:0] a,
  input  [7:0] b,
  output [7:0] s
);
  adder u_add (.x(a), .y(b), .z(s));
endmodule
