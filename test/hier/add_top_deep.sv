// Two-level hierarchy: add_top → wrap → adder.
module adder (
  input  [7:0] x, input [7:0] y, output [7:0] z
);
  assign z = x + y;
endmodule

module wrap (
  input  [7:0] aa, input [7:0] bb, output [7:0] cc
);
  adder u_a (.x(aa), .y(bb), .z(cc));
endmodule

module add_top (
  input  [7:0] a, input [7:0] b, output [7:0] s
);
  wrap u_w (.aa(a), .bb(b), .cc(s));
endmodule
