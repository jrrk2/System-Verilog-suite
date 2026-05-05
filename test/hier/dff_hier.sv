// Sequential hierarchy: top wraps a child DFF.
module dff (
  input  c, input r, input  d, output reg q
);
  always @(posedge c or posedge r)
    if (r) q <= 1'b0;
    else   q <= d;
endmodule

module dff_top (
  input  c, input r, input  d, output q
);
  dff u_dff (.c(c), .r(r), .d(d), .q(q));
endmodule
