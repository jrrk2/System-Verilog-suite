module dff_top (
  input  c, input r, input  d, output reg q
);
  always @(posedge c or posedge r)
    if (r) q <= 1'b0;
    else   q <= d;
endmodule
