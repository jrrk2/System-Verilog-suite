module counter (
  input  c,
  input  r,
  input  en,
  output reg [3:0] q
);
  always @(posedge c or posedge r)
    if (r)       q <= 4'd0;
    else if (en) q <= q + 4'd1;
endmodule
