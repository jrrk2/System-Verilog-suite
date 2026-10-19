// A combinational block that zeroes a vector and then writes its bits one at a
// time.  Constant propagation substituted the just-assigned 8'd0 for the WRITE
// TARGET of each bit write (the first argument of a write pseudo-op is a name,
// not a value), the writes landed on a literal, and the output came out tied
// to ground with its input dropped -- in every binary before the fix.
module comb_bitwrite_const(input logic [7:0] a, input logic clk, output logic [7:0] y, output logic [7:0] q);
  always_comb begin
    y = 8'd0;
    for (int i = 0; i < 8; i++) y[i] = a[7-i];
  end
  always_ff @(posedge clk) begin
    for (int j = 0; j < 8; j++) q[j] <= a[j] ^ y[j];
  end
endmodule
