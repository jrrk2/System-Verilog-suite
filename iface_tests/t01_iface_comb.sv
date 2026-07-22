interface if1; logic [7:0] d; endinterface
module t01_iface_comb(input [7:0] a, output [7:0] y);
  if1 u();
  assign u.d = a + 8'd1;
  assign y   = u.d;
endmodule
