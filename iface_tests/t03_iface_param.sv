interface if3 #(parameter W=8); logic [W-1:0] d; endinterface
module t03_iface_param(input [7:0] a, output [7:0] y);
  if3 #(.W(8)) u();
  assign u.d = a ^ 8'hA5;
  assign y   = u.d;
endmodule
