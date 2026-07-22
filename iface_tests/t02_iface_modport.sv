interface if2(input logic clk);
  logic [7:0] data, cnt;
  modport wr(input clk, output data);
  modport rd(input clk, input data, output cnt);
endinterface
module drv(if2.wr p); always_ff @(posedge p.clk) p.data <= p.data + 8'd1; endmodule
module rcv(if2.rd p); always_ff @(posedge p.clk) p.cnt  <= p.data;        endmodule
module t02_iface_modport(input clk, output [7:0] cnt);
  if2 u(clk);
  drv d(u);
  rcv r(u);
  assign cnt = u.cnt;
endmodule
