interface hs_if;
  logic       valid, ready;
  logic [7:0] data;
  modport src(output valid, output data, input ready);
  modport dst(input valid, input data, output ready);
endinterface
module src_m(input clk, input rst, input [7:0] din, hs_if.src s);
  always_ff @(posedge clk) begin
    if (rst) begin s.valid <= 0; s.data <= 0; end
    else if (!s.valid || s.ready) begin s.valid <= 1; s.data <= din; end
  end
endmodule
module dst_m(input clk, hs_if.dst s, output reg [7:0] acc);
  assign s.ready = 1'b1;
  always_ff @(posedge clk) if (s.valid & s.ready) acc <= acc + s.data;
endmodule
module t04_iface_hs(input clk, input rst, input [7:0] din, output [7:0] acc);
  hs_if h();
  src_m u_s(clk, rst, din, h);
  dst_m u_d(clk, h, acc);
endmodule
