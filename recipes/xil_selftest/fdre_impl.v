module top(input clk, input rst, input en, input d, output q);
  FDRE ff (.C(clk), .CE(en), .D(d), .R(rst), .Q(q));
endmodule
