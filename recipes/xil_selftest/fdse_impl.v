module top(input clk, input set, input en, input d, output q);
  FDSE ff (.C(clk), .CE(en), .D(d), .S(set), .Q(q));
endmodule
