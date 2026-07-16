module top(input CLK, input CE, input D, output Q);
  SRL16E ff (.CLK(CLK), .CE(CE), .D(D), .Q(Q),
             .A0(1'b0), .A1(1'b1), .A2(1'b1), .A3(1'b0));
endmodule
