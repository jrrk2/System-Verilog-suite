module top(input CLK, input CE, input D, output Q, output Q31);
  SRLC32E ff (.CLK(CLK), .CE(CE), .D(D), .Q(Q), .Q31(Q31), .A(5'b11111));
endmodule
