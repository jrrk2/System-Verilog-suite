module top(input CLK, input CE, input D, output Q, output Q31);
  reg [31:0] ff__sr;
  always @(posedge CLK) if (CE) ff__sr <= {ff__sr[30:0], D};
  assign Q   = ff__sr[31];
  assign Q31 = ff__sr[31];
endmodule
