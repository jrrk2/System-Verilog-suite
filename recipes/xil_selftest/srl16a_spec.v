module top(input CLK, input CE, input D, output Q);
  reg [15:0] ff__sr;
  always @(posedge CLK) if (CE) ff__sr <= {ff__sr[14:0], D};
  assign Q = ff__sr[15];
endmodule
