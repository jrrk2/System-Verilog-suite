module srl (input clk, input ce, input d, output q);
  reg [15:0] sr;
  always @(posedge clk) if (ce) sr <= {sr[14:0], d};
  assign q = sr[15];
endmodule
