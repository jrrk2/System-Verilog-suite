// Golden = the original FF-chain shift register, with its register renamed to
// the name srl_infer's expanded SRL cell produces after flatten
// (<inst>__sr, inst = sr__srlq15_1) so the by-name ffrip state matching aligns.
module srl(input clk, input ce, input d, output q);
  reg [15:0] sr__srlq15_1__sr;
  always @(posedge clk) if (ce) sr__srlq15_1__sr <= {sr__srlq15_1__sr[14:0], d};
  assign q = sr__srlq15_1__sr[15];
endmodule
