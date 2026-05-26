module op_block_inc (input clk, input resetn, output reg [7:0] x);
  always @(posedge clk) if (!resetn) x = 8'd0; else x = x + 1;
endmodule
