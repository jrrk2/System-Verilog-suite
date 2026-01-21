module DFF_nibble (input clk, input [3:0] data, output reg [3:0] q);

   always @(posedge clk) q <= data;

endmodule // DFF
