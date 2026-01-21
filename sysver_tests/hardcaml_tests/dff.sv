module DFF (input clk, data, output reg q);

   always @(posedge clk) q <= data;

endmodule // DFF
