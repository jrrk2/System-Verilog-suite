module top(input clk, input rst, input en, input d, output reg q);
  always @(posedge clk)
    if (rst) q <= 1'b0;
    else if (en) q <= d;
endmodule
