module top(input clk, input set, input en, input d, output reg q);
  always @(posedge clk)
    if (set) q <= 1'b1;
    else if (en) q <= d;
endmodule
