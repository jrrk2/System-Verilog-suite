module top(input a, input b, input c, input d, output y);
  assign y = (a & b) | (c ^ d);
endmodule
