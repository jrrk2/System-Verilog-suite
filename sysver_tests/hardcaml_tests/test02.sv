
module test02(a, y);
  input signed [3:0] a;
  output signed [4:0] y;
  assign y = (~a) >> 1;
endmodule
