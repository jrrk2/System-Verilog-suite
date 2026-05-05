module chain (
  input  [7:0] a, input [7:0] b, input [7:0] c, input [7:0] d,
  output [7:0] s
);
  wire [7:0] t1, t2;
  assign t1 = a + b;
  assign t2 = t1 + c;
  assign s  = t2 + d;
endmodule
