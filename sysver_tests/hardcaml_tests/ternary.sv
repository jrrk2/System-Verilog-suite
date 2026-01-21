module ternary (
    input [3:0] a,
    input [3:0] b,
    input c,
    output [3:0] d);

    assign
      d = c ? a : b;

endmodule
