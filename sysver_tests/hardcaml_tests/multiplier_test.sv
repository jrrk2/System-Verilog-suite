module multiplier_test (
    input [5:0] b,
    input [5:0] a,
    output reg [5:0] mul);

    always_comb
          mul = a * b;

endmodule
