module multiplier_signed (
    input signed [5:0] b,
    input signed [5:0] a,
    output reg [5:0] mul);

    always_comb
          mul <= a * b;

endmodule
