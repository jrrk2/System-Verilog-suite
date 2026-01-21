module less_test (
    input [3:0] a,
    input [3:0] b,
    output reg c);

    always_comb
          c = a < b;

endmodule
