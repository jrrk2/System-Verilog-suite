module adder_test (
    input [7:0] a,
    input [7:0] b,
    output reg [7:0] c);

    always_comb
          c = a + b;

endmodule
