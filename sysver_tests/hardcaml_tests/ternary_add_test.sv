module ternary_add_test (
    input clear,
    input clock,
    input [3:0] a,
    input [3:0] b,
    input c,
    output reg [3:0] d_reg);

    always @(posedge clock)
        if (clear)
            d_reg <= 4'b0000;
        else
            d_reg <= c ? { a - b } : ( a + b );

endmodule
