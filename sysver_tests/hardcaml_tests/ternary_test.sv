module ternary_test (
    input clear,
    input clock,
    input [3:0] b,
    input [7:0] a,
    output reg [7:0] d,
    output reg [7:0] d_reg);

    always @(posedge clock)
        d_reg <= clear ? 8'b00000000 : ( a == b ) ? { a[6:0], 1'b0 } : ( a + b );

    always_comb
        d =  ( a == b ) ? { a[6:0], 1'b0 } : ( a + b );

endmodule
