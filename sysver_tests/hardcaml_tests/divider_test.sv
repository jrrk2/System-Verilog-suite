module divider_test (
    input clear,
    input clock,
    input [3:0] b,
    input [11:0] a,
    output reg [11:0] d_reg);

    always @(posedge clock)
        if (clear)
            d_reg <= 8'b00000000;
        else
          d_reg <= a / b;

endmodule
