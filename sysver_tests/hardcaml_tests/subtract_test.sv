module subtract_test (
    input clk,
    input signed [7:0] a,
    input signed [7:0] b,
    output reg [7:0] c);

    always @(posedge clk)
          c <= a - b;

endmodule
