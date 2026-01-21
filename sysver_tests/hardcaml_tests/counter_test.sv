module counter_test (
    input rst,
    input clk,
    output reg [3:0] cnt);

    always @(posedge clk)
        if (rst)
            cnt <= 4'b0000;
        else
          cnt <= cnt + 1;

endmodule
