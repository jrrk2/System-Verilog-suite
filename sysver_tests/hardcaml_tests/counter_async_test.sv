module counter_async_test (
    input rst,
    input clk,
    output reg [8:0] cnt);

    always @(posedge clk or posedge rst)
        if (rst)
            cnt <= 8'b00000000;
        else
          cnt <= cnt + 1;

endmodule
