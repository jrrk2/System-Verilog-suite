module counter_async_en_test #(WIDTH=8) (
    input rst,
    input clk,
    input en,
    output reg [WIDTH-1:0] cnt);

    always @(posedge clk or posedge rst)
        if (rst)
            cnt <= 0;
        else if (en)
          cnt <= cnt + 1;

endmodule
