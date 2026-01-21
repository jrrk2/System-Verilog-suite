module unary(clk, u1, y);

input clk;

input [3:0] u1;

output reg [15:0] y;

always @(posedge clk) begin
   y <= {&u1, |u1, ^u1, &~u1, |~u1, ^~u1, ~u1};
end

endmodule
