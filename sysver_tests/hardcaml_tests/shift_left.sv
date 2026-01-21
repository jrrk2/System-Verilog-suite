module shift_left(
input clk,
input [3:0] u1, u2,
output reg [3:0] y);

always @(posedge clk) begin
		 y <= u1 << u2;
end

endmodule
