module casetest8(clk, mode, u1, u2, y);

input clk;
input [2:0] mode;

input [3:0] u1, u2;

output reg [3:0] y;

always @(posedge clk) begin
	case (mode)
		 0: y <= u1 + u2;
		 1: y <= u1 - u2;
		 2: y <= u1 * u2;
		 3: y <= u1 & u2;
		 4: y <= u1 | u2;
		 5: y <= u1 ^ u2;
		 6: y <= u1 &~ u2;
		 7: y <= u1 |~ u2;
	endcase
end

endmodule
