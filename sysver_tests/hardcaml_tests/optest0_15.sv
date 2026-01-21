module optest0_15(clk, mode, u1, s1, u2, s2, y);

input clk;
input [3:0] mode;

input [3:0] u1, u2;
input signed [3:0] s1, s2;

output reg [7:0] y;

always @(posedge clk) begin
	case (mode)
		 4'd0: y <= u1 << u2;
/*		 4'd1: y <= u1 << s2;
		 4'd2: y <= s1 << u2;
*/		 4'd3: y <= s1 << s2;
/*
		 4'd4: y <= u1 >> u2;
		 4'd5: y <= u1 >> s2;
		 4'd6: y <= s1 >> u2;
		 4'd7: y <= s1 >> s2;

		 4'd8: y <= u1 <<< u2;
		 4'd9: y <= u1 <<< s2;
		4'd10: y <= s1 <<< u2;
		4'd11: y <= s1 <<< s2;

		4'd12: y <= u1 >>> u2;
		4'd13: y <= u1 >>> s2;
		4'd14: y <= s1 >>> u2;
		4'd15: y <= s1 >>> s2;
 */
	       default: y <= 8'h42;
	endcase
end

endmodule
