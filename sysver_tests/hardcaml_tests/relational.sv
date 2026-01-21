module relational(mode, u1, u2, y);

input [2:0] mode;

input [3:0] u1, u2;

output reg y;

always_comb case (mode)
		 0: y = u1 < u2;
		 1: y = u1 <= u2;
		 2: y = u1 == u2;
		 3: y = u1 != u2;
		 4: y = u1 >= u2;
		 5: y = u1 > u2;
		 6: y = & (u1 ^ u2);
		 7: y = | (u1 ^ u2);
	endcase

endmodule
