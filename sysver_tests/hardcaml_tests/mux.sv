module mux(mode, u1, u2, u3, u4, y);

input [1:0] mode;
input [3:0] u1, u2, u3, u4;

output reg [3:0] y;

always_comb
	case (mode)
		 0: y <= u1;
		 1: y <= u2;
		 2: y <= u3;
		 3: y <= u4;
	endcase

endmodule
