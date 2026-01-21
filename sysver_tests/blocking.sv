 

	module test_add ( 
		input logic [3:0] a, b, 
		output logic [3:0] c 
	); 
		always @* begin 
			c = a; 
			c += b; 
		end 
	endmodule

	module test_sub ( 
		input logic [3:0] a, b, 
		output logic [3:0] c 
	); 
		always @* begin 
			c = a; 
			c -= b; 
		end 
	endmodule

	module test_mul ( 
		input logic [3:0] a, b, 
		output logic [3:0] c 
	); 
		always @* begin 
			c = a; 
			c *= b; 
		end 
	endmodule

 

	module test_bit_and ( 
		input logic [3:0] a, b, 
		output logic [3:0] c 
	); 
		always @* begin 
			c = a; 
			c &= b; 
		end 
	endmodule

	module test_bit_or ( 
		input logic [3:0] a, b, 
		output logic [3:0] c 
	); 
		always @* begin 
			c = a; 
			c |= b; 
		end 
	endmodule

	module test_bit_xor ( 
		input logic [3:0] a, b, 
		output logic [3:0] c 
	); 
		always @* begin 
			c = a; 
			c ^= b; 
		end 
	endmodule

	module test_shl ( 
		input logic [3:0] a, b, 
		output logic [3:0] c 
	); 
		always @* begin 
			c = a; 
			c <<= b; 
		end 
	endmodule

	module test_shr ( 
		input logic [3:0] a, b, 
		output logic [3:0] c 
	); 
		always @* begin 
			c = a; 
			c >>= b; 
		end 
	endmodule

	module test_sshl ( 
		input logic [3:0] a, b, 
		output logic [3:0] c 
	); 
		always @* begin 
			c = a; 
			c <<<= b; 
		end 
	endmodule

	module test_sshr ( 
		input logic [3:0] a, b, 
		output logic [3:0] c 
	); 
		always @* begin 
			c = a; 
			c >>>= b; 
		end 
	endmodule

