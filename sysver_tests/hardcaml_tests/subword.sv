module subword ( 
		input [3:0] a, b, output [7:0] c
	);

   assign c[7:4] = a;
   assign c[3:0] = b;

endmodule
