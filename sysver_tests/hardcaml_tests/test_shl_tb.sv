module test_shl_tb();

   reg b;
   reg [3:0] a;
   wire [3:0] c;

   test_shl dut(.a, .b, .c);
   
   initial
     begin
	#1000
	b = 0;
	a = 5;
	#1000
	$display(a,b,c);
	b = 1;
	a = 5;
	#1000
	$display(a,b,c);
	b = 1;
	a = 3;
	#1000
	$display(a,b,c);
     end

endmodule // test_shl_tb

