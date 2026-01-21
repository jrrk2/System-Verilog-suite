module less_test_tb ();
   
   reg [7:0] a;
   reg [7:0] b;
   
   wire c, ch;

   wire [8:0] diff = {1'b0,a} - {1'b0,b};
   
   less_test dut(.a, .b, .c);
   
   less_test_hardcaml duth(.a, .b, .c(ch));
   
   always
     begin
	a = $random();
	b = $random();
        #1000
        $display("a,b,c,ch = %x %x %x %x %x", a, b, diff, c, ch);
     end

   initial
        #10000
     	$finish;	

endmodule
