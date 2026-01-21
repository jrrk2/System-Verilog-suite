module multiplier_signed_tb ();

   logic clock;
   logic signed [3:0] b;   
   logic signed [7:0] a;
   wire signed [11:0]	      y;

   multiplier_signed dut (
    .clock,
    .b,
    .a,
    .y);

   initial
     begin
	a = 0;
	b = 0;
	@(negedge clock)
	  $display(a,b,y);
	a = 2;
	b = 3;
	@(negedge clock)
	  $display(a,b,y);
	a = -2;
	b = 5;
	@(negedge clock)
	  $display(a,b,y);
	a = 6;
	b = -3;
	@(negedge clock)
	  $display(a,b,y);
	a = -5;
	b = -7;
	@(negedge clock)
	  $display(a,b,y);
	$finish();
	
     end

   always
     begin
	clock = 0;
	# 1000
	clock = 1;
	# 1000
	clock = 0;
     end
   
endmodule
