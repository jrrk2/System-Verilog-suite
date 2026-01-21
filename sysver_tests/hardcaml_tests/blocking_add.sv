module blocking_add ( 
	input logic [3:0] a, b, 
	output logic [3:0] y
	); 

   always_comb
     begin
      y = a*b;
      for (int i = 0; i < 4; i=i+1)
	begin
	   y = y - (a[i] ? b : 0);
	end
     end
   
endmodule
