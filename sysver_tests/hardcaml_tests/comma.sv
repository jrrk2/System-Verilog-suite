module comma (output [7:0] c, d, input [3:0] a, b); 

   assign c={a,b};
   assign d={b,a};
   
endmodule
