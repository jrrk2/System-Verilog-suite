
module genscope # (parameter GEN=1) (
	input wire	   CLK,
	input wire	   RST,
	output logic [3:0] Q);
   
case (GEN)
  0:
    begin:zero
       always @(posedge CLK or posedge RST)
       if ((RST ==  1'b1))
            Q <= 0;
         else
	      Q <= Q + 1;
     end
  1:
    begin:one
       always @(posedge CLK or posedge RST)
       if ((RST ==  1'b1))
            Q <= 0;
         else
	      Q <= Q - 1;
     end

  default:
    $error("GEN size must be 6 or 11");

endcase // case (SIZE_E)

endmodule // slib_fifo
