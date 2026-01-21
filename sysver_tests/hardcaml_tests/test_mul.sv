module test_mul(
    input logic [3:0] a  /*  ARNG(3:0) */ ,
    input logic [3:0] b  /*  ARNG(3:0) */ ,
    output logic [3:0] c  /*  ARNG(3:0) */ );
    logic [3:0]	       d  /*  ARNG(3:0) */ ;
   
    always@*
        begin
        d = a;
	c = (d*b);
        end
    endmodule
