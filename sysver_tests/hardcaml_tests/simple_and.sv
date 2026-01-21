module simple_and(
	   output logic y,
	   input logic a,b);

always_comb
begin
   y = a & b;
end

endmodule
