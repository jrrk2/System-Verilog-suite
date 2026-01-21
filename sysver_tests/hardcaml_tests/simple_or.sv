module simple_or(
	   output logic y,
	   input logic a,b);

always_comb
begin
   y = a | b;
end

endmodule
