module simple(
	   output logic y,
	   input logic a,b);

always_comb
begin
   y = a ^ b;
end

endmodule
