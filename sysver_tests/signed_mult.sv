module signed_mult(input clk, input signed [7:0] a,b, output logic [15:0] y);

   always @(posedge clk) y = a*b;

endmodule
