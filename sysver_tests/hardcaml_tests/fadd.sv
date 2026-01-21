module fadd (input clk, carry, data1, data2, output reg [1:0] sum);

   always @(posedge clk) sum <= {1'b0, data1} + {1'b0, data2} + {1'b0, carry};

endmodule // fadd
