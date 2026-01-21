module mapped(input in0,in1, output out1);

   XOR1 xor1 (.a(in0), .b(in1), .y(out1));

endmodule // mapped

module XOR1(input a,b, output y);

   assign y = a^b;

endmodule // mapped

