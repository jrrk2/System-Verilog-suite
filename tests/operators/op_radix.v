module op_radix (output [31:0] y);
  parameter [31:0] PA = 32'h 0010_0000;
  assign y = PA;
endmodule
