// Reduction OR: O = |a (1 iff any bit of a is 1).
module redor4 (input logic [3:0] a, output logic y);
  assign y = |a;
endmodule
