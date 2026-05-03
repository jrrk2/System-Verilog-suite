// Reduction AND: O = &a (1 iff all bits of a are 1).
module redand4 (input logic [3:0] a, output logic y);
  assign y = &a;
endmodule
