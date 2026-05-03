// Bit merge (concat).
module concat2 (input logic [3:0] a, input logic [3:0] b, output logic [7:0] y);
  assign y = {a, b};
endmodule
