// Arithmetic right shift.
module ashr4 (input logic signed [7:0] a, input logic [2:0] s,
              output logic signed [7:0] y);
  assign y = a >>> s;
endmodule
