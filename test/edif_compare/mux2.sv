module mux2 (
  input  logic       sel,
  input  logic [3:0] a,
  input  logic [3:0] b,
  output logic [3:0] y
);
  assign y = sel ? a : b;
endmodule
