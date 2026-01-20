module combinational (
  input logic [7:0] a,
  input logic [7:0] b,
  input logic sel,
  output logic [7:0] result
);
  assign result = (sel ? a : b);
endmodule