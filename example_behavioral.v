module example (
  input a,
  input b,
  input c,
  input sel,
  output y,
  output z
);

  // Basic logic operations
  assign y = (a & b) | c;

  // Multiplexer
  assign z = sel ? a : b;

endmodule
