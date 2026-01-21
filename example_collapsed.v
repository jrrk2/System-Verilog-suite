module example (
  a,
  b,
  c,
  sel,
  y,
  z
);

  input a;
  input b;
  input c;
  input sel;
  output y;
  output z;

  assign y = w1 | c;
  assign z = w2 | w4;

endmodule
