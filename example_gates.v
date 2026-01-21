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

  wire w1, w2, w3, w4;

  // y = (a & b) | c
  AND2 u1 (.A1(a), .A2(b), .ZN(w1));
  OR2 u2 (.A1(w1), .A2(c), .ZN(y));

  // z = sel ? a : b
  AND2 u3 (.A1(sel), .A2(a), .ZN(w2));
  INV u4 (.I(sel), .ZN(w3));
  AND2 u5 (.A1(w3), .A2(b), .ZN(w4));
  OR2 u6 (.A1(w2), .A2(w4), .ZN(z));

endmodule
