module test_and (
  a,
  b,
  y
);

  input a;
  input b;
  output y;


  AND2 u1 (.A1(a), .A2(b), .ZN(y));

endmodule
