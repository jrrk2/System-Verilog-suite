// Bit-select read `a[N]` — Verible parses as `reference3` (TUPLE3
// with kReference + kSelectVariableDimension). Without the BSlice
// emission, the generic TUPLE3 wrapper recurses into slot 1 and
// drops the index.
module bit_select (
  input  logic [3:0] a,
  output logic       b0,
  output logic       b1,
  output logic       b3
);
  assign b0 = a[0];
  assign b1 = a[1];
  assign b3 = a[3];
endmodule
