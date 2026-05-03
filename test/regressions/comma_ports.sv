// Comma-grouped ANSI port declarations: subsequent names in
//   `input  [3:0] a, b,`
//   `output [3:0] sum1, sum2`
// must inherit BOTH the direction AND the width from the preceding
// explicit decl. Verible parses the bare names (b, sum2) as separate
// `port1` siblings of the explicit decl, in REVERSE of source order
// — so the converter has to collect events in DFS order, then iterate
// them as-is (which is reversed back) to apply inheritance correctly.
module comma_ports (
  input  [3:0] a, b,
  output [3:0] sum1, sum2
);
  assign sum1 = a + b;
  assign sum2 = a - b;
endmodule
