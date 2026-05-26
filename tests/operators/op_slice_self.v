// Slice-LHS read-modify-write: `r[3:0] = r[3:0] ^ 4'hF` after `r = in`.
// Catches two regressions at once:
//   (1) the verible side must emit `r[3:0] = ...` as a per-bit
//       slice-write (@slice_write or @part_sel_write_*), not as a
//       whole-signal BAssign with a 4-bit RHS — otherwise the high
//       4 bits of r get zeroed instead of preserved;
//   (2) the SSA pass must version r so the slice-write's RHS reads
//       the *previous* version (`in`) and the new version stitches
//       `in[7:4]` back in via BConcat — otherwise the merged
//       always-block closes the dataflow loop through
//       `Always.Variable.value r` and trips the comb-loop detector.
module op_slice_self (
    input  [7:0] in,
    input        sel,
    output [7:0] out
);
  reg [7:0] r;
  always @* begin
    r = in;
    if (sel) r[3:0] = r[3:0] ^ 4'hf;
  end
  assign out = r;
endmodule
