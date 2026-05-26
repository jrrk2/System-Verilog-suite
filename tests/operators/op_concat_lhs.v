// Concat-LHS with slice writes — picorv32 pcpi_mul carry-save idiom.
// The combinational always-block uses `{carry, sum_slice[w-1:0]} = ...`
// to grab a 4-bit slice and its carry-out from a wider add.  Verible
// emits a concat-lhs; our lowering must rewrite each part as a
// slice-write (or part-sel-write), not as a whole-signal write that
// discards the slice range.  The picorv32 pcpi_mul comb loop comes
// from this lowering treating `next_rd[j+:4] = ...` as
// `next_rd := ...[3:0]` — the whole next_rd reg gets the 4-bit value,
// and 16 unrolled iterations alias the same signal, producing a
// structural cycle.
module op_concat_lhs (
    input  [7:0] a,
    input  [7:0] b,
    output [7:0] y,
    output       co
);
  reg [7:0] r;
  reg       c_lo;
  reg       c_hi;
  always @* begin
    r = 0;
    {c_lo, r[3:0]} = a[3:0] + b[3:0];
    {c_hi, r[7:4]} = a[7:4] + b[7:4] + {3'b0, c_lo};
  end
  assign y  = r;
  assign co = c_hi;
endmodule
