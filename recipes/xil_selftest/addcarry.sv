// 8-bit + 8-bit -> 9-bit add: exercises the CARRY4 carry-out path (widen add to
// result width in behavioral_to_hardcaml) + concat-output fanout in
// flatten_for_z3 (CARRY4 .O/.CO wired to per-bit nets).
module addcarry (input [7:0] a, input [7:0] b, output [8:0] y);
  assign y = a + b;
endmodule
