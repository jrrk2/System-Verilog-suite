// 128-bit constant XOR: guards the BConst.value Z.t / Z.to_int overflow fixes.
// If any synth pass truncated the literal to 63 bits, y's high bits would be
// a^0 instead of a^const_hi and the behavioral==gatemap miter would DIFFER.
module widek (input [127:0] a, output [127:0] y);
  assign y = a ^ 128'hDEADBEEFCAFEBABE0123456789ABCDEF;
endmodule
