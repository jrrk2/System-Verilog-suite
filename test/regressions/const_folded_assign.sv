// Constant folder in the SV elaborator (verilator) rewrites
//   assign x = <constant_expr>;
// into
//   INITIAL { Assign(VarRef x, <const>) }
// in the JSON tree, NOT a continuous assign. The Verilator->BIR
// converter has to recover the assignment from this shape; otherwise
// the output port is undriven on that side while the Verible side
// still has the un-folded `assign a = ~2'd3`, and the miter rejects
// the pair.
//
// Found via random_sv_gen --seed 144.
module const_folded_assign (
  input  [1:0] b, c,
  input        d,
  output [1:0] a
);
  assign a = (~2'd3);
endmodule
