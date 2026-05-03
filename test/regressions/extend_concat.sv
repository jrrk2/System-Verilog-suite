// The SV elaborator constant-folds nested concatenations like
//   {{{4{1'b0}}, e}, {{7{1'b0}}, c}}
// into the equivalent of EXTEND(CONCAT(e, EXTEND(c))) where each
// EXTEND widens an operand to the next layer's width. The
// JSON->BIR converter has to:
//  (1) emit BConcat with proper zero-pad for each EXTEND/EXTENDS, and
//  (2) compute the operand's bit-width even when that operand is a
//      Concat/Replicate (sv_parse drops the dtype_ref on those).
// Without (1), the EXTEND wrapper is dropped and the result is too
// narrow; without (2), the outer EXTEND's pad calculation is wrong.
//
// On the Verible side, the same expression tests the
// expr_primary_braces2 handler — `{4{1'b0}}` must produce
// BReplicate{count=4}, not be silently swallowed into the parent
// concat as a single 1-bit literal.
//
// Found via random_sv_gen --seed 102.
module extend_concat (
  input  logic       c,
  input  logic [3:0] e,
  output logic [15:0] b
);
  assign b = {{{4{1'b0}}, e}, {{7{1'b0}}, c}};
endmodule
