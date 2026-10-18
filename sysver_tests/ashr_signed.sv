// `>>>` on operands DECLARED signed (no $signed wrapper): arithmetic, sign
// bits shifted in, all sign bits once the count reaches the width; on an
// unsigned operand it is a logical shift.  A signed shift in a wider context
// sign-extends its left operand first.  The Verible front-end used to emit a
// logical shift for all of these unless the operand was wrapped in $signed.
module ashr_signed (
  input  logic signed [15:0] a,
  input  logic        [15:0] u,
  input  logic        [4:0]  n,
  output logic        [15:0] y_var,     // a >>> n
  output logic        [15:0] y_uns,     // u >>> n (logical)
  output logic signed [23:0] y_wide,    // a >>> n in a 24-bit context
  output logic        [15:0] y_big,     // a >>> (n + 16): all sign bits
  // the same four in procedural code, where the front-end's own shift
  // conversion is used (continuous assigns take another path)
  output logic        [15:0] p_var, p_uns, p_big,
  output logic signed [23:0] p_wide
);
  assign y_var  = a >>> n;
  assign y_uns  = u >>> n;
  assign y_wide = a >>> n;
  assign y_big  = a >>> (n + 5'd16);
  always_comb begin
    p_var  = a >>> n;
    p_uns  = u >>> n;
    p_wide = a >>> n;
    p_big  = a >>> (n + 5'd16);
  end
endmodule
