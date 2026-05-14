// Regression #139: array-typed localparam in an included .svh.
// Verible's elaborator silently substituted 32'h0 for dynamic indexes
// into this kind of table; cross-checked against verilator which
// preserved it.  Keep this file alongside svh_array_localparam.sv so
// both frontends see the same include.

localparam logic [31:0] LUT_Q31 [0:7] = '{
  32'd  341782638,
  32'd  256300821,
  32'd  192198501,
  32'd  144128543,
  32'd  108081160,
  32'd   81049436,
  32'd   60778503,
  32'd   45577447
};
