// Regression #139 (variant): array-typed localparam from an included
// .svh, indexed by a generate-loop genvar — every lookup resolves
// at elaboration time to a specific constant.  No ROM is required;
// constant-folding suffices.  Pairs with svh_array_localparam.sv,
// which is the runtime-indexed (ROM) variant.
//
// Expected: each output slice = LUT_Q31[j] xor zero-extended in_x
//   for j in 0..7, with LUT_Q31[j] resolved to its literal value.
// The Verible pipeline must run the loop-unroll pass to substitute
// the genvar, then a constant-lookup into the array initialiser
// to fold LUT_Q31[j] to its declared 32-bit constant.
//
// Smoke check: `sv_decompiler parse verible ...` should reveal each
// of the eight LUT values appearing in the BIR after elaboration
// (not 32'0).

module svh_array_localparam_unroll (
  input  logic         clk,
  input  logic [15:0]  in_x,
  output logic [255:0] out_y
);
  `include "svh_array_localparam.svh"

  genvar j;
  generate
    for (j = 0; j < 8; j = j + 1) begin : gen_lut
      assign out_y[j*32 +: 32] = LUT_Q31[j] ^ {{16{1'b0}}, in_x};
    end
  endgenerate
endmodule
