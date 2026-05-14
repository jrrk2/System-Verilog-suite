// Regression #139: dynamic index into an array-typed localparam whose
// initializer is in an included .svh.  Distilled from smollm/rope.sv
// line 115 (FREQ_TURNS_Q31[cord_pair[4:0]]) — the smallest input that
// triggers Verible's silent zero-substitution.
//
// Expected (Verilator-frontend BIR): the multiplicand is a slice
//   reference to LUT_Q31 indexed by `sel`, so out_y depends on both
//   `in_x` and `sel`.
//
// Observed (Verible-frontend BIR before fix): the array reference
//   becomes 32'h0, so out_y = in_x * 0 = 0 — DCE then strips the
//   multiplier entirely.
//
// Smoke check (after fix): `sv_decompiler parse verible … | grep -F`
// must show `LUT_Q31` or `lut_lookup`, not `32'h0`.

module svh_array_localparam (
  input  logic         clk,
  input  logic         rst,
  input  logic  [2:0]  sel,
  input  logic [15:0]  in_x,
  output logic [47:0]  out_y
);
  `include "svh_array_localparam.svh"

  logic [31:0] lut_lookup;
  always_comb lut_lookup = LUT_Q31[sel];

  always_ff @(posedge clk) begin
    if (rst) out_y <= 48'd0;
    else     out_y <= {{32{1'b0}}, in_x} * lut_lookup;
  end
endmodule
