// Two-clock module exercising CDC analysis (#137):
//   - q_unsync   : direct cross-domain capture, no synchroniser
//   - q_sync2    : through a 2-FF synchroniser chain (sync_meta -> q_sync2)
//   - q_ctrl     : crossing reaches dst FF via an if-condition (control path)
module cdc_basic (
  input  logic clk_a,
  input  logic clk_b,
  input  logic rst_n,
  input  logic d_a,
  output logic q_a,
  output logic q_unsync,
  output logic q_sync2,
  output logic q_ctrl
);
  logic sync_meta;

  // Source domain: q_a is the only FF on clk_a.
  always_ff @(posedge clk_a) q_a <= d_a;

  // Destination domain (clk_b):
  //   q_unsync captures q_a directly (CDC, unsynchronised).
  always_ff @(posedge clk_b) q_unsync <= q_a;

  //   sync_meta -> q_sync2 is a textbook 2-FF synchroniser.
  always_ff @(posedge clk_b) sync_meta <= q_a;
  always_ff @(posedge clk_b) q_sync2 <= sync_meta;

  //   q_ctrl is gated by q_a (control-path crossing).
  always_ff @(posedge clk_b)
    if (q_a) q_ctrl <= 1'b1;
    else     q_ctrl <= 1'b0;
endmodule
