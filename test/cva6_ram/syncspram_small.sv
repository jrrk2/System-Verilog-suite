// Small concrete instance of cva6's SyncSpRam for equivalence testing.
//
// 16 words × 8 bits = 128 bits total. Small enough that Vivado may
// not yet infer a full BRAM in synth (likely distributed RAM / LUTRAM
// for the synth flow, plain register array for the -rtl flow).
//
// Wrapping with fixed parameters lets the existing test/edif_compare
// harness drive Vivado without parameter-passing complications.
module syncspram_small (
  input  logic       clk_i,
  input  logic       rst_ni,
  input  logic       cs_i,
  input  logic       we_i,
  input  logic [3:0] addr_i,
  input  logic [7:0] wdata_i,
  output logic [7:0] rdata_o
);
  SyncSpRam #(
    .ADDR_WIDTH (4),
    .DATA_DEPTH (16),
    .DATA_WIDTH (8),
    .OUT_REGS   (0),
    .SIM_INIT   (0)
  ) i_ram (
    .Clk_CI    (clk_i),
    .Rst_RBI   (rst_ni),
    .CSel_SI   (cs_i),
    .WrEn_SI   (we_i),
    .Addr_DI   (addr_i),
    .WrData_DI (wdata_i),
    .RdData_DO (rdata_o)
  );
endmodule
