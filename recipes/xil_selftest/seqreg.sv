// Multi-bit registered logic: exercises the FF-state-naming alignment
// (fpga_map splits the reg into q__b<i> FDREs; ffpack re-packs them into a bus
// FF, the reg-bitbus resolver ties q__b<i>->q[i], and the undriven-net tie
// grounds the floating FDRE.R / GND).  Internal reg -> output keeps reg and port
// distinct.  NO arithmetic (separate CARRY4 carry-out issue).
module seqreg (input clk, input en, input [7:0] d, output [7:0] y);
  reg [7:0] r;
  always @(posedge clk) if (en) r <= d ^ 8'h3C;
  assign y = r;
endmodule
