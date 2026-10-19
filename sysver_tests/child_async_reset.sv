// A child's async reset is an alias of the parent's.  yosys flattens the
// hierarchy and names the child's pins `u.RST`; the RTLIL importer then
// copy-propagates the alias `u.RST = RST` into the child's reset test but
// used to leave the process header saying `async u.RST`: ffrip could not
// find the reset test in the body, and the writer printed the header's name
// over the body's.  The yosys roundtrip of this design DIFFERed; check_ir
// now rejects a process whose header reset is not what the body tests.
module child_async_reset_sub(input logic CLK, input logic RST, input logic CE,
                             input logic D, output logic Q, output logic [2:0] CNT);
  always @(posedge CLK or posedge RST)
    if (RST) begin
      Q <= 1'b0;
      CNT <= 3'd0;
    end else if (CE) begin
      if (D && CNT != 3'd4) CNT <= CNT + 3'd1;
      else if (!D && CNT != 3'd0) CNT <= CNT - 3'd1;
      if (CNT == 3'd4) Q <= 1'b1;
      else if (CNT == 3'd0) Q <= 1'b0;
    end
endmodule

module child_async_reset(input logic CLK, input logic RST, input logic EN,
                         input logic DIN, output logic DOUT, output logic [2:0] LEVEL,
                         output logic TICK);
  logic q;
  child_async_reset_sub u(.CLK(CLK), .RST(RST), .CE(EN), .D(DIN), .Q(q), .CNT(LEVEL));
  always @(posedge CLK or posedge RST)
    if (RST) TICK <= 1'b0;
    else TICK <= q ^ TICK;
  assign DOUT = q;
endmodule
