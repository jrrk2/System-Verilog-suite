module ram64m_top (
  input WCLK, input WE,
  input [5:0] ADDRA, input [5:0] ADDRB, input [5:0] ADDRC, input [5:0] ADDRD,
  input DIA, input DIB, input DIC, input DID,
  output DOA, output DOB, output DOC, output DOD);
  reg [63:0] u__mem_a, u__mem_b, u__mem_c, u__mem_d;
  always @(posedge WCLK) if (WE) begin
    u__mem_a[ADDRD] <= DIA; u__mem_b[ADDRD] <= DIB;
    u__mem_c[ADDRD] <= DIC; u__mem_d[ADDRD] <= DID;
  end
  assign DOA = u__mem_a[ADDRA];
  assign DOB = u__mem_b[ADDRB];
  assign DOC = u__mem_c[ADDRC];
  assign DOD = u__mem_d[ADDRD];
endmodule
