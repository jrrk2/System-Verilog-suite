module ram64m_top (
  input WCLK, input WE,
  input [5:0] ADDRA, input [5:0] ADDRB, input [5:0] ADDRC, input [5:0] ADDRD,
  input DIA, input DIB, input DIC, input DID,
  output DOA, output DOB, output DOC, output DOD);
  RAM64M u (.WCLK(WCLK), .WE(WE), .ADDRA(ADDRA), .ADDRB(ADDRB), .ADDRC(ADDRC), .ADDRD(ADDRD),
            .DIA(DIA), .DIB(DIB), .DIC(DIC), .DID(DID),
            .DOA(DOA), .DOB(DOB), .DOC(DOC), .DOD(DOD));
endmodule
