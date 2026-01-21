//
// UART 16750
//
// Converted to System Verilog by Jonathan Kimmitt
// This version has been partially checked with formality but some bugs remain
// Original Author:   Sebastian Witt
// Date:     14.03.2019
// Version:  1.6
//
// History:  1.0 - Initial version
//           1.1 - THR empty interrupt register connected to RST
//           1.2 - Registered outputs
//           1.3 - Automatic flow control
//           1.4 - De-assert IIR FIFO64 when FIFO is disabled
//           1.5 - Inverted low active outputs when RST is active
//           1.6 - Converted to System Verilog
//
//
// This code is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This code is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the
// Free Software  Foundation, Inc., 59 Temple Place, Suite 330,
// Boston, MA  02111-1307  USA
//

module apb_uart(
	input wire		CLK,
	input wire		RSTN,
	input wire		PSEL,
	input wire		PENABLE,
	input wire		PWRITE,
	input wire	[2:0] 	PADDR,
	input wire	[31:0] 	PWDATA,
	output logic	[31:0] 	PRDATA,
	output logic		PREADY,
	output logic		PSLVERR,
	output logic		INT,
	output logic		OUT1N,
	output logic		OUT2N,
	output logic		RTSN,
	output logic		DTRN,
	input wire		CTSN,
	input wire		DSRN,
	input wire		DCDN,
	input wire		RIN,
	input wire		SIN,
	output logic		SOUT); // 507
/* design apb_uart */
/* architecture rtl */
typedef enum {FALSE,TRUE} bool_t; // 527
// Declare component uart_transmitter; // 950
// Declare component uart_receiver; // 950
// Declare component uart_interrupt; // 950
// Declare component uart_baudgen; // 950
// Declare component slib_edge_detect; // 950
// Declare component slib_input_sync; // 950
logic iWrite; // 612
logic iRead; // 612
logic iRST; // 612
logic iRBRRead; // 612
logic iTHRWrite; // 612
logic iDLLWrite; // 612
logic iDLMWrite; // 612
logic iIERWrite; // 612
logic iIIRRead; // 612
logic iFCRWrite; // 612
logic iLCRWrite; // 612
logic iMCRWrite; // 612
logic iLSRRead; // 612
logic iMSRRead; // 612
logic iSCRWrite; // 612
logic [7:0] iTSR; // 605
logic [7:0] iRBR; // 605
logic [7:0] iDLL; // 605
logic [7:0] iDLM; // 605
logic [7:0] iIER; // 605
logic [7:0] iIIR; // 605
logic [7:0] iFCR; // 605
logic [7:0] iLCR; // 605
logic [7:0] iMCR; // 605
logic [7:0] iLSR; // 605
logic [7:0] iMSR; // 605
logic [7:0] iSCR; // 605
logic iIER_ERBI; // 612
logic iIER_ETBEI; // 612
logic iIER_ELSI; // 612
logic iIER_EDSSI; // 612
logic iIIR_PI; // 612
logic iIIR_ID0; // 612
logic iIIR_ID1; // 612
logic iIIR_ID2; // 612
logic iIIR_FIFO64; // 612
logic iFCR_FIFOEnable; // 612
logic iFCR_RXFIFOReset; // 612
logic iFCR_TXFIFOReset; // 612
logic iFCR_DMAMode; // 612
logic iFCR_FIFO64E; // 612
logic [1:0] iFCR_RXTrigger; // 605
logic [1:0] iLCR_WLS; // 605
logic iLCR_STB; // 612
logic iLCR_PEN; // 612
logic iLCR_EPS; // 612
logic iLCR_SP; // 612
logic iLCR_BC; // 612
logic iLCR_DLAB; // 612
logic iMCR_DTR; // 612
logic iMCR_RTS; // 612
logic iMCR_OUT1; // 612
logic iMCR_OUT2; // 612
logic iMCR_LOOP; // 612
logic iMCR_AFE; // 612
logic iLSR_DR; // 612
logic iLSR_OE; // 612
logic iLSR_PE; // 612
logic iLSR_FE; // 612
logic iLSR_BI; // 612
logic iLSR_THRE; // 612
logic iLSR_THRNF; // 612
logic iLSR_TEMT; // 612
logic iLSR_FIFOERR; // 612
logic iMSR_dCTS; // 612
logic iMSR_dDSR; // 612
logic iMSR_TERI; // 612
logic iMSR_dDCD; // 612
logic iMSR_CTS; // 612
logic iMSR_DSR; // 612
logic iMSR_RI; // 612
logic iMSR_DCD; // 612
logic iCTSNs; // 612
logic iDSRNs; // 612
logic iDCDNs; // 612
logic iRINs; // 612
logic iCTSn; // 612
logic iDSRn; // 612
logic iDCDn; // 612
logic iRIn; // 612
logic iCTSnRE; // 612
logic iCTSnFE; // 612
logic iDSRnRE; // 612
logic iDSRnFE; // 612
logic iDCDnRE; // 612
logic iDCDnFE; // 612
logic iRInRE; // 612
logic iRInFE; // 612
logic [15:0] iBaudgenDiv; // 605
logic iBaudtick16x; // 612
logic iBaudtick2x; // 612
logic iRCLK; // 612
logic iBAUDOUTN; // 612
logic iTXFIFOClear; // 612
logic iTXFIFOWrite; // 612
logic iTXFIFORead; // 612
logic iTXFIFOEmpty; // 612
logic iTXFIFOFull; // 612
logic iTXFIFO16Full; // 612
logic iTXFIFO64Full; // 612
logic [5:0] iTXFIFOUsage; // 605
logic [7:0] iTXFIFOQ; // 605
logic iRXFIFOClear; // 612
logic iRXFIFOWrite; // 612
logic iRXFIFORead; // 612
logic iRXFIFOEmpty; // 612
logic iRXFIFOFull; // 612
logic iRXFIFO16Full; // 612
logic iRXFIFO64Full; // 612
logic [10:0] iRXFIFOD; // 605
logic [10:0] iRXFIFOQ; // 605
logic [5:0] iRXFIFOUsage; // 605
logic iRXFIFOTrigger; // 612
logic iRXFIFO16Trigger; // 612
logic iRXFIFO64Trigger; // 612
logic iRXFIFOPE; // 612
logic iRXFIFOFE; // 612
logic iRXFIFOBI; // 612
logic iSOUT; // 612
logic iTXStart; // 612
logic iTXClear; // 612
logic iTXFinished; // 612
logic iTXRunning; // 612
logic iSINr; // 612
logic iSIN; // 612
logic iRXFinished; // 612
logic iRXClear; // 612
logic [7:0] iRXData; // 605
logic iRXPE; // 612
logic iRXFE; // 612
logic iRXBI; // 612
logic iFERE; // 612
logic iPERE; // 612
logic iBIRE; // 612
logic [6:0] iFECounter; // 900
logic iFEIncrement; // 612
logic iFEDecrement; // 612
logic iRDAInterrupt; // 612
logic [5:0] iTimeoutCount; // 605
logic iCharTimeout; // 612
logic iLSR_THRERE; // 612
logic iTHRInterrupt; // 612
logic iTXEnable; // 612
logic iRTS; // 612
assign /*903*/ iWrite = (PSEL ==  1'b1 && PENABLE ==  1'b1) && PWRITE ==  1'b1 ?  1'b1 :   1'b0; // 905
assign /*903*/ iRead = (PSEL ==  1'b1 && PENABLE ==  1'b1) && PWRITE ==  1'b0 ?  1'b1 :   1'b0; // 905
assign /*903*/ iRST = RSTN ==  1'b0 ?  1'b1 :   1'b0; // 905
assign /*903*/ iRBRRead = (iRead ==  1'b1 && PADDR == 3'b000) && iLCR_DLAB ==  1'b0 ?  1'b1 :   1'b0; // 905
assign /*903*/ iTHRWrite = (iWrite ==  1'b1 && PADDR == 3'b000) && iLCR_DLAB ==  1'b0 ?  1'b1 :   1'b0; // 905
assign /*903*/ iDLLWrite = (iWrite ==  1'b1 && PADDR == 3'b000) && iLCR_DLAB ==  1'b1 ?  1'b1 :   1'b0; // 905
assign /*903*/ iDLMWrite = (iWrite ==  1'b1 && PADDR == 3'b001) && iLCR_DLAB ==  1'b1 ?  1'b1 :   1'b0; // 905
assign /*903*/ iIERWrite = (iWrite ==  1'b1 && PADDR == 3'b001) && iLCR_DLAB ==  1'b0 ?  1'b1 :   1'b0; // 905
assign /*903*/ iIIRRead = iRead ==  1'b1 && PADDR == 3'b010 ?  1'b1 :   1'b0; // 905
assign /*903*/ iFCRWrite = iWrite ==  1'b1 && PADDR == 3'b010 ?  1'b1 :   1'b0; // 905
assign /*903*/ iLCRWrite = iWrite ==  1'b1 && PADDR == 3'b011 ?  1'b1 :   1'b0; // 905
assign /*903*/ iMCRWrite = iWrite ==  1'b1 && PADDR == 3'b100 ?  1'b1 :   1'b0; // 905
assign /*903*/ iLSRRead = iRead ==  1'b1 && PADDR == 3'b101 ?  1'b1 :   1'b0; // 905
assign /*903*/ iMSRRead = iRead ==  1'b1 && PADDR == 3'b110 ?  1'b1 :   1'b0; // 905
assign /*903*/ iSCRWrite = iWrite ==  1'b1 && PADDR == 3'b111 ?  1'b1 :   1'b0; // 905
slib_input_sync UART_IS_SIN (CLK,iRST,SIN,iSINr); // 879
slib_input_sync UART_IS_CTS (CLK,iRST,CTSN,iCTSNs); // 879
slib_input_sync UART_IS_DSR (CLK,iRST,DSRN,iDSRNs); // 879
slib_input_sync UART_IS_DCD (CLK,iRST,DCDN,iDCDNs); // 879
slib_input_sync UART_IS_RI (CLK,iRST,RIN,iRINs); // 879
slib_input_filter #(.SIZE(2)) UART_IF_CTS (CLK,iRST,iBaudtick2x,iCTSNs,iCTSn); // 879
slib_input_filter #(.SIZE(2)) UART_IF_DSR (CLK,iRST,iBaudtick2x,iDSRNs,iDSRn); // 879
slib_input_filter #(.SIZE(2)) UART_IF_DCD (CLK,iRST,iBaudtick2x,iDCDNs,iDCDn); // 879
slib_input_filter #(.SIZE(2)) UART_IF_RI (CLK,iRST,iBaudtick2x,iRINs,iRIn); // 879

always @(posedge CLK or posedge iRST)
  if ((iRST ==  1'b1))
    begin
       /* block const 263 */
       iDLL <= (0<<7)|(0<<6)|(0<<5)|(0<<4)|(0<<3)|(0<<2)|(0<<1)|(0<<0);
       /* block const 263 */
       iDLM <= (0<<7)|(0<<6)|(0<<5)|(0<<4)|(0<<3)|(0<<2)|(0<<1)|(0<<0);
    end
  else
    begin
       if ((iDLLWrite ==  1'b1))
         begin
            iDLL <= PWDATA[7:0] ; // 413
         end
       if ((iDLMWrite ==  1'b1))
         begin
            iDLM <= PWDATA[7:0] ; // 413
         end
    end

always @(posedge CLK or posedge iRST)
  if ((iRST ==  1'b1))
    begin
       iIER[3:0] <= 0;
    end
  else
    begin
       if ((iIERWrite ==  1'b1))
         begin
            iIER[3:0] <= PWDATA[3:0];
         end
    end

assign /*432*/ iIER_ERBI = iIER[0]; // 434
assign /*432*/ iIER_ETBEI = iIER[1]; // 434
assign /*432*/ iIER_ELSI = iIER[2]; // 434
assign /*432*/ iIER_EDSSI = iIER[3]; // 434
assign iIER[7:4] = 0;

uart_interrupt UART_IIC (
	.CLK(CLK),
	.RST(iRST),
	.IER(iIER[3:0] ),
	.LSR(iLSR[4:0] ),
	.THI(iTHRInterrupt),
	.RDA(iRDAInterrupt),
	.CTI(iCharTimeout),
	.AFE(iMCR_AFE),
	.MSR(iMSR[3:0] ),
	.IIR(iIIR[3:0] ),
	.INT(INT)); // 879
slib_edge_detect UART_IIC_THRE_ED (
	.CLK(CLK),
	.RST(iRST),
	.D(iLSR_THRE),
	.RE(iLSR_THRERE),
        .FE()); // 879

always @(posedge CLK or posedge iRST)
  if ((iRST ==  1'b1))
    begin
       iTHRInterrupt <=  1'b0; // 413
    end
  else
    begin
       if (((iLSR_THRERE ==  1'b1 | iFCR_TXFIFOReset ==  1'b1) | ((iIERWrite ==  1'b1 && PWDATA[1] ==  1'b1) && iLSR_THRE ==  1'b1)))
         begin
            iTHRInterrupt <=  1'b1; // 413
         end
       else if     (((iIIRRead ==  1'b1 && iIIR[3:1]  == 3'b001) | iTHRWrite ==  1'b1))
         begin
            iTHRInterrupt <=  1'b0; // 413
         end
    end

assign /*903*/ iRDAInterrupt = (iFCR_FIFOEnable ==  1'b0 && iLSR_DR ==  1'b1) | (iFCR_FIFOEnable ==  1'b1 && iRXFIFOTrigger ==  1'b1) ?  1'b1 :   1'b0; // 905
assign /*432*/ iIIR_PI = iIIR[0]; // 434
assign /*432*/ iIIR_ID0 = iIIR[1]; // 434
assign /*432*/ iIIR_ID1 = iIIR[2]; // 434
assign /*432*/ iIIR_ID2 = iIIR[3]; // 434
assign /*432*/ iIIR_FIFO64 = iIIR[5]; // 434
assign iIIR[4] = 0;
assign iIIR[5] = iFCR_FIFOEnable ? iFCR_FIFO64E : 1'b0;
assign iIIR[6] = iFCR_FIFOEnable;
assign iIIR[7] = iFCR_FIFOEnable;
   
always @(posedge CLK or posedge iRST)
  if ((iRST ==  1'b1))
    begin
       /* block const 263 */
       iTimeoutCount <= (0<<5)|(0<<4)|(0<<3)|(0<<2)|(0<<1)|(0<<0);
       iCharTimeout <=  1'b0; // 413
    end
  else
    begin
       if (((iRXFIFOEmpty ==  1'b1 | iRBRRead ==  1'b1) | iRXFIFOWrite ==  1'b1))
         begin
            /* block const 263 */
            iTimeoutCount <= (0<<5)|(0<<4)|(0<<3)|(0<<2)|(0<<1)|(0<<0);
         end
       else if     (((iRXFIFOEmpty ==  1'b0 && iBaudtick2x ==  1'b1) && iTimeoutCount[5] ==  1'b0))
         begin
            iTimeoutCount <= iTimeoutCount + 1; // 413
         end
       if ((iFCR_FIFOEnable ==  1'b1))
         begin
            if ((iRBRRead ==  1'b1))
              begin
                 iCharTimeout <=  1'b0; // 413
              end
            else if         ((iTimeoutCount[5] ==  1'b1))
              begin
                 iCharTimeout <=  1'b1; // 413
              end
         end
       else
         begin
            iCharTimeout <=  1'b0; // 413
         end
    end

always @(posedge CLK or posedge iRST)
  if ((iRST ==  1'b1))
    begin
       iFCR_FIFOEnable <=  1'b0; // 413
       iFCR_RXFIFOReset <=  1'b0; // 413
       iFCR_TXFIFOReset <=  1'b0; // 413
       iFCR_DMAMode <=  1'b0; // 413
       iFCR_FIFO64E <=  1'b0; // 413
       /* block const 263 */
       iFCR_RXTrigger <= (0<<1)|(0<<0);
    end
  else
    begin
       iFCR_RXFIFOReset <=  1'b0; // 413
       iFCR_TXFIFOReset <=  1'b0; // 413
       if ((iFCRWrite ==  1'b1))
         begin
            iFCR_FIFOEnable <= PWDATA[0]; // 413
            iFCR_DMAMode <= PWDATA[3]; // 413
            iFCR_RXTrigger <= PWDATA[7:6] ; // 413
            if ((iLCR_DLAB ==  1'b1))
              begin
                 iFCR_FIFO64E <= PWDATA[5]; // 413
              end
            
            if (((PWDATA[1] ==  1'b1 | (iFCR_FIFOEnable ==  1'b0 && PWDATA[0] ==  1'b1)) | (iFCR_FIFOEnable ==  1'b1 && PWDATA[0] ==  1'b0)))
              begin
                 iFCR_RXFIFOReset <=  1'b1; // 413
              end
            
            if (((PWDATA[2] ==  1'b1 | (iFCR_FIFOEnable ==  1'b0 && PWDATA[0] ==  1'b1)) | (iFCR_FIFOEnable ==  1'b1 && PWDATA[0] ==  1'b0)))
              begin
                 iFCR_TXFIFOReset <=  1'b1; // 413
              end
         end
    end

assign    iFCR[0] = iFCR_FIFOEnable;
assign    iFCR[1] = iFCR_RXFIFOReset;
assign    iFCR[2] = iFCR_TXFIFOReset;
assign    iFCR[3] = iFCR_DMAMode;
assign    iFCR[4] = 1'b0;
assign    iFCR[5] = iFCR_FIFO64E;
assign    iFCR[7:6] = iFCR_RXTrigger;

always @(posedge CLK or posedge iRST)
  if ((iRST ==  1'b1))
    begin
       /* block const 263 */
       iLCR <= (0<<7)|(0<<6)|(0<<5)|(0<<4)|(0<<3)|(0<<2)|(0<<1)|(0<<0);
    end
  else
    begin
       if ((iLCRWrite ==  1'b1))
         begin
            iLCR <= PWDATA[7:0] ; // 413
         end
    end

assign /*432*/ iLCR_WLS = iLCR[1:0] ; // 434
assign /*432*/ iLCR_STB = iLCR[2]; // 434
assign /*432*/ iLCR_PEN = iLCR[3]; // 434
assign /*432*/ iLCR_EPS = iLCR[4]; // 434
assign /*432*/ iLCR_SP = iLCR[5]; // 434
assign /*432*/ iLCR_BC = iLCR[6]; // 434
assign /*432*/ iLCR_DLAB = iLCR[7]; // 434

always @(posedge CLK or posedge iRST)
  if ((iRST ==  1'b1))
    begin
       iMCR[5:0] <= 0;
    end
  else
    begin
       if ((iMCRWrite ==  1'b1))
         begin
            iMCR[5:0] <= PWDATA[5:0];
         end
    end

assign /*432*/ iMCR_DTR = iMCR[0]; // 434
assign /*432*/ iMCR_RTS = iMCR[1]; // 434
assign /*432*/ iMCR_OUT1 = iMCR[2]; // 434
assign /*432*/ iMCR_OUT2 = iMCR[3]; // 434
assign /*432*/ iMCR_LOOP = iMCR[4]; // 434
assign /*432*/ iMCR_AFE = iMCR[5]; // 434
assign iMCR[7:6] = 0;

always @(posedge CLK or posedge iRST)
  if ((iRST ==  1'b1))
    begin
       iLSR_OE <=  1'b0; // 413
       iLSR_PE <=  1'b0; // 413
       iLSR_FE <=  1'b0; // 413
       iLSR_BI <=  1'b0; // 413
       iFECounter <= 0; // 413
       iLSR_FIFOERR <=  1'b0; // 413
    end
  else
    begin
       if ((((iFCR_FIFOEnable ==  1'b0 && iLSR_DR ==  1'b1) && iRXFinished ==  1'b1) | ((iFCR_FIFOEnable ==  1'b1 && iRXFIFOFull ==  1'b1) && iRXFinished ==  1'b1)))
         begin
            iLSR_OE <=  1'b1; // 413
         end
       else if     ((iLSRRead ==  1'b1))
         begin
            iLSR_OE <=  1'b0; // 413
         end
       if ((iPERE ==  1'b1))
         begin
            iLSR_PE <=  1'b1; // 413
         end
       else if     ((iLSRRead ==  1'b1))
         begin
            iLSR_PE <=  1'b0; // 413
         end
       if ((iFERE ==  1'b1))
         begin
            iLSR_FE <=  1'b1; // 413
         end
       else if     ((iLSRRead ==  1'b1))
         begin
            iLSR_FE <=  1'b0; // 413
         end
       if ((iBIRE ==  1'b1))
         begin
            iLSR_BI <=  1'b1; // 413
         end
       else if     ((iLSRRead ==  1'b1))
         begin
            iLSR_BI <=  1'b0; // 413
         end
       if ((iFECounter != 0))
         begin
            iLSR_FIFOERR <=  1'b1; // 413
         end
       else if     ((iRXFIFOEmpty ==  1'b1 | iRXFIFOQ[10:8]  == 3'b000))
         begin
            iLSR_FIFOERR <=  1'b0; // 413
         end
       if ((iRXFIFOClear ==  1'b1))
         begin
            iFECounter <= 0; // 413
         end
       else
         begin
            if ((iFEIncrement ==  1'b1 && iFEDecrement ==  1'b0))
              begin
                 iFECounter <= iFECounter + 1; // 413
              end
            else if         ((iFEIncrement ==  1'b0 && iFEDecrement ==  1'b1))
              begin
                 iFECounter <= iFECounter - 1; // 413
              end
         end
    end

assign /*903*/ iRXFIFOPE = iRXFIFOEmpty ==  1'b0 && iRXFIFOQ[8] ==  1'b1 ?  1'b1 :   1'b0; // 905
assign /*903*/ iRXFIFOFE = iRXFIFOEmpty ==  1'b0 && iRXFIFOQ[9] ==  1'b1 ?  1'b1 :   1'b0; // 905
assign /*903*/ iRXFIFOBI = iRXFIFOEmpty ==  1'b0 && iRXFIFOQ[10] ==  1'b1 ?  1'b1 :   1'b0; // 905
slib_edge_detect UART_PEDET (.CLK,.RST(iRST),.D(iRXFIFOPE),.RE(iPERE),.FE()); // 879
slib_edge_detect UART_FEDET (.CLK,.RST(iRST),.D(iRXFIFOFE),.RE(iFERE),.FE()); // 879
slib_edge_detect UART_BIDET (.CLK,.RST(iRST),.D(iRXFIFOBI),.RE(iBIRE),.FE()); // 879
assign /*903*/ iFEIncrement = iRXFIFOWrite ==  1'b1 && iRXFIFOD[10:8]  != 3'b000 ?  1'b1 :   1'b0; // 905
assign /*903*/ iFEDecrement = (iFECounter != 0 && iRXFIFOEmpty ==  1'b0) && ((iPERE ==  1'b1 | iFERE ==  1'b1) | iBIRE ==  1'b1) ?  1'b1 :   1'b0; // 905
assign     iLSR[0]         = iLSR_DR;
assign     iLSR[1]         = iLSR_OE;
assign     iLSR[2]         = iLSR_PE;
assign     iLSR[3]         = iLSR_FE;
assign     iLSR[4]         = iLSR_BI;
assign     iLSR[5]         = iLSR_THRNF;
assign     iLSR[6]         = iLSR_TEMT;
assign     iLSR[7]         = iFCR_FIFOEnable && iLSR_FIFOERR;
   
assign /*903*/ iLSR_DR = iRXFIFOEmpty ==  1'b0 | iRXFIFOWrite ==  1'b1 ?  1'b1 :   1'b0; // 905
assign /*903*/ iLSR_THRE = iTXFIFOEmpty ==  1'b1 ?  1'b1 :   1'b0; // 905
assign /*903*/ iLSR_TEMT = iTXRunning ==  1'b0 && iLSR_THRE ==  1'b1 ?  1'b1 :   1'b0; // 905
assign /*903*/ iLSR_THRNF = ((iFCR_FIFOEnable ==  1'b0 && iTXFIFOEmpty ==  1'b1) | (iFCR_FIFOEnable ==  1'b1 && iTXFIFOFull ==  1'b0)) ?  1'b1 :   1'b0; // 905
assign /*903*/ iMSR_CTS = (iMCR_LOOP ==  1'b1 && iRTS ==  1'b1) | (iMCR_LOOP ==  1'b0 && iCTSn ==  1'b0) ?  1'b1 :   1'b0; // 905
assign /*903*/ iMSR_DSR = (iMCR_LOOP ==  1'b1 && iMCR_DTR ==  1'b1) | (iMCR_LOOP ==  1'b0 && iDSRn ==  1'b0) ?  1'b1 :   1'b0; // 905
assign /*903*/ iMSR_RI = (iMCR_LOOP ==  1'b1 && iMCR_OUT1 ==  1'b1) | (iMCR_LOOP ==  1'b0 && iRIn ==  1'b0) ?  1'b1 :   1'b0; // 905
assign /*903*/ iMSR_DCD = (iMCR_LOOP ==  1'b1 && iMCR_OUT2 ==  1'b1) | (iMCR_LOOP ==  1'b0 && iDCDn ==  1'b0) ?  1'b1 :   1'b0; // 905
slib_edge_detect UART_ED_CTS (
	.CLK(CLK),
	.RST(iRST),
	.D(iMSR_CTS),
	.RE(iCTSnRE),
	.FE(iCTSnFE)); // 879
slib_edge_detect UART_ED_DSR (
	.CLK(CLK),
	.RST(iRST),
	.D(iMSR_DSR),
	.RE(iDSRnRE),
	.FE(iDSRnFE)); // 879
slib_edge_detect UART_ED_RI (
	.CLK(CLK),
	.RST(iRST),
	.D(iMSR_RI),
	.RE(iRInRE),
	.FE(iRInFE)); // 879
slib_edge_detect UART_ED_DCD (
	.CLK(CLK),
	.RST(iRST),
	.D(iMSR_DCD),
	.RE(iDCDnRE),
	.FE(iDCDnFE)); // 879

always @(posedge CLK or posedge iRST)
  if ((iRST ==  1'b1))
    begin
       iMSR_dCTS <=  1'b0; // 413
       iMSR_dDSR <=  1'b0; // 413
       iMSR_TERI <=  1'b0; // 413
       iMSR_dDCD <=  1'b0; // 413
    end
  else
    begin
       if ((iCTSnRE ==  1'b1 | iCTSnFE ==  1'b1))
         begin
            iMSR_dCTS <=  1'b1; // 413
         end
       else if     ((iMSRRead ==  1'b1))
         begin
            iMSR_dCTS <=  1'b0; // 413
         end
       if ((iDSRnRE ==  1'b1 | iDSRnFE ==  1'b1))
         begin
            iMSR_dDSR <=  1'b1; // 413
         end
       else if     ((iMSRRead ==  1'b1))
         begin
            iMSR_dDSR <=  1'b0; // 413
         end
       if ((iRInFE ==  1'b1))
         begin
            iMSR_TERI <=  1'b1; // 413
         end
       else if     ((iMSRRead ==  1'b1))
         begin
            iMSR_TERI <=  1'b0; // 413
         end
       if ((iDCDnRE ==  1'b1 | iDCDnFE ==  1'b1))
         begin
            iMSR_dDCD <=  1'b1; // 413
         end
       else if     ((iMSRRead ==  1'b1))
         begin
            iMSR_dDCD <=  1'b0; // 413
         end
    end

assign    iMSR[0]     = iMSR_dCTS;
assign    iMSR[1]     = iMSR_dDSR;
assign    iMSR[2]     = iMSR_TERI;
assign    iMSR[3]     = iMSR_dDCD;
assign    iMSR[4]     = iMSR_CTS;
assign    iMSR[5]     = iMSR_DSR;
assign    iMSR[6]     = iMSR_RI;
assign    iMSR[7]     = iMSR_DCD;

always @(posedge CLK or posedge iRST)
  if ((iRST ==  1'b1))
    begin
       /* block const 263 */
       iSCR <= (0<<7)|(0<<6)|(0<<5)|(0<<4)|(0<<3)|(0<<2)|(0<<1)|(0<<0);
    end
  else
    begin
       if ((iSCRWrite ==  1'b1))
         begin
            iSCR <= PWDATA[7:0] ; // 413
         end
    end

assign /*432*/ iBaudgenDiv = {iDLM, iDLL}
; // 434
uart_baudgen UART_BG16 (
	.CLK(CLK),
	.RST(iRST),
	.CE( 1'b1),
	.CLEAR( 1'b0),
	.DIVIDER(iBaudgenDiv),
	.BAUDTICK(iBaudtick16x)); // 879
slib_clock_div #(.RATIO(8)) UART_BG2 (
	.CLK(CLK),
	.RST(iRST),
	.CE(iBaudtick16x),
	.Q(iBaudtick2x)); // 879
slib_edge_detect UART_RCLK (
	.CLK(CLK),
	.RST(iRST),
	.D(iBAUDOUTN),
	.RE(iRCLK),
        .FE()); // 879
slib_fifo #(.WIDTH(8), .SIZE_E(6)) UART_TXFF (
	.CLK(CLK),
	.RST(iRST),
	.CLEAR(iTXFIFOClear),
	.WRITE(iTXFIFOWrite),
	.READ(iTXFIFORead),
	.D(PWDATA[7:0] ),
	.Q(iTXFIFOQ),
	.EMPTY(iTXFIFOEmpty),
	.FULL(iTXFIFO64Full),
	.USAGE(iTXFIFOUsage)); // 879
assign /*432*/ iTXFIFO16Full = iTXFIFOUsage[4]; // 434
assign /*903*/ iTXFIFOFull = iFCR_FIFO64E ==  1'b0 ? iTXFIFO16Full :  iTXFIFO64Full; // 905
assign /*903*/ iTXFIFOWrite = ((iFCR_FIFOEnable ==  1'b0 && iTXFIFOEmpty ==  1'b1) | (iFCR_FIFOEnable ==  1'b1 && iTXFIFOFull ==  1'b0)) && iTHRWrite ==  1'b1 ?  1'b1 :   1'b0; // 905
assign /*903*/ iTXFIFOClear = iFCR_TXFIFOReset ==  1'b1 ?  1'b1 :   1'b0; // 905
slib_fifo #(.WIDTH(11), .SIZE_E(6)) UART_RXFF (
	.CLK(CLK),
	.RST(iRST),
	.CLEAR(iRXFIFOClear),
	.WRITE(iRXFIFOWrite),
	.READ(iRXFIFORead),
	.D(iRXFIFOD),
	.Q(iRXFIFOQ),
	.EMPTY(iRXFIFOEmpty),
	.FULL(iRXFIFO64Full),
	.USAGE(iRXFIFOUsage)); // 879
assign /*903*/ iRXFIFORead = iRBRRead ==  1'b1 ?  1'b1 :   1'b0; // 905
assign /*432*/ iRXFIFO16Full = iRXFIFOUsage[4]; // 434
assign /*903*/ iRXFIFOFull = iFCR_FIFO64E ==  1'b0 ? iRXFIFO16Full :  iRXFIFO64Full; // 905
assign /*432*/ iRBR = iRXFIFOQ[7:0] ; // 434
assign /*903*/ iRXFIFO16Trigger = ((((iFCR_RXTrigger == 2'b00 && iRXFIFOEmpty ==  1'b0) | (iFCR_RXTrigger == 2'b01 && (iRXFIFOUsage[2] ==  1'b1 | iRXFIFOUsage[3] ==  1'b1))) | (iFCR_RXTrigger == 2'b10 && iRXFIFOUsage[3] ==  1'b1)) | (((iFCR_RXTrigger == 2'b11 && iRXFIFOUsage[3] ==  1'b1) && iRXFIFOUsage[2] ==  1'b1) && iRXFIFOUsage[1] ==  1'b1)) | iRXFIFO16Full ==  1'b1 ?  1'b1 :   1'b0; // 905
assign /*903*/ iRXFIFO64Trigger = ((((iFCR_RXTrigger == 2'b00 && iRXFIFOEmpty ==  1'b0) | (iFCR_RXTrigger == 2'b01 && (iRXFIFOUsage[4] ==  1'b1 | iRXFIFOUsage[5] ==  1'b1))) | (iFCR_RXTrigger == 2'b10 && iRXFIFOUsage[5] ==  1'b1)) | (((iFCR_RXTrigger == 2'b11 && iRXFIFOUsage[5] ==  1'b1) && iRXFIFOUsage[4] ==  1'b1) && iRXFIFOUsage[3] ==  1'b1)) | iRXFIFO64Full ==  1'b1 ?  1'b1 :   1'b0; // 905
assign /*903*/ iRXFIFOTrigger = iFCR_FIFO64E ==  1'b0 ? iRXFIFO16Trigger :  iRXFIFO64Trigger; // 905
uart_transmitter UART_TX (
	.CLK(CLK),
	.RST(iRST),
	.TXCLK(iBaudtick2x),
	.TXSTART(iTXStart),
	.CLEAR(iTXClear),
	.WLS(iLCR_WLS),
	.STB(iLCR_STB),
	.PEN(iLCR_PEN),
	.EPS(iLCR_EPS),
	.SP(iLCR_SP),
	.BC(iLCR_BC),
	.DIN(iTSR),
	.TXFINISHED(iTXFinished),
	.SOUT(iSOUT)); // 879
assign /*432*/ iTXClear =  1'b0; // 434
uart_receiver UART_RX (
	.CLK(CLK),
	.RST(iRST),
	.RXCLK(iRCLK),
	.RXCLEAR(iRXClear),
	.WLS(iLCR_WLS),
	.STB(iLCR_STB),
	.PEN(iLCR_PEN),
	.EPS(iLCR_EPS),
	.SP(iLCR_SP),
	.SIN(iSIN),
	.PE(iRXPE),
	.FE(iRXFE),
	.BI(iRXBI),
	.DOUT(iRXData),
	.RXFINISHED(iRXFinished)); // 879
assign /*432*/ iRXClear =  1'b0; // 434
assign /*903*/ iSIN = iMCR_LOOP ==  1'b0 ? iSINr :  iSOUT; // 905
assign /*903*/ iTXEnable = iTXFIFOEmpty ==  1'b0 && (iMCR_AFE ==  1'b0 | (iMCR_AFE ==  1'b1 && iMSR_CTS ==  1'b1)) ?  1'b1 :   1'b0; // 905

   typedef enum logic [1:0] {TXIDLE, TXSTART, TXRUN, TXEND} tx_state_type;
   typedef enum logic {RXIDLE, RXSAVE} rx_state_type;
   
   rx_state_type rx_State;
   tx_state_type tx_State;
   
    // Transmitter process
    always @ (posedge CLK or posedge iRST)
        if (iRST == 1'b1)
          begin
            tx_State    <= TXIDLE;
            iTSR        <= 0;
            iTXStart    <= 1'b0;
            iTXFIFORead <= 1'b0;
            iTXRunning  <= 1'b0;
          end
        else
          begin
            // Defaults
            iTXStart    <= 1'b0;
            iTXFIFORead <= 1'b0;
            iTXRunning  <= 1'b0;

            case(tx_State)
                TXIDLE       :
                  begin
                     if (iTXEnable == 1'b1)
                       begin
                          iTXStart <= 1'b1;            // Start transmitter
                          tx_State <= TXSTART;
                       end
                     else
                       tx_State <= TXIDLE;
                  end
                TXSTART    :
                  begin
                     iTSR <= iTXFIFOQ;
                     iTXStart <= 1'b1;                // Start transmitter
                     iTXFIFORead <= 1'b1;             // Increment TX FIFO read counter
                     tx_State <= TXRUN;
                  end
                TXRUN      :
                  begin
                     if (iTXFinished == 1'b1)     // TX finished
                       tx_State <= TXEND;
                     else
                       tx_State <= TXRUN;
                     iTXRunning <= 1'b1;
                     iTXStart   <= 1'b1;
                  end
                TXEND      :  tx_State <= TXIDLE;
                default    :  tx_State <= TXIDLE;
            endcase;
    end

    // Receiver process
    always @(posedge CLK or posedge iRST)
        if (iRST == 1'b1)
          begin
            rx_State        <= RXIDLE;
            iRXFIFOWrite <= 1'b0;
            iRXFIFOClear <= 1'b0;
            iRXFIFOD     <= 0;
          end
        else
          begin
            // Defaults
            iRXFIFOWrite <= 1'b0;
            iRXFIFOClear <= iFCR_RXFIFOReset;

            case (rx_State)
                RXIDLE       :
                  begin
                     if (iRXFinished == 1'b1)
                       begin // Receive finished
                          iRXFIFOD <= {iRXBI, iRXFE, iRXPE, iRXData};
                          if (iFCR_FIFOEnable == 1'b0)
                            iRXFIFOClear <= 1'b1;    // Non-FIFO mode
                          rx_State <= RXSAVE;
                       end
                     else
                       rx_State <= RXIDLE;
                  end
                RXSAVE    :
                  begin
                     if (iFCR_FIFOEnable == 1'b0)
                       iRXFIFOWrite <= 1'b1;        // Non-FIFO mode: Overwrite
                     else if (iRXFIFOFull == 1'b0)
                       iRXFIFOWrite <= 1'b1;        // FIFO mode
                     rx_State <= RXIDLE;
                  end
                default    :  rx_State <= RXIDLE;
             endcase; // case rx_State
          end

always @(posedge CLK or posedge iRST)
  if ((iRST ==  1'b1))
    begin
       iRTS <=  1'b0; // 413
    end
  else
    begin
       if ((iMCR_RTS ==  1'b0 | (iMCR_AFE ==  1'b1 && iRXFIFOTrigger ==  1'b1)))
         begin
            iRTS <=  1'b0; // 413
         end
       else if     ((iMCR_RTS ==  1'b1 && (iMCR_AFE ==  1'b0 | (iMCR_AFE ==  1'b1 && iRXFIFOEmpty ==  1'b1))))
         begin
            iRTS <=  1'b1; // 413
         end
    end

always @(posedge CLK or posedge iRST)
  if ((iRST ==  1'b1))
    begin
       iBAUDOUTN <=  1'b1; // 413
       OUT1N <=  1'b1; // 413
       OUT2N <=  1'b1; // 413
       RTSN <=  1'b1; // 413
       DTRN <=  1'b1; // 413
       SOUT <=  1'b1; // 413
    end
  else
    begin
       iBAUDOUTN <=  1'b0; // 413
       OUT1N <=  1'b0; // 413
       OUT2N <=  1'b0; // 413
       RTSN <=  1'b0; // 413
       DTRN <=  1'b0; // 413
       SOUT <=  1'b0; // 413
       if ((iBaudtick16x ==  1'b0))
         begin
            iBAUDOUTN <=  1'b1; // 413
         end
       
       if ((iMCR_LOOP ==  1'b1 | iMCR_OUT1 ==  1'b0))
         begin
            OUT1N <=  1'b1; // 413
         end
       
       if ((iMCR_LOOP ==  1'b1 | iMCR_OUT2 ==  1'b0))
         begin
            OUT2N <=  1'b1; // 413
         end
       
       if ((iMCR_LOOP ==  1'b1 | iRTS ==  1'b0))
         begin
            RTSN <=  1'b1; // 413
         end
       
       if ((iMCR_LOOP ==  1'b1 | iMCR_DTR ==  1'b0))
         begin
            DTRN <=  1'b1; // 413
         end
       
       if ((iMCR_LOOP ==  1'b1 | iSOUT ==  1'b1))
         begin
            SOUT <=  1'b1; // 413
              end
    end

always @(PADDR or iLCR_DLAB or iRBR or iDLL or iDLM or iIER or iIIR or iLCR or iMCR or iLSR or iMSR or iSCR)
  begin
     case (PADDR)
       3'b000:
         begin
            if ((iLCR_DLAB ==  1'b0))
              begin
                 PRDATA[7:0] <= iRBR;
              end
            else
              begin
                 PRDATA[7:0] <= iDLL;
              end
         end
       
       3'b001:
         begin
            if ((iLCR_DLAB ==  1'b0))
              begin
                 PRDATA[7:0] <= iIER;
              end
            else
              begin
                 PRDATA[7:0] <= iDLM;
              end
         end
       
       3'b010:
         begin
            PRDATA[7:0] <= iIIR;
         end
       
       3'b011:
         begin
            PRDATA[7:0] <= iLCR;
         end
       
       3'b100:
         begin
            PRDATA[7:0] <= iMCR;
         end
       
       3'b101:
         begin
            PRDATA[7:0] <= iLSR;
         end
       
       3'b110:
         begin
            PRDATA[7:0] <= iMSR;
         end
       
       3'b111:
         begin
            PRDATA[7:0] <= iSCR;
         end
       
       default:
         begin
            PRDATA[7:0] <= iRBR;
         end
       
     endcase

  end

assign PRDATA[31:8] = 24'b0;
assign /*432*/ PREADY =  1'b1; // 434
assign /*432*/ PSLVERR =  1'b0; // 434

endmodule // apb_uart
//
// UART 16750
//
// Converted to System Verilog by Jonathan Kimmitt
// This version has been partially checked with formality but some bugs remain
// Original Author:   Sebastian Witt
// Date:     14.03.2019
// Version:  1.6
//
// History:  1.0 - Initial version
//           1.1 - THR empty interrupt register connected to RST
//           1.2 - Registered outputs
//           1.3 - Automatic flow control
//           1.4 - De-assert IIR FIFO64 when FIFO is disabled
//           1.5 - Inverted low active outputs when RST is active
//           1.6 - Converted to System Verilog
//
//
// This code is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This code is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the
// Free Software  Foundation, Inc., 59 Temple Place, Suite 330,
// Boston, MA  02111-1307  USA
//

module slib_clock_div #(parameter RATIO = 4) (
	input wire		CLK,
	input wire		RST,
	input wire		CE,
	output logic		Q); // 507
/* design slib_clock_div */
/* architecture rtl */
typedef enum {FALSE,TRUE} bool_t; // 527
logic iQ; // 612
logic [$clog2(RATIO-1)-1:0] iCounter;
   
always @(posedge CLK or posedge RST)
begin
if ((RST ==  1'b1))
  begin
  iCounter <= 0; // 413
    iQ <=  1'b0; // 413
      end
  else
  begin
  iQ <=  1'b0; // 413
    if ((CE ==  1'b1))
          begin
      if ((iCounter == (RATIO - 1)))
                  begin
          iQ <=  1'b1; // 413
            iCounter <= 0; // 413
                      end
           else
          begin
          iCounter <= iCounter + 1; // 413
                      end
                end
      
  end
  
end

assign /*432*/ Q = iQ; // 434

endmodule // slib_clock_div
//
// UART 16750
//
// Converted to System Verilog by Jonathan Kimmitt
// This version has been partially checked with formality but some bugs remain
// Original Author:   Sebastian Witt
// Date:     14.03.2019
// Version:  1.6
//
// History:  1.0 - Initial version
//           1.1 - THR empty interrupt register connected to RST
//           1.2 - Registered outputs
//           1.3 - Automatic flow control
//           1.4 - De-assert IIR FIFO64 when FIFO is disabled
//           1.5 - Inverted low active outputs when RST is active
//           1.6 - Converted to System Verilog
//
//
// This code is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This code is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the
// Free Software  Foundation, Inc., 59 Temple Place, Suite 330,
// Boston, MA  02111-1307  USA
//

module slib_counter # (parameter WIDTH = 4) (
	input wire		CLK,
	input wire		RST,
	input wire		CLEAR,
	input wire		LOAD,
	input wire		ENABLE,
	input wire		DOWN,
	input wire	[WIDTH - 1:0] 	D,
	output logic	[WIDTH - 1:0] 	Q,
	output logic		OVERFLOW); // 507
/* design slib_counter */
/* architecture rtl */
typedef enum {FALSE,TRUE} bool_t; // 527
logic [WIDTH:0] iCounter; // 605

always @(posedge CLK or posedge RST)
if (RST)
  begin
     /* block const 263 */
     iCounter <= 0;
  end
else
  begin
     if ((CLEAR ==  1'b1))
       begin
          /* block const 263 */
          iCounter <= 0;
       end
     else if ((LOAD ==  1'b1))
       begin
          iCounter <= $unsigned({ 1'b0, D}); // 413
       end
     
     else if ((ENABLE ==  1'b1))
       begin
          if ((DOWN ==  1'b0))
            begin
               iCounter <= iCounter + 1; // 413
            end
          else
            begin
               iCounter <= iCounter - 1; // 413
            end
       end
     
     if ((iCounter[WIDTH] ==  1'b1))
       begin
          iCounter[WIDTH] <= 0;
       end
  end
  
assign /*432*/ Q = iCounter[WIDTH - 1:0]; // 434
assign /*432*/ OVERFLOW = iCounter[WIDTH]; // 434
endmodule // slib_counter
//
// UART 16750
//
// Converted to System Verilog by Jonathan Kimmitt
// This version has been partially checked with formality but some bugs remain
// Original Author:   Sebastian Witt
// Date:     14.03.2019
// Version:  1.6
//
// History:  1.0 - Initial version
//           1.1 - THR empty interrupt register connected to RST
//           1.2 - Registered outputs
//           1.3 - Automatic flow control
//           1.4 - De-assert IIR FIFO64 when FIFO is disabled
//           1.5 - Inverted low active outputs when RST is active
//           1.6 - Converted to System Verilog
//
//
// This code is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This code is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the
// Free Software  Foundation, Inc., 59 Temple Place, Suite 330,
// Boston, MA  02111-1307  USA
//

module slib_edge_detect(
	input wire		CLK,
	input wire		RST,
	input wire		D,
	output logic		RE,
	output logic		FE); // 507
/* design slib_edge_detect */
/* architecture rtl */
typedef enum {FALSE,TRUE} bool_t; // 527
logic iDd; // 612

always @(posedge CLK or posedge RST)
begin
if ((RST ==  1'b1))
  begin
  iDd <=  1'b0; // 413
      end
  else
  begin
  iDd <= D; // 413
      end
  
end

assign /*903*/ RE = iDd ==  1'b0 && D ==  1'b1 ?  1'b1 :   1'b0; // 905
assign /*903*/ FE = iDd ==  1'b1 && D ==  1'b0 ?  1'b1 :   1'b0; // 905

endmodule // slib_edge_detect
//
// UART 16750
//
// Converted to System Verilog by Jonathan Kimmitt
// This version has been partially checked with formality but some bugs remain
// Original Author:   Sebastian Witt
// Date:     14.03.2019
// Version:  1.6
//
// History:  1.0 - Initial version
//           1.1 - THR empty interrupt register connected to RST
//           1.2 - Registered outputs
//           1.3 - Automatic flow control
//           1.4 - De-assert IIR FIFO64 when FIFO is disabled
//           1.5 - Inverted low active outputs when RST is active
//           1.6 - Converted to System Verilog
//
//
// This code is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This code is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the
// Free Software  Foundation, Inc., 59 Temple Place, Suite 330,
// Boston, MA  02111-1307  USA
//

module slib_fifo # (parameter WIDTH = 8, parameter SIZE_E=6) (
	input wire		CLK,
	input wire		RST,
	input wire		CLEAR,
	input wire		WRITE,
	input wire		READ,
	input wire	[WIDTH - 1:0] 	D,
	output logic	[WIDTH - 1:0] 	Q,
	output logic		EMPTY,
	output logic		FULL,
	output logic	[SIZE_E - 1:0] 	USAGE); // 507
/* design slib_fifo */
/* architecture rtl */
typedef enum {FALSE,TRUE} bool_t; // 527
logic iEMPTY; // 612
logic iFULL; // 612
logic [SIZE_E:0] iWRAddr; // 605
logic [SIZE_E:0] iRDAddr; // 605
logic [SIZE_E:0] init; // 605
logic [SIZE_E - 1:0] iUSAGE; // 605

case (SIZE_E)
  6:
    begin:size64
       logic [WIDTH-1:0] iFIFOMem [0:2**SIZE_E-1];

       always @(posedge CLK or posedge RST)
       begin
       if ((RST ==  1'b1))
         begin
            Q <= (0<<0);
         end
         else
         begin
         if ((WRITE ==  1'b1 && iFULL ==  1'b0))
           begin
              iFIFOMem[iWRAddr[SIZE_E-1:0]] <= D;
           end

       Q <= iFIFOMem[iRDAddr[SIZE_E - 1:0]]; // 413
             end

       end

    end
  11:
    begin:size2048
     RAMB16_S9_S9 RAMB16_S9_S9_inst
       (
        .CLKA   ( CLK                      ),     // Port A Clock
        .DOA    ( Q                        ),     // Port A 1-bit Data Output
        .DOPA   (                          ),
        .ADDRA  ( iRDAddr[SIZE_E - 1:0]    ),     // Port A 14-bit Address Input
        .DIA    ( 8'b0                     ),     // Port A 1-bit Data Input
        .DIPA   ( 1'b0                     ),
        .ENA    ( 1'b1                     ),     // Port A RAM Enable Input
        .SSRA   ( 1'b0                     ),     // Port A Synchronous Set/Reset Input
        .WEA    ( 1'b0                     ),     // Port A Write Enable Input
        .CLKB   ( CLK                      ),     // Port B Clock
        .DOB    (                          ),     // Port B 1-bit Data Output
        .DOPB   (                          ),
        .ADDRB  ( iWRAddr[SIZE_E-1:0]      ),     // Port B 14-bit Address Input
        .DIB    ( D                        ),     // Port B 1-bit Data Input
        .DIPB   ( 1'b0                     ),
        .ENB    ( iFULL == 1'b0            ),     // Port B RAM Enable Input
        .SSRB   ( 1'b0                     ),     // Port B Synchronous Set/Reset Input
        .WEB    ( WRITE                    )      // Port B Write Enable Input
        );
    end // block: size2048
  default:
    $error("FIFO size must be 6 or 11");

endcase // case (SIZE_E)
   
assign /*903*/ iFULL = (iRDAddr[SIZE_E - 1:0]  == iWRAddr[SIZE_E - 1:0] ) && (iRDAddr[SIZE_E] != iWRAddr[SIZE_E]) ?  1'b1 :   1'b0; // 905

always @(posedge CLK or posedge RST)
begin
if ((RST ==  1'b1))
  begin
  /* block const 263 */
iWRAddr <= 0;
/* block const 263 */
iRDAddr <= 0;
iEMPTY <=  1'b1; // 413
      end
  else
  begin
  if ((WRITE ==  1'b1 && iFULL ==  1'b0))
          begin
      iWRAddr <= iWRAddr + 1; // 413
              end
      
if ((READ ==  1'b1 && iEMPTY ==  1'b0))
          begin
      iRDAddr <= iRDAddr + 1; // 413
              end
      
if ((CLEAR ==  1'b1))
          begin
      /* block const 263 */
iWRAddr <= 0;
/* block const 263 */
iRDAddr <= 0;
      end
      
if ((iRDAddr == iWRAddr))
          begin
      iEMPTY <=  1'b1; // 413
              end
       else
      begin
      iEMPTY <=  1'b0; // 413
              end
        end
  
end

always @(posedge CLK or posedge RST)
begin
if ((RST ==  1'b1))
  begin
  /* block const 263 */
iUSAGE <= 0;
  end
  else
  begin
  if ((CLEAR ==  1'b1))
          begin
      /* block const 263 */
iUSAGE <= 0;
      end
       else
      begin
      if (((READ ==  1'b0 && WRITE ==  1'b1) && iFULL ==  1'b0))
                  begin
          iUSAGE <= iUSAGE + 1; // 413
                      end
          
if (((WRITE ==  1'b0 && READ ==  1'b1) && iEMPTY ==  1'b0))
                  begin
          iUSAGE <= iUSAGE - 1; // 413
                      end
          
      end
        end
  
end

assign /*432*/ EMPTY = iEMPTY; // 434
assign /*432*/ FULL = iFULL; // 434
assign /*432*/ USAGE = iUSAGE; // 434

endmodule // slib_fifo
//
// UART 16750
//
// Converted to System Verilog by Jonathan Kimmitt
// This version has been partially checked with formality but some bugs remain
// Original Author:   Sebastian Witt
// Date:     14.03.2019
// Version:  1.6
//
// History:  1.0 - Initial version
//           1.1 - THR empty interrupt register connected to RST
//           1.2 - Registered outputs
//           1.3 - Automatic flow control
//           1.4 - De-assert IIR FIFO64 when FIFO is disabled
//           1.5 - Inverted low active outputs when RST is active
//           1.6 - Converted to System Verilog
//
//
// This code is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This code is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the
// Free Software  Foundation, Inc., 59 Temple Place, Suite 330,
// Boston, MA  02111-1307  USA
//

module slib_input_filter #(parameter SIZE = 4) (
	input wire		CLK,
	input wire		RST,
	input wire		CE,
	input wire		D,
	output logic		Q); // 507
/* design slib_input_filter */
/* architecture rtl */
typedef enum {FALSE,TRUE} bool_t; // 527

logic [$clog2(SIZE+1)-1:0] iCount;
   
always @(posedge CLK or posedge RST)
  begin
     if ((RST ==  1'b1))
       begin
          iCount <= 0; // 413
          Q <=  1'b0; // 413
       end
     else
       begin
          if ((CE ==  1'b1))
            begin
               if ((D ==  1'b1) && (iCount != SIZE))
                 begin
                    iCount <= iCount + 1; // 413
                 end
               else if         ((D ==  1'b0) && (iCount != 0))
                 begin
                    iCount <= iCount - 1; // 413
                 end
            end
          
          if ((iCount == SIZE))
            begin
               Q <=  1'b1; // 413
            end
          else if     ((iCount == 0))
            begin
               Q <=  1'b0; // 413
            end
       end
     
  end
   

endmodule
//
// UART 16750
//
// Converted to System Verilog by Jonathan Kimmitt
// This version has been partially checked with formality but some bugs remain
// Original Author:   Sebastian Witt
// Date:     14.03.2019
// Version:  1.6
//
// History:  1.0 - Initial version
//           1.1 - THR empty interrupt register connected to RST
//           1.2 - Registered outputs
//           1.3 - Automatic flow control
//           1.4 - De-assert IIR FIFO64 when FIFO is disabled
//           1.5 - Inverted low active outputs when RST is active
//           1.6 - Converted to System Verilog
//
//
// This code is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This code is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the
// Free Software  Foundation, Inc., 59 Temple Place, Suite 330,
// Boston, MA  02111-1307  USA
//

module slib_input_sync(
	input wire		CLK,
	input wire		RST,
	input wire		D,
	output logic		Q); // 507
/* design slib_input_sync */
/* architecture rtl */
typedef enum {FALSE,TRUE} bool_t; // 527
logic [1:0] iD; // 605

always @(posedge CLK or posedge RST)
  begin
     if ((RST ==  1'b1))
       begin
          /* block const 263 */
          iD <= (0<<1)|(0<<0);
       end
     else
       begin
          iD[0] <= D;
          iD[1] <= iD[0];       
       end
  end

assign /*432*/ Q = iD[1]; // 434
endmodule // slib_input_sync
//
// UART 16750
//
// Converted to System Verilog by Jonathan Kimmitt
// This version has been partially checked with formality but some bugs remain
// Original Author:   Sebastian Witt
// Date:     14.03.2019
// Version:  1.6
//
// History:  1.0 - Initial version
//           1.1 - THR empty interrupt register connected to RST
//           1.2 - Registered outputs
//           1.3 - Automatic flow control
//           1.4 - De-assert IIR FIFO64 when FIFO is disabled
//           1.5 - Inverted low active outputs when RST is active
//           1.6 - Converted to System Verilog
//
//
// This code is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This code is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the
// Free Software  Foundation, Inc., 59 Temple Place, Suite 330,
// Boston, MA  02111-1307  USA
//

module slib_mv_filter #(parameter WIDTH = 4, THRESHOLD = 10) (
	input wire		CLK,
	input wire		RST,
	input wire		SAMPLE,
	input wire		CLEAR,
	input wire		D,
	output logic		Q); // 507
/* design slib_mv_filter */
/* architecture rtl */
typedef enum {FALSE,TRUE} bool_t; // 527
logic [WIDTH:0] iCounter; // 605
logic iQ; // 612

always @(posedge CLK or posedge RST)
  if (RST)
    begin
       /* block const 263 */
       iCounter <= 0;
       iQ <=  1'b0; // 413
    end
  else
    begin
       if (iCounter >= THRESHOLD)
         begin
            iQ <=  1'b1; // 413
         end
       else
         begin
            if ((SAMPLE ==  1'b1 && D ==  1'b1))
              begin
                 iCounter <= iCounter + 1; // 413
              end
         end
       if ((CLEAR ==  1'b1))
         begin
            /* block const 263 */
            iCounter <= 0;
            iQ <=  1'b0; // 413
         end
    end

assign /*432*/ Q = iQ; // 434

endmodule // slib_mv_filter
//
// UART 16750
//
// Converted to System Verilog by Jonathan Kimmitt
// This version has been partially checked with formality but some bugs remain
// Original Author:   Sebastian Witt
// Date:     14.03.2019
// Version:  1.6
//
// History:  1.0 - Initial version
//           1.1 - THR empty interrupt register connected to RST
//           1.2 - Registered outputs
//           1.3 - Automatic flow control
//           1.4 - De-assert IIR FIFO64 when FIFO is disabled
//           1.5 - Inverted low active outputs when RST is active
//           1.6 - Converted to System Verilog
//
//
// This code is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This code is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the
// Free Software  Foundation, Inc., 59 Temple Place, Suite 330,
// Boston, MA  02111-1307  USA
//

module uart_baudgen(
	input wire		CLK,
	input wire		RST,
	input wire		CE,
	input wire		CLEAR,
	input wire	[15:0] 	DIVIDER,
	output logic		BAUDTICK); // 507
/* design uart_baudgen */
/* architecture rtl */
typedef enum {FALSE,TRUE} bool_t; // 527
logic [15:0] iCounter; // 605

always @(posedge CLK or posedge RST)
begin
if ((RST ==  1'b1))
  begin
  /* block const 263 */
iCounter <= (0<<15)|(0<<14)|(0<<13)|(0<<12)|(0<<11)|(0<<10)|(0<<9)|(0<<8)|(0<<7)|(0<<6)|(0<<5)|(0<<4)|(0<<3)|(0<<2)|(0<<1)|(0<<0);
BAUDTICK <=  1'b0; // 413
      end
  else
  begin
  if ((CLEAR ==  1'b1))
          begin
      /* block const 263 */
iCounter <= (0<<15)|(0<<14)|(0<<13)|(0<<12)|(0<<11)|(0<<10)|(0<<9)|(0<<8)|(0<<7)|(0<<6)|(0<<5)|(0<<4)|(0<<3)|(0<<2)|(0<<1)|(0<<0);
      end
          else if     ((CE ==  1'b1))
          begin
      iCounter <= iCounter + 1; // 413
              end
      BAUDTICK <=  1'b0; // 413
    if ((iCounter == $unsigned(DIVIDER)))
          begin
      /* block const 263 */
iCounter <= (0<<15)|(0<<14)|(0<<13)|(0<<12)|(0<<11)|(0<<10)|(0<<9)|(0<<8)|(0<<7)|(0<<6)|(0<<5)|(0<<4)|(0<<3)|(0<<2)|(0<<1)|(0<<0);
BAUDTICK <=  1'b1; // 413
              end
      
  end
  
end

endmodule // uart_baudgen
//
// UART 16750
//
// Converted to System Verilog by Jonathan Kimmitt
// This version has been partially checked with formality but some bugs remain
// Original Author:   Sebastian Witt
// Date:     14.03.2019
// Version:  1.6
//
// History:  1.0 - Initial version
//           1.1 - THR empty interrupt register connected to RST
//           1.2 - Registered outputs
//           1.3 - Automatic flow control
//           1.4 - De-assert IIR FIFO64 when FIFO is disabled
//           1.5 - Inverted low active outputs when RST is active
//           1.6 - Converted to System Verilog
//
//
// This code is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This code is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the
// Free Software  Foundation, Inc., 59 Temple Place, Suite 330,
// Boston, MA  02111-1307  USA
//

module uart_interrupt(
	input wire		CLK,
	input wire		RST,
	input wire	[3:0] 	IER,
	input wire	[4:0] 	LSR,
	input wire		THI,
	input wire		RDA,
	input wire		CTI,
	input wire		AFE,
	input wire	[3:0] 	MSR,
	output logic	[3:0] 	IIR,
	output logic		INT); // 507
/* design uart_interrupt */
/* architecture rtl */
typedef enum {FALSE,TRUE} bool_t; // 527
logic iRLSInterrupt; // 612
logic iRDAInterrupt; // 612
logic iCTIInterrupt; // 612
logic iTHRInterrupt; // 612
logic iMSRInterrupt; // 612
logic [3:0] iIIR; // 605
assign /*432*/ iRLSInterrupt = IER[2] && (((LSR[1] | LSR[2]) | LSR[3]) | LSR[4]); // 434
assign /*432*/ iRDAInterrupt = IER[0] && RDA; // 434
assign /*432*/ iCTIInterrupt = IER[0] && CTI; // 434
assign /*432*/ iTHRInterrupt = IER[1] && THI; // 434
assign /*432*/ iMSRInterrupt = IER[3] && ((((MSR[0] &&  ~ AFE) | MSR[1]) | MSR[2]) | MSR[3]); // 434

always @(posedge CLK or posedge RST)
  if ((RST ==  1'b1))
    begin
       iIIR <= 4'b0001; // 413
    end
  else
    begin
       if ((iRLSInterrupt ==  1'b1))
         begin
            iIIR <= 4'b0110; // 413
         end
       else if ((iCTIInterrupt ==  1'b1))
         begin
            iIIR <= 4'b1100; // 413
         end
       else if ((iRDAInterrupt ==  1'b1))
         begin
            iIIR <= 4'b0100; // 413
         end
       else if ((iTHRInterrupt ==  1'b1))
         begin
            iIIR <= 4'b0010; // 413
         end
       else if ((iMSRInterrupt ==  1'b1))
         begin
            iIIR <= 4'b0000; // 413
         end
       else
         begin
            iIIR <= 4'b0001; // 413
         end
    end

assign /*432*/ IIR = iIIR; // 434
assign /*432*/ INT =  ~ iIIR[0]; // 434

endmodule // uart_interrupt
//
// UART 16750
//
// Converted to System Verilog by Jonathan Kimmitt
// This version has been partially checked with formality but some bugs remain
// Original Author:   Sebastian Witt
// Date:     14.03.2019
// Version:  1.6
//
// History:  1.0 - Initial version
//           1.1 - THR empty interrupt register connected to RST
//           1.2 - Registered outputs
//           1.3 - Automatic flow control
//           1.4 - De-assert IIR FIFO64 when FIFO is disabled
//           1.5 - Inverted low active outputs when RST is active
//           1.6 - Converted to System Verilog
//
//
// This code is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This code is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the
// Free Software  Foundation, Inc., 59 Temple Place, Suite 330,
// Boston, MA  02111-1307  USA
//

module uart_receiver(
	input wire		CLK,
	input wire		RST,
	input wire		RXCLK,
	input wire		RXCLEAR,
	input wire	[1:0] 	WLS,
	input wire		STB,
	input wire		PEN,
	input wire		EPS,
	input wire		SP,
	input wire		SIN,
	output logic		PE,
	output logic		FE,
	output logic		BI,
	output logic	[7:0] 	DOUT,
	output logic		RXFINISHED); // 507
/* design uart_receiver */
/* architecture rtl */
typedef enum {FALSE,TRUE} bool_t; // 527
typedef enum logic [2:0] {IDLE,
START,
DATA,
PAR,
STOP,
MWAIT} state_type; // 674
state_type CState, NState; // 908
logic [3:0] iBaudCount; // 605
logic iBaudCountClear; // 612
logic iBaudStep; // 612
logic iBaudStepD; // 612
logic iFilterClear; // 612
logic iFSIN; // 612
logic iFStopBit; // 612
logic iParity; // 612
logic iParityReceived; // 612
logic [3:0] iDataCount; // 900
logic iDataCountInit; // 612
logic iDataCountFinish; // 612
logic iRXFinished; // 612
logic iFE; // 612
logic iBI; // 612
logic iNoStopReceived; // 612
logic [7:0] iDOUT; // 605
slib_counter #(.WIDTH(4)) RX_BRC (
	.CLK(CLK),
	.RST(RST),
	.CLEAR(iBaudCountClear),
	.LOAD( 1'b0),
	.ENABLE(RXCLK),
	.DOWN( 1'b0),
	.D(4'b0000),
	.Q(iBaudCount),
	.OVERFLOW(iBaudStep)); // 879
slib_mv_filter #(.WIDTH(4),.THRESHOLD(10)) RX_MVF (
	.CLK(CLK),
	.RST(RST),
	.SAMPLE(RXCLK),
	.CLEAR(iFilterClear),
	.D(SIN),
	.Q(iFSIN)); // 879
slib_input_filter #(.SIZE(4)) RX_IFSB (
	.CLK(CLK),
	.RST(RST),
	.CE(RXCLK),
	.D(SIN),
	.Q(iFStopBit)); // 879

always @(posedge CLK or posedge RST)
  begin
     if ((RST ==  1'b1))
       begin
          iBaudStepD <=  1'b0; // 413
       end
     else
       begin
          iBaudStepD <= iBaudStep; // 413
       end
  end

assign /*432*/ iFilterClear = iBaudStepD | iBaudCountClear; // 434

always @(iDOUT or EPS)
begin
iParity <= (((((((iDOUT[7] ^ iDOUT[6]) ^ iDOUT[5]) ^ iDOUT[4]) ^ iDOUT[3]) ^ iDOUT[2]) ^ iDOUT[1]) ^ iDOUT[0]) ^  ~ EPS; // 413

end


always @(posedge CLK or posedge RST)
  if ((RST ==  1'b1))
    begin
       iDataCount <= 0; // 413
       /* block const 263 */
       iDOUT <= (0<<7)|(0<<6)|(0<<5)|(0<<4)|(0<<3)|(0<<2)|(0<<1)|(0<<0);
    end
  else
    begin
       if ((iDataCountInit ==  1'b1))
         begin
            iDataCount <= 0; // 413
            /* block const 263 */
            iDOUT <= (0<<7)|(0<<6)|(0<<5)|(0<<4)|(0<<3)|(0<<2)|(0<<1)|(0<<0);
         end
       else
         begin
            if ((iBaudStep ==  1'b1 && iDataCountFinish ==  1'b0))
              begin
                 iDOUT[iDataCount] <= iFSIN;
                 iDataCount <= iDataCount + 1; // 413
              end
         end
    end

assign /*903*/ iDataCountFinish = (((WLS == 2'b00 && iDataCount == 5) | (WLS == 2'b01 && iDataCount == 6)) | (WLS == 2'b10 && iDataCount == 7)) | (WLS == 2'b11 && iDataCount == 8) ?  1'b1 :   1'b0; // 905

always @(posedge CLK or posedge RST)
  if ((RST ==  1'b1))
    begin
       CState <= IDLE; // 413
    end
  else
    begin
       CState <= NState; // 413
    end

always @(CState or SIN or iFSIN or iFStopBit or iBaudStep or iBaudCount or iDataCountFinish or PEN or WLS or STB)
begin
NState <= IDLE; // 413
iBaudCountClear <=  1'b0; // 413
iDataCountInit <=  1'b0; // 413
iRXFinished <=  1'b0; // 413
case (CState)
  IDLE:
    begin
  if ((SIN ==  1'b0))
          begin
      NState <= START; // 413
              end
      
iBaudCountClear <=  1'b1; // 413
    iDataCountInit <=  1'b1; // 413
      end
  
  START:
    begin
  iDataCountInit <=  1'b1; // 413
    if ((iBaudStep ==  1'b1))
          begin
      if ((iFSIN ==  1'b0))
                  begin
          NState <= DATA; // 413
                      end
          
      end
       else
      begin
      NState <= START; // 413
              end
        end
  
  DATA:
    begin
  if ((iDataCountFinish ==  1'b1))
          begin
      if ((PEN ==  1'b1))
                  begin
          NState <= PAR; // 413
                      end
           else
          begin
          NState <= STOP; // 413
                      end
                end
       else
      begin
      NState <= DATA; // 413
              end
        end
  
  PAR:
    begin
  if ((iBaudStep ==  1'b1))
          begin
      NState <= STOP; // 413
              end
       else
      begin
      NState <= PAR; // 413
              end
        end
  
  STOP:
    begin
  if ((iBaudCount[3] ==  1'b1))
          begin
      if ((iFStopBit ==  1'b0))
                  begin
          iRXFinished <=  1'b1; // 413
            NState <= MWAIT; // 413
                      end
           else
          begin
          iRXFinished <=  1'b1; // 413
            NState <= IDLE; // 413
                      end
                end
       else
      begin
      NState <= STOP; // 413
              end
        end
  
  MWAIT:
    begin
  if ((SIN ==  1'b0))
          begin
      NState <= MWAIT; // 413
              end
      
  end
  
  default:
    begin
  begin end  end
  
endcase

end


always @(posedge CLK or posedge RST)
begin
if ((RST ==  1'b1))
  begin
  PE <=  1'b0; // 413
    iParityReceived <=  1'b0; // 413
      end
  else
  begin
  if ((CState == PAR && iBaudStep ==  1'b1))
          begin
      iParityReceived <= iFSIN; // 413
              end
      
if ((PEN ==  1'b1))
          begin
      PE <=  1'b0; // 413
        if ((SP ==  1'b1))
                  begin
          if (((EPS ^ iParityReceived) ==  1'b0))
                          begin
              PE <=  1'b1; // 413
                              end
              
          end
           else
          begin
          if ((iParity != iParityReceived))
                          begin
              PE <=  1'b1; // 413
                              end
              
          end
                end
       else
      begin
      PE <=  1'b0; // 413
        iParityReceived <=  1'b0; // 413
              end
        end
  
end

assign /*903*/ iNoStopReceived = iFStopBit ==  1'b0 && (CState == STOP) ?  1'b1 :   1'b0; // 905
assign /*903*/ iBI = (iDOUT == 8'b00000000 && iParityReceived ==  1'b0) && iNoStopReceived ==  1'b1 ?  1'b1 :   1'b0; // 905
assign /*903*/ iFE = iNoStopReceived ==  1'b1 ?  1'b1 :   1'b0; // 905
assign /*432*/ DOUT = iDOUT; // 434
assign /*432*/ BI = iBI; // 434
assign /*432*/ FE = iFE; // 434
assign /*432*/ RXFINISHED = iRXFinished; // 434
endmodule
//
// UART 16750
//
// Converted to System Verilog by Jonathan Kimmitt
// This version has been partially checked with formality but some bugs remain
// Original Author:   Sebastian Witt
// Date:     14.03.2019
// Version:  1.6
//
// History:  1.0 - Initial version
//           1.1 - THR empty interrupt register connected to RST
//           1.2 - Registered outputs
//           1.3 - Automatic flow control
//           1.4 - De-assert IIR FIFO64 when FIFO is disabled
//           1.5 - Inverted low active outputs when RST is active
//           1.6 - Converted to System Verilog
//
//
// This code is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This code is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the
// Free Software  Foundation, Inc., 59 Temple Place, Suite 330,
// Boston, MA  02111-1307  USA
//

module uart_transmitter(
	input wire		CLK,
	input wire		RST,
	input wire		TXCLK,
	input wire		TXSTART,
	input wire		CLEAR,
	input wire	[1:0] 	WLS,
	input wire		STB,
	input wire		PEN,
	input wire		EPS,
	input wire		SP,
	input wire		BC,
	input wire	[7:0] 	DIN,
	output logic		TXFINISHED,
	output logic		SOUT); // 507
/* design uart_transmitter */
/* architecture rtl */
typedef enum {FALSE,TRUE} bool_t; // 527
typedef enum logic [3:0] {IDLE,
START,
BIT0,
BIT1,
BIT2,
BIT3,
BIT4,
BIT5,
BIT6,
BIT7,
PAR,
STOP,
STOP2} state_type; // 674
state_type CState, NState; // 908
logic iTx2; // 612
logic iSout; // 612
logic iParity; // 612
logic iFinished; // 612

always @(posedge CLK or posedge RST)
  if ((RST ==  1'b1))
    begin
       CState <= IDLE; // 413
       iTx2 <=  1'b0; // 413
    end
  else
    begin
       if ((TXCLK ==  1'b1))
         begin
            if ((iTx2 ==  1'b0))
              begin
                 CState <= NState; // 413
                 iTx2 <=  1'b1; // 413
              end
            else
              begin
                 if ((((WLS == 2'b00) && (STB ==  1'b1)) && CState == STOP2))
                   begin
                      CState <= NState; // 413
                      iTx2 <=  1'b1; // 413
                   end
                 else
                   begin
                      CState <= CState; // 413
                      iTx2 <=  1'b0; // 413
                   end
              end
         end
    end

always @(CState or TXSTART or DIN or WLS or PEN or SP or EPS or STB or iParity)
  begin
     NState <= IDLE; // 413
     iSout <=  1'b1; // 413
     case (CState)
       IDLE:
         begin
            if ((TXSTART ==  1'b1))
              begin
                 NState <= START; // 413
              end
            
         end
       
       START:
         begin
            iSout <=  1'b0; // 413
            NState <= BIT0; // 413
         end
       
       BIT0:
         begin
            iSout <= DIN[0]; // 413
            NState <= BIT1; // 413
         end
       
       BIT1:
         begin
            iSout <= DIN[1]; // 413
            NState <= BIT2; // 413
         end
       
       BIT2:
         begin
            iSout <= DIN[2]; // 413
            NState <= BIT3; // 413
         end
       
       BIT3:
         begin
            iSout <= DIN[3]; // 413
            NState <= BIT4; // 413
         end
       
       BIT4:
         begin
            iSout <= DIN[4]; // 413
            if ((WLS == 2'b00))
              begin
                 if ((PEN ==  1'b1))
                   begin
                      NState <= PAR; // 413
                   end
                 else
                   begin
                      NState <= STOP; // 413
                   end
              end
            else
              begin
                 NState <= BIT5; // 413
              end
         end
       
       BIT5:
         begin
            iSout <= DIN[5]; // 413
            if ((WLS == 2'b01))
              begin
                 if ((PEN ==  1'b1))
                   begin
                      NState <= PAR; // 413
                   end
                 else
                   begin
                      NState <= STOP; // 413
                   end
              end
            else
              begin
                 NState <= BIT6; // 413
              end
         end
       
       BIT6:
         begin
            iSout <= DIN[6]; // 413
            if ((WLS == 2'b10))
              begin
                 if ((PEN ==  1'b1))
                   begin
                      NState <= PAR; // 413
                   end
                 else
                   begin
                      NState <= STOP; // 413
                   end
              end
            else
              begin
                 NState <= BIT7; // 413
              end
         end
       
       BIT7:
         begin
            iSout <= DIN[7]; // 413
            if ((PEN ==  1'b1))
              begin
                 NState <= PAR; // 413
              end
            else
              begin
                 NState <= STOP; // 413
              end
         end
       
       PAR:
         begin
            if ((SP ==  1'b1))
              begin
                 if ((EPS ==  1'b1))
                   begin
                      iSout <=  1'b0; // 413
                   end
                 else
                   begin
                      iSout <=  1'b1; // 413
                   end
              end
            else
              begin
                 if ((EPS ==  1'b1))
                   begin
                      iSout <= iParity; // 413
                   end
                 else
                   begin
                      iSout <=  ~ iParity; // 413
                   end
              end
            NState <= STOP; // 413
         end
       
       STOP:
         begin
            if ((STB ==  1'b1))
              begin
                 NState <= STOP2; // 413
              end
            else
              begin
                 if ((TXSTART ==  1'b1))
                   begin
                      NState <= START; // 413
                   end
                 
              end
         end
       
  STOP2:
    begin
       if ((TXSTART ==  1'b1))
         begin
            NState <= START; // 413
         end
       
    end
       
       default:
         begin
            begin end  end
       
     endcase
     
  end
   
    // Parity generation
    always @ (DIN or WLS)
    begin:TX_PAR
        logic iP40, iP50, iP60, iP70;
        iP40 = DIN[4] ^ DIN[3] ^ DIN[2] ^ DIN[1] ^ DIN[0];
        iP50 = DIN[5] ^ iP40;
        iP60 = DIN[6] ^ iP50;
        iP70 = DIN[7] ^ iP60;

        case(WLS)
            2'b00: iParity <= iP40;
            2'b01: iParity <= iP50;
            2'b10: iParity <= iP60;
            default: iParity <= iP70;
        endcase;
    end

    logic iLast;
    always @(posedge CLK or posedge RST)
    begin:TX_FIN
        if (RST)
          begin
             iFinished <= 1'b0;
             iLast <= 1'b0;
          end
        else
          begin
             iFinished <= 1'b0;
             if (iLast == 1'b0 && CState == STOP)
               iFinished <= 1'b1;
             if (CState == STOP)
               iLast <= 1'b1;
             else
               iLast <= 1'b0;
          end
    end

assign /*903*/ SOUT = BC ==  1'b0 ? iSout :   1'b0; // 905
assign /*432*/ TXFINISHED = iFinished; // 434

endmodule // uart_transmitter
