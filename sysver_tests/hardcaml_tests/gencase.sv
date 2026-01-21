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

module gencase # (parameter WIDTH = 8, parameter SIZE_E=6) (
	input wire		   CLK, RST, READ, WRITE,
        input wire [SIZE_E:0]	   iRDAddr,
        input wire [SIZE_E:0]	   iWRAddr,
	input logic [WIDTH - 1:0]  D,
	output logic [WIDTH - 1:0] Q);

case (SIZE_E)
  6:
    begin:size64
       reg [WIDTH-1:0] iFIFOMem [0:2**SIZE_E-1];

       always @(posedge CLK or posedge RST)
       begin
       if ((RST ==  1'b1))
            Q <= (0<<0);
         else if (READ)
            Q <= iFIFOMem[iRDAddr[SIZE_E - 1:0]]; // 413
         else if (WRITE)
           iFIFOMem[iWRAddr[SIZE_E - 1:0]] <= D;
	  
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

endmodule // slib_fifo
