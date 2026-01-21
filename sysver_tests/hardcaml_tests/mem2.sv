module mem2 # (parameter WIDTH = 8, parameter SIZE_E=6) (
	input wire		   CLK,
	input wire		   WRITE,
	input wire		   READ,
	input wire [SIZE_E-1:0]	   iWRAddr,
        input wire [SIZE_E-1:0]	   iRDAddr,
	input wire [WIDTH - 1:0]   D,
	output wire [WIDTH - 1:0]  Q);

logic [WIDTH - 1:0] rd;   

logic [WIDTH-1:0] imem [0:2**SIZE_E-1];

always @(posedge CLK)
     begin
        if (WRITE) case(iWRAddr)
	     0: imem[0] <= D;
	     1: imem[1] <= D;
	     2: imem[2] <= D;
	     3: imem[3] <= D;
	   endcase // case (iWrAddr)
	case(iRDAddr)
	  0: rd <= imem[0];
	  1: rd <= imem[1];
	  2: rd <= imem[2];
	  3: rd <= imem[3];
	endcase
     end

   assign Q = rd;
   
endmodule // mem1
