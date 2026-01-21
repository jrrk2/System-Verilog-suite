module mem1 # (parameter WIDTH = 8, parameter SIZE_E=6) (
	input wire		   CLK,
	input wire		   WRITE,
	input wire		   READ,
	input wire [SIZE_E-1:0]	   iWRAddr,
        input wire [SIZE_E-1:0]	   iRDAddr,
	input wire [WIDTH - 1:0]   D,
	output logic [WIDTH - 1:0] Q);

logic [WIDTH-1:0] imem [0:2**SIZE_E-1];

always @(posedge CLK)
     begin
        if (WRITE)
	  imem[iWRAddr] <= D;
	Q <= imem[iRDAddr];
     end

endmodule // mem1
