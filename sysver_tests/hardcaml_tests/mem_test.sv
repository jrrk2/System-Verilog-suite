module mem_test # (parameter WIDTH = 8, parameter SIZE_E=6) (
	input wire		   CLK,
	input wire		   RST,
	input wire		   READ,
	input wire		   WRITE,
	input wire [WIDTH - 1:0]   D,
	output logic [WIDTH - 1:0] Q);

reg [SIZE_E:0] iWRAddr; // 605
reg [SIZE_E:0] iRDAddr; // 605

reg [WIDTH-1:0] iFIFOMem [0:2**SIZE_E-1];

always @(posedge CLK or posedge RST)
       if (RST)
         begin
         iWRAddr <= 0;
         iRDAddr <= 0;
         end							      
       else
         begin
         if (WRITE)
             begin
             iFIFOMem[iWRAddr[SIZE_E-1:0]] <= D;
	     iWRAddr <= iWRAddr + 1;
             end;
         if (READ)
             begin
             Q <= iFIFOMem[iRDAddr[SIZE_E - 1:0]]; // 413
	     iRDAddr <= iRDAddr + 1;
             end
          end

endmodule // slib_fifo
