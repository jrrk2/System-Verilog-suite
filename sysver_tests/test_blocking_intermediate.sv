module test_blocking_intermediate(
    input clk,
    input [7:0] a, b,
    output logic [15:0] y, z
);
    logic [15:0] temp;
    
    always @(posedge clk) begin
        temp = a * b;   // blocking - should be continuous (SSA)
        y <= temp;      // non-blocking - should be register
        z <= temp + 1;  // non-blocking using temp - should be register
    end
endmodule
