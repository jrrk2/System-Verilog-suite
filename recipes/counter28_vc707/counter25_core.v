// 28-bit PRBS + Johnson counter — the SVS regression target for #65
// (FDSE INIT) and #106 (LUT5/OUTMUX OBUF buffer).  The original
// counter25_core uses a 25-bit LFSR (~6 Hz Johnson advance at 200
// MHz); a residual silicon-level rate glitch on V7 OLOGIC pads
// pushes the visible LED rate above persistence-of-vision.
// Widening to 28 bits drops the visible cadence to ~1 Hz, slow
// enough to confirm Johnson stepping on hardware while the rate
// glitch is investigated separately.
module counter25_core (
    input  wire clk,
    input  wire rst,
    output wire [7:0] led
);
    reg  [27:0] prbs = 28'h1;
    wire        fb   = prbs[27] ^ prbs[2];    // x^28 + x^3 + 1 (primitive)
    wire        tick = (prbs == 28'h1);
    always @(posedge clk)
        if (rst) prbs <= 28'h1;
        else     prbs <= {prbs[26:0], fb};

    reg [7:0] johnson = 8'h00;
    always @(posedge clk)
        if (rst)       johnson <= 8'h00;
        else if (tick) johnson <= {johnson[6:0], ~johnson[7]};
    assign led = johnson;
endmodule
