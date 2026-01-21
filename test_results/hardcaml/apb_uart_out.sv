(* Generated via HardCaml circuit construction *)
(* Using Signal/Always API directly *)

module slib_input_filter__S4 (
    Q
);

    output Q;

    wire _16;
    wire _14;
    wire [31:0] _11;
    wire [28:0] _9;
    wire [31:0] _10;
    wire _12;
    wire _15;
    wire [2:0] _4;
    wire [2:0] _1;
    wire [31:0] _6;
    wire _8;
    wire _17;
    wire _2;
    assign _16 = 1'b1;
    assign _14 = 1'b0;
    assign _11 = 32'b00000000000000000000000000000000;
    assign _9 = 29'b00000000000000000000000000000;
    assign _10 = { _9,
                   _1 };
    assign _12 = _10 == _11;
    assign _15 = _12 ? _14 : _14;
    assign _4 = 3'b000;
    assign _1 = _4;
    assign _6 = { _9,
                  _1 };
    assign _8 = _6 == _11;
    assign _17 = _8 ? _16 : _15;
    assign _2 = _17;
    assign Q = _2;

endmodule


module slib_mv_filter_error (
    error
);

    output error;

    wire _2;
    assign _2 = 1'b0;
    assign error = _2;

endmodule


module slib_input_filter (
    Q
);

    output Q;

    wire _16;
    wire _14;
    wire [31:0] _11;
    wire [29:0] _9;
    wire [31:0] _10;
    wire _12;
    wire _15;
    wire [1:0] _4;
    wire [1:0] _1;
    wire [31:0] _6;
    wire _8;
    wire _17;
    wire _2;
    assign _16 = 1'b1;
    assign _14 = 1'b0;
    assign _11 = 32'b00000000000000000000000000000000;
    assign _9 = 30'b000000000000000000000000000000;
    assign _10 = { _9,
                   _1 };
    assign _12 = _10 == _11;
    assign _15 = _12 ? _14 : _14;
    assign _4 = 2'b00;
    assign _1 = _4;
    assign _6 = { _9,
                  _1 };
    assign _8 = _6 == _11;
    assign _17 = _8 ? _16 : _15;
    assign _2 = _17;
    assign Q = _2;

endmodule


module slib_counter (
    Q,
    OVERFLOW
);

    output Q;
    output OVERFLOW;

    wire _3;
    assign _3 = 1'b0;
    assign Q = _3;
    assign OVERFLOW = _3;

endmodule


module slib_fifo__Wb (
    Q,
    EMPTY,
    FULL,
    USAGE
);

    output [10:0] Q;
    output EMPTY;
    output FULL;
    output [5:0] USAGE;

    wire [5:0] _10;
    wire [5:0] _1;
    wire _11;
    wire _16;
    wire [6:0] _12;
    wire [6:0] _4;
    wire [6:0] _5;
    wire _14;
    wire _17;
    wire _6;
    wire [10:0] _18;
    wire [10:0] _8;
    assign _10 = 6'b000000;
    assign _1 = _10;
    assign _11 = 1'b0;
    assign _16 = 1'b1;
    assign _12 = 7'b0000000;
    assign _4 = _12;
    assign _5 = _12;
    assign _14 = _5 == _4;
    assign _17 = _14 ? _16 : _11;
    assign _6 = _17;
    assign _18 = 11'b00000000000;
    assign _8 = _18;
    assign Q = _8;
    assign EMPTY = _6;
    assign FULL = _11;
    assign USAGE = _1;

endmodule


module uart_transmitter_error (
    error
);

    output error;

    wire _2;
    assign _2 = 1'b0;
    assign error = _2;

endmodule


module uart_receiver_error (
    error
);

    output error;

    wire _2;
    assign _2 = 1'b0;
    assign error = _2;

endmodule


module uart_interrupt (
    IIR,
    INT
);

    output [3:0] IIR;
    output INT;

    wire _4;
    wire [3:0] _5;
    wire [3:0] _2;
    assign _4 = 1'b1;
    assign _5 = 4'b0001;
    assign _2 = _5;
    assign IIR = _2;
    assign INT = _4;

endmodule


module uart_baudgen_error (
    error
);

    output error;

    wire _2;
    assign _2 = 1'b0;
    assign error = _2;

endmodule


module slib_input_sync (
    Q
);

    output Q;

    wire _2;
    assign _2 = 1'b0;
    assign Q = _2;

endmodule


module slib_fifo (
    Q,
    EMPTY,
    FULL,
    USAGE
);

    output [7:0] Q;
    output EMPTY;
    output FULL;
    output [5:0] USAGE;

    wire [5:0] _10;
    wire [5:0] _1;
    wire _11;
    wire _16;
    wire [6:0] _12;
    wire [6:0] _4;
    wire [6:0] _5;
    wire _14;
    wire _17;
    wire _6;
    wire [7:0] _18;
    wire [7:0] _8;
    assign _10 = 6'b000000;
    assign _1 = _10;
    assign _11 = 1'b0;
    assign _16 = 1'b1;
    assign _12 = 7'b0000000;
    assign _4 = _12;
    assign _5 = _12;
    assign _14 = _5 == _4;
    assign _17 = _14 ? _16 : _11;
    assign _6 = _17;
    assign _18 = 8'b00000000;
    assign _8 = _18;
    assign Q = _8;
    assign EMPTY = _6;
    assign FULL = _11;
    assign USAGE = _1;

endmodule


module slib_edge_detect (
    RE,
    FE
);

    output RE;
    output FE;

    wire _3;
    assign _3 = 1'b0;
    assign RE = _3;
    assign FE = _3;

endmodule


module slib_clock_div (
    Q
);

    output Q;

    wire _3;
    wire _1;
    assign _3 = 1'b0;
    assign _1 = _3;
    assign Q = _1;

endmodule


module apb_uart (
    PRDATA,
    PREADY,
    PSLVERR,
    INT,
    OUT1N,
    OUT2N,
    RTSN,
    DTRN,
    SOUT
);

    output [31:0] PRDATA;
    output PREADY;
    output PSLVERR;
    output INT;
    output OUT1N;
    output OUT2N;
    output RTSN;
    output DTRN;
    output SOUT;

    wire _17;
    wire _1;
    wire _3;
    wire _5;
    wire _7;
    wire _9;
    wire _22;
    wire _11;
    wire [31:0] _25;
    wire [31:0] _15;
    assign _17 = 1'b1;
    assign _1 = _17;
    assign _3 = _17;
    assign _5 = _17;
    assign _7 = _17;
    assign _9 = _17;
    assign _22 = 1'b0;
    assign _11 = _22;
    assign _25 = 32'b00000000000000000000000000000000;
    assign _15 = _25;
    assign PRDATA = _15;
    assign PREADY = _17;
    assign PSLVERR = _22;
    assign INT = _11;
    assign OUT1N = _9;
    assign OUT2N = _7;
    assign RTSN = _5;
    assign DTRN = _3;
    assign SOUT = _1;

endmodule
