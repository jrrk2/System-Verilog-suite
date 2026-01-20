// structural_primitives.sv - Hardware Primitive Library
// Target for structural decompiler output

// =============================================================================
// Flip-Flops and Storage Elements
// =============================================================================

// D flip-flop with enable and reset
module dff_en #(
  parameter WIDTH = 1,
  parameter RESET_VAL = 0
) (
  input  logic clk,
  input  logic rst,
  input  logic en,
  input  logic [WIDTH-1:0] d,
  output logic [WIDTH-1:0] q
);
  always_ff @(posedge clk or posedge rst) begin
    if (rst)
      q <= RESET_VAL;
    else if (en)
      q <= d;
  end
endmodule

// Simple D flip-flop without enable
module dff #(
  parameter WIDTH = 1,
  parameter RESET_VAL = 0
) (
  input  logic clk,
  input  logic rst,
  input  logic [WIDTH-1:0] d,
  output logic [WIDTH-1:0] q
);
  dff_en #(.WIDTH(WIDTH), .RESET_VAL(RESET_VAL)) inst (
    .clk(clk), .rst(rst), .en(1'b1), .d(d), .q(q)
  );
endmodule

// Latch (for always_latch blocks)
module latch_en #(
  parameter WIDTH = 1
) (
  input  logic en,
  input  logic [WIDTH-1:0] d,
  output logic [WIDTH-1:0] q
);
  always_latch begin
    if (en)
      q = d;
  end
endmodule

// =============================================================================
// Arithmetic Operations
// =============================================================================

// Adder
module adder #(
  parameter WIDTH = 32
) (
  input  logic [WIDTH-1:0] a,
  input  logic [WIDTH-1:0] b,
  output logic [WIDTH-1:0] out
);
  assign out = a + b;
endmodule

// Adder with carry
module adder_carry #(
  parameter WIDTH = 32
) (
  input  logic [WIDTH-1:0] a,
  input  logic [WIDTH-1:0] b,
  input  logic cin,
  output logic [WIDTH-1:0] out,
  output logic cout
);
  assign {cout, out} = a + b + {{WIDTH{1'b0}}, cin};
endmodule

// Subtractor
module subtractor #(
  parameter WIDTH = 32
) (
  input  logic [WIDTH-1:0] a,
  input  logic [WIDTH-1:0] b,
  output logic [WIDTH-1:0] out
);
  assign out = a - b;
endmodule

// Multiplier
module multiplier #(
  parameter WIDTH = 32
) (
  input  logic [WIDTH-1:0] a,
  input  logic [WIDTH-1:0] b,
  output logic [WIDTH-1:0] out
);
  assign out = a * b;
endmodule

// Divider
module divider #(
  parameter WIDTH = 32
) (
  input  logic [WIDTH-1:0] a,
  input  logic [WIDTH-1:0] b,
  output logic [WIDTH-1:0] out
);
  assign out = (b != 0) ? (a / b) : '0;
endmodule

// Negator
module negator #(
  parameter WIDTH = 32
) (
  input  logic [WIDTH-1:0] in,
  output logic [WIDTH-1:0] out
);
  assign out = -in;
endmodule

// Incrementer
module incrementer #(
  parameter WIDTH = 32
) (
  input  logic [WIDTH-1:0] in,
  output logic [WIDTH-1:0] out
);
  assign out = in + 1'b1;
endmodule

// Decrementer
module decrementer #(
  parameter WIDTH = 32
) (
  input  logic [WIDTH-1:0] in,
  output logic [WIDTH-1:0] out
);
  assign out = in - 1'b1;
endmodule

// =============================================================================
// Bitwise Logic Operations
// =============================================================================

// Bitwise AND
module bitwise_and #(
  parameter WIDTH = 32
) (
  input  logic [WIDTH-1:0] a,
  input  logic [WIDTH-1:0] b,
  output logic [WIDTH-1:0] out
);
  assign out = a & b;
endmodule

// Bitwise OR
module bitwise_or #(
  parameter WIDTH = 32
) (
  input  logic [WIDTH-1:0] a,
  input  logic [WIDTH-1:0] b,
  output logic [WIDTH-1:0] out
);
  assign out = a | b;
endmodule

// Bitwise XOR
module bitwise_xor #(
  parameter WIDTH = 32
) (
  input  logic [WIDTH-1:0] a,
  input  logic [WIDTH-1:0] b,
  output logic [WIDTH-1:0] out
);
  assign out = a ^ b;
endmodule

// Bitwise NOT
module bitwise_not #(
  parameter WIDTH = 32
) (
  input  logic [WIDTH-1:0] in,
  output logic [WIDTH-1:0] out
);
  assign out = ~in;
endmodule

// =============================================================================
// Reduction Operations
// =============================================================================

// Reduction AND
module reduction_and #(
  parameter WIDTH = 32
) (
  input  logic [WIDTH-1:0] in,
  output logic out
);
  assign out = &in;
endmodule

// Reduction OR
module reduction_or #(
  parameter WIDTH = 32
) (
  input  logic [WIDTH-1:0] in,
  output logic out
);
  assign out = |in;
endmodule

// Reduction XOR (parity)
module reduction_xor #(
  parameter WIDTH = 32
) (
  input  logic [WIDTH-1:0] in,
  output logic out
);
  assign out = ^in;
endmodule

// Logical NOT
module logical_not (
  input  logic in,
  output logic out
);
  assign out = !in;
endmodule

// =============================================================================
// Comparison Operations
// =============================================================================

// Equality comparator
module comparator_eq #(
  parameter WIDTH = 32
) (
  input  logic [WIDTH-1:0] a,
  input  logic [WIDTH-1:0] b,
  output logic out
);
  assign out = (a == b);
endmodule

// Inequality comparator
module comparator_neq #(
  parameter WIDTH = 32
) (
  input  logic [WIDTH-1:0] a,
  input  logic [WIDTH-1:0] b,
  output logic out
);
  assign out = (a != b);
endmodule

// Less than comparator
module comparator_lt #(
  parameter WIDTH = 32
) (
  input  logic [WIDTH-1:0] a,
  input  logic [WIDTH-1:0] b,
  output logic out
);
  assign out = (a < b);
endmodule

// Less than or equal comparator
module comparator_lte #(
  parameter WIDTH = 32
) (
  input  logic [WIDTH-1:0] a,
  input  logic [WIDTH-1:0] b,
  output logic out
);
  assign out = (a <= b);
endmodule

// Greater than comparator
module comparator_gt #(
  parameter WIDTH = 32
) (
  input  logic [WIDTH-1:0] a,
  input  logic [WIDTH-1:0] b,
  output logic out
);
  assign out = (a > b);
endmodule

// Greater than or equal comparator
module comparator_gte #(
  parameter WIDTH = 32
) (
  input  logic [WIDTH-1:0] a,
  input  logic [WIDTH-1:0] b,
  output logic out
);
  assign out = (a >= b);
endmodule

// =============================================================================
// Shift Operations
// =============================================================================

// Left shifter
module shifter_left #(
  parameter WIDTH = 32
) (
  input  logic [WIDTH-1:0] a,
  input  logic [WIDTH-1:0] b,
  output logic [WIDTH-1:0] out
);
  assign out = a << b;
endmodule

// Right shifter (logical)
module shifter_right #(
  parameter WIDTH = 32
) (
  input  logic [WIDTH-1:0] a,
  input  logic [WIDTH-1:0] b,
  output logic [WIDTH-1:0] out
);
  assign out = a >> b;
endmodule

// Right shifter (arithmetic)
module shifter_right_arith #(
  parameter WIDTH = 32
) (
  input  logic signed [WIDTH-1:0] a,
  input  logic [WIDTH-1:0] b,
  output logic signed [WIDTH-1:0] out
);
  assign out = a >>> b;
endmodule

// Barrel shifter (combinational)
module barrel_shifter #(
  parameter WIDTH = 32
) (
  input  logic [WIDTH-1:0] in,
  input  logic [$clog2(WIDTH)-1:0] shift_amt,
  input  logic shift_right,  // 0=left, 1=right
  input  logic arith,        // 0=logical, 1=arithmetic
  output logic [WIDTH-1:0] out
);
  logic [WIDTH-1:0] temp;
  
  always_comb begin
    if (shift_right) begin
      if (arith)
        temp = $signed(in) >>> shift_amt;
      else
        temp = in >> shift_amt;
    end else begin
      temp = in << shift_amt;
    end
  end
  
  assign out = temp;
endmodule

// =============================================================================
// Multiplexers and Selection
// =============================================================================

// 2-to-1 multiplexer
module mux2 #(
  parameter WIDTH = 32
) (
  input  logic sel,
  input  logic [WIDTH-1:0] in0,
  input  logic [WIDTH-1:0] in1,
  output logic [WIDTH-1:0] out
);
  assign out = sel ? in1 : in0;
endmodule

// 4-to-1 multiplexer
module mux4 #(
  parameter WIDTH = 32
) (
  input  logic [1:0] sel,
  input  logic [WIDTH-1:0] in0,
  input  logic [WIDTH-1:0] in1,
  input  logic [WIDTH-1:0] in2,
  input  logic [WIDTH-1:0] in3,
  output logic [WIDTH-1:0] out
);
  always_comb begin
    case (sel)
      2'b00: out = in0;
      2'b01: out = in1;
      2'b10: out = in2;
      2'b11: out = in3;
    endcase
  end
endmodule

/*
// N-to-1 multiplexer (parameterized)
module mux_n #(
  parameter WIDTH = 32,
  parameter N = 4
) (
  input  logic [$clog2(N)-1:0] sel,
  input  logic [WIDTH-1:0] in [N-1:0],
  output logic [WIDTH-1:0] out
);
  assign out = in[sel];
endmodule
*/

// Priority encoder
module priority_encoder #(
  parameter WIDTH = 8
) (
  input  logic [WIDTH-1:0] in,
  output logic [$clog2(WIDTH)-1:0] out,
  output logic valid
);
  integer i;
  always_comb begin
    out = '0;
    valid = 1'b0;
    for (i = WIDTH-1; i >= 0; i = i - 1) begin
      if (in[i]) begin
        out = i[$clog2(WIDTH)-1:0];
        valid = 1'b1;
      end
    end
  end
endmodule

// One-hot to binary encoder
module onehot_to_binary #(
  parameter WIDTH = 8
) (
  input  logic [WIDTH-1:0] in,
  output logic [$clog2(WIDTH)-1:0] out
);
  integer i;
  always_comb begin
    out = '0;
    for (i = 0; i < WIDTH; i = i + 1) begin
      if (in[i])
        out = out | i[$clog2(WIDTH)-1:0];
    end
  end
endmodule

// =============================================================================
// Memory Elements
// =============================================================================

// Single-port RAM
module memory #(
  parameter ADDR_WIDTH = 10,
  parameter DATA_WIDTH = 32,
  parameter DEPTH = 1024
) (
  input  logic clk,
  input  logic we,
  input  logic [ADDR_WIDTH-1:0] addr,
  input  logic [DATA_WIDTH-1:0] din,
  output logic [DATA_WIDTH-1:0] dout
);
  logic [DATA_WIDTH-1:0] mem [0:DEPTH-1];
  
  always_ff @(posedge clk) begin
    if (we)
      mem[addr] <= din;
    dout <= mem[addr];
  end
endmodule

// Dual-port RAM
module memory_dual_port #(
  parameter ADDR_WIDTH = 10,
  parameter DATA_WIDTH = 32,
  parameter DEPTH = 1024
) (
  input  logic clk,
  
  // Port A
  input  logic we_a,
  input  logic [ADDR_WIDTH-1:0] addr_a,
  input  logic [DATA_WIDTH-1:0] din_a,
  output logic [DATA_WIDTH-1:0] dout_a,
  
  // Port B
  input  logic we_b,
  input  logic [ADDR_WIDTH-1:0] addr_b,
  input  logic [DATA_WIDTH-1:0] din_b,
  output logic [DATA_WIDTH-1:0] dout_b
);
  logic [DATA_WIDTH-1:0] mem [0:DEPTH-1];
  
  always_ff @(posedge clk) begin
    if (we_a)
      mem[addr_a] <= din_a;
    dout_a <= mem[addr_a];
    
    if (we_b)
      mem[addr_b] <= din_b;
    dout_b <= mem[addr_b];
  end
endmodule

// ROM (with initialization)
module rom #(
  parameter ADDR_WIDTH = 10,
  parameter DATA_WIDTH = 32,
  parameter DEPTH = 1024,
  parameter INIT_FILE = ""
) (
  input  logic clk,
  input  logic [ADDR_WIDTH-1:0] addr,
  output logic [DATA_WIDTH-1:0] dout
);
  logic [DATA_WIDTH-1:0] mem [0:DEPTH-1];
  
  initial begin
    if (INIT_FILE != "")
      $readmemh(INIT_FILE, mem);
  end
  
  always_ff @(posedge clk) begin
    dout <= mem[addr];
  end
endmodule

// Register file (multi-read, single-write)
module register_file #(
  parameter ADDR_WIDTH = 5,
  parameter DATA_WIDTH = 32,
  parameter NUM_REGS = 32
) (
  input  logic clk,
  input  logic we,
  input  logic [ADDR_WIDTH-1:0] waddr,
  input  logic [DATA_WIDTH-1:0] wdata,
  input  logic [ADDR_WIDTH-1:0] raddr1,
  output logic [DATA_WIDTH-1:0] rdata1,
  input  logic [ADDR_WIDTH-1:0] raddr2,
  output logic [DATA_WIDTH-1:0] rdata2
);
  logic [DATA_WIDTH-1:0] regs [0:NUM_REGS-1];
  
  always_ff @(posedge clk) begin
    if (we && waddr != 0)  // Register 0 typically hardwired to 0
      regs[waddr] <= wdata;
  end
  
  assign rdata1 = (raddr1 == 0) ? '0 : regs[raddr1];
  assign rdata2 = (raddr2 == 0) ? '0 : regs[raddr2];
endmodule

// FIFO
module fifo #(
  parameter DATA_WIDTH = 32,
  parameter DEPTH = 16,
  parameter ADDR_WIDTH = $clog2(DEPTH)
) (
  input  logic clk,
  input  logic rst,
  
  input  logic wr_en,
  input  logic [DATA_WIDTH-1:0] wr_data,
  output logic full,
  
  input  logic rd_en,
  output logic [DATA_WIDTH-1:0] rd_data,
  output logic empty,
  
  output logic [ADDR_WIDTH:0] count
);
  logic [DATA_WIDTH-1:0] mem [0:DEPTH-1];
  logic [ADDR_WIDTH-1:0] wr_ptr, rd_ptr;
  logic [ADDR_WIDTH:0] cnt;
  
  assign full = (cnt == DEPTH);
  assign empty = (cnt == 0);
  assign count = cnt;
  
  always_ff @(posedge clk or posedge rst) begin
    if (rst) begin
      wr_ptr <= '0;
      rd_ptr <= '0;
      cnt <= '0;
    end else begin
      if (wr_en && !full) begin
        mem[wr_ptr] <= wr_data;
        wr_ptr <= wr_ptr + 1'b1;
        cnt <= cnt + 1'b1;
      end
      
      if (rd_en && !empty) begin
        rd_ptr <= rd_ptr + 1'b1;
        cnt <= cnt - 1'b1;
      end
      
      if (wr_en && rd_en && !full && !empty) begin
        cnt <= cnt;  // Simultaneous read/write
      end
    end
  end
  
  assign rd_data = mem[rd_ptr];
endmodule

// =============================================================================
// Counters
// =============================================================================

// Up counter with enable and reset
module counter #(
  parameter WIDTH = 32
) (
  input  logic clk,
  input  logic rst,
  input  logic en,
  output logic [WIDTH-1:0] count
);
  always_ff @(posedge clk or posedge rst) begin
    if (rst)
      count <= '0;
    else if (en)
      count <= count + 1'b1;
  end
endmodule

// Up/down counter
module counter_updown #(
  parameter WIDTH = 32
) (
  input  logic clk,
  input  logic rst,
  input  logic en,
  input  logic up,  // 1=up, 0=down
  output logic [WIDTH-1:0] count
);
  always_ff @(posedge clk or posedge rst) begin
    if (rst)
      count <= '0;
    else if (en) begin
      if (up)
        count <= count + 1'b1;
      else
        count <= count - 1'b1;
    end
  end
endmodule

// Modulo counter (wraps at MAX)
module counter_modulo #(
  parameter WIDTH = 32,
  parameter MAX = 100
) (
  input  logic clk,
  input  logic rst,
  input  logic en,
  output logic [WIDTH-1:0] count,
  output logic tc  // Terminal count
);
  always_ff @(posedge clk or posedge rst) begin
    if (rst)
      count <= '0;
    else if (en) begin
      if (count == MAX - 1)
        count <= '0;
      else
        count <= count + 1'b1;
    end
  end
  
  assign tc = (count == MAX - 1) && en;
endmodule

// =============================================================================
// Edge Detectors
// =============================================================================

// Rising edge detector
module edge_detect_rising (
  input  logic clk,
  input  logic rst,
  input  logic in,
  output logic out
);
  logic in_d;
  
  always_ff @(posedge clk or posedge rst) begin
    if (rst)
      in_d <= 1'b0;
    else
      in_d <= in;
  end
  
  assign out = in && !in_d;
endmodule

// Falling edge detector
module edge_detect_falling (
  input  logic clk,
  input  logic rst,
  input  logic in,
  output logic out
);
  logic in_d;
  
  always_ff @(posedge clk or posedge rst) begin
    if (rst)
      in_d <= 1'b0;
    else
      in_d <= in;
  end
  
  assign out = !in && in_d;
endmodule

// Any edge detector
module edge_detect (
  input  logic clk,
  input  logic rst,
  input  logic in,
  output logic out
);
  logic in_d;
  
  always_ff @(posedge clk or posedge rst) begin
    if (rst)
      in_d <= 1'b0;
    else
      in_d <= in;
  end
  
  assign out = in ^ in_d;
endmodule

// =============================================================================
// Synchronizers
// =============================================================================

// Two-stage synchronizer for clock domain crossing
module synchronizer #(
  parameter WIDTH = 1,
  parameter STAGES = 2
) (
  input  logic clk,
  input  logic rst,
  input  logic [WIDTH-1:0] in,
  output logic [WIDTH-1:0] out
);
  logic [WIDTH-1:0] sync_chain [STAGES-1:0];
  
  always_ff @(posedge clk or posedge rst) begin
    if (rst) begin
      for (int i = 0; i < STAGES; i++)
        sync_chain[i] <= '0;
    end else begin
      sync_chain[0] <= in;
      for (int i = 1; i < STAGES; i++)
        sync_chain[i] <= sync_chain[i-1];
    end
  end
  
  assign out = sync_chain[STAGES-1];
endmodule

// =============================================================================
// Pulse Generators
// =============================================================================

// Single-cycle pulse generator
module pulse_generator (
  input  logic clk,
  input  logic rst,
  input  logic trigger,
  output logic pulse
);
  logic trigger_d;
  
  always_ff @(posedge clk or posedge rst) begin
    if (rst)
      trigger_d <= 1'b0;
    else
      trigger_d <= trigger;
  end
  
  assign pulse = trigger && !trigger_d;
endmodule

// Multi-cycle pulse generator
module pulse_generator_multi #(
  parameter CYCLES = 4
) (
  input  logic clk,
  input  logic rst,
  input  logic trigger,
  output logic pulse
);
  logic [$clog2(CYCLES+1)-1:0] count;
  logic active;
  
  always_ff @(posedge clk or posedge rst) begin
    if (rst) begin
      count <= '0;
      active <= 1'b0;
    end else begin
      if (trigger && !active) begin
        active <= 1'b1;
        count <= CYCLES - 1;
      end else if (active) begin
        if (count == 0)
          active <= 1'b0;
        else
          count <= count - 1'b1;
      end
    end
  end
  
  assign pulse = active;
endmodule

// Sign extender
module sign_extender #(
  parameter WIDTH_IN = 8,
  parameter WIDTH_OUT = 16
) (
  input  logic [WIDTH_IN-1:0]  in,
  output logic [WIDTH_OUT-1:0] out
);
  assign out = {{(WIDTH_OUT-WIDTH_IN){in[WIDTH_IN-1]}}, in};
endmodule

// Signed multiplier
module multiplier_signed #(
  parameter WIDTH = 16
) (
  input  logic signed [WIDTH-1:0] a,
  input  logic signed [WIDTH-1:0] b,
  output logic signed [WIDTH-1:0] out
);
  assign out = a * b;
endmodule
