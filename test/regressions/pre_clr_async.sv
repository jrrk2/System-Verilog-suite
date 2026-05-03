// Three-edge async always: posedge clk OR posedge clear OR posedge
// preset. Vivado emits an RTL_REG_ASYNC cell with PRE and CLR pins;
// `mk_register` in ver_front_to_behavioral must wrap PRE → all-ones
// and CLR → zero around the synchronous body, otherwise the EDIF
// path drops the preset/clear semantics. Same shape as sr_ff8 but
// minimal (1-bit).
module pre_clr_async (
  input  logic clk,
  input  logic preset,
  input  logic clear,
  input  logic d,
  output logic q
);
  always @(posedge clk or posedge clear or posedge preset)
    if (preset)     q <= 1'b1;
    else if (clear) q <= 1'b0;
    else            q <= d;
endmodule
