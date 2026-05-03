// Constant function returning a struct used as a typed parameter
// default — the popcount/CVA6Cfg pattern in miniature.
//
// Top instantiates `child` twice with two different INPUT_WIDTH
// values pulled from the struct. Verible-side specialise_design
// should produce TWO child specialisations whose port widths reflect
// the resolved field values (8 and 16).
//
// If the const-fn evaluator is missing/buggy the override stays
// symbolic and we get one fused specialisation — visible to the
// FF-set comparator as a port-shape mismatch against any reference.

package cfg_pkg;
  typedef struct packed {
    int unsigned A;
    int unsigned B;
  } cfg_t;

  function automatic cfg_t mk_cfg();
    cfg_t cfg;
    cfg.A = 8;
    cfg.B = 16;
    return cfg;
  endfunction
endpackage

module child #(
    parameter int unsigned WIDTH = 8
) (
    input  logic [WIDTH-1:0] din,
    output logic [WIDTH-1:0] dout
);
  assign dout = din;
endmodule

module top
#(
    parameter cfg_pkg::cfg_t Cfg = cfg_pkg::mk_cfg()
)
(
    input  logic [Cfg.A-1:0] in_a,
    input  logic [Cfg.B-1:0] in_b,
    output logic [Cfg.A-1:0] out_a,
    output logic [Cfg.B-1:0] out_b
);
  child #(.WIDTH(Cfg.A)) u_a (.din(in_a), .dout(out_a));
  child #(.WIDTH(Cfg.B)) u_b (.din(in_b), .dout(out_b));
endmodule
