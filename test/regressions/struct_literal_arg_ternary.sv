// Exercises all three layers added in one shot:
//   - Struct literal recognition: `'{ FIELD: value, ... }`
//   - Function arg substitution: const-fn called with a struct value
//   - Ternary `?:` inside the function body
//
// `mk_cfg` takes a config struct and returns a derived struct where
// the W_OUT field depends on a ternary on the input W. The top
// module passes a struct literal, the parameter default folds via
// the function call, and the resulting Cfg.W_OUT propagates as the
// child's WIDTH override.
//
// Expected: child__W64 (because in_cfg.W = 64 → W_OUT = 64 via the
// ternary's true branch). If any of the three new layers misfires
// we'd see child__WCfg.W_OUT (symbolic), or child__W32 (else
// branch), or no specialisation at all.

package mk_pkg;
  typedef struct packed {
    int unsigned W;
  } in_cfg_t;

  typedef struct packed {
    int unsigned W_OUT;
  } out_cfg_t;

  function automatic out_cfg_t mk_cfg(in_cfg_t in_cfg);
    out_cfg_t cfg;
    cfg.W_OUT = (in_cfg.W >= 32) ? in_cfg.W : 8;
    return cfg;
  endfunction
endpackage

module child #(parameter int unsigned WIDTH = 8) (
    input  logic [WIDTH-1:0] din,
    output logic [WIDTH-1:0] dout
);
  assign dout = din;
endmodule

module top
#(parameter mk_pkg::out_cfg_t Cfg = mk_pkg::mk_cfg('{W: 64}))
(
    input  logic [Cfg.W_OUT-1:0] in_d,
    output logic [Cfg.W_OUT-1:0] out_d
);
  child #(.WIDTH(Cfg.W_OUT)) u (.din(in_d), .dout(out_d));
endmodule
