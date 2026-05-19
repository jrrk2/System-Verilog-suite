// Regression for task #141 PStruct path. A struct-typed parameter with
// a named-key `'{F1: v1, F2: v2}` default — verible_to_behavioral's
// extract_body_params now harvests these into cur_struct_params, and
// eval_int folds `cfg.BHTEntries` to the concrete int (1024) instead
// of falling through to the Some 0 degenerate fallback.
//
// Concretely: width `[$clog2(cfg.BHTEntries)-1:0]` should evaluate to
// 10 bits (clog2(1024) = 10), not 0 bits.
typedef struct packed {
  int BHTEntries;
  int CacheSize;
} bht_cfg_t;

module struct_param_default #(
  parameter bht_cfg_t cfg = '{ BHTEntries: 1024, CacheSize: 16384 }
) (
  input  logic                          clk_i,
  input  logic [$clog2(cfg.BHTEntries)-1:0] addr_i,
  output logic                          hit_o
);
  always_ff @(posedge clk_i) hit_o <= (addr_i != '0);
endmodule
