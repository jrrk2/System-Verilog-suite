-- recipes/cdc_report.lua
--
-- Replaces test_cdc.ml: parse a design, run CDC analysis on the top
-- module, print the report.  ASIC-friendly (no FPGA-specific passes).
--
-- Caller globals:
--   TOP    string -- top-module name
--   FILES  table  -- source files
--   FRONTEND  string (optional, default "verible")

fe = FRONTEND or "verible"
print("recipe: cdc_report  top=" .. TOP .. "  frontend=" .. fe)

prog = svd.parse(fe, TOP, FILES)
prog = svd.meminfer(prog)  -- so memories don't masquerade as CDC paths
m = svd.pick(prog, TOP)
report = svd.cdc_analyse(m)
print(report)
