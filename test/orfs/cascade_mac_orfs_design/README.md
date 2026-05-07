# cascade_mac ORFS design files

Drop `config.mk` and `constraint.sdc` into your local
`OpenROAD-flow-scripts/flow/designs/nangate45/cascade_mac/` and the
`cascade_mac.sv` source under
`OpenROAD-flow-scripts/flow/designs/src/cascade_mac/`, then run:

```sh
cd $HOME/OpenROAD-flow-scripts/flow
USE_DECOMP_SYNTH=1 \
  make DESIGN_CONFIG=designs/nangate45/cascade_mac/config.mk
```

`cascade_mac.sv` itself comes from `test/cascade/cascade_mac.sv`.
