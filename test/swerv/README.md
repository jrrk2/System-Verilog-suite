# SweRV regression — RAM-variant identification

This regression sweeps the SweRV-EH1 design (from `synlig`'s test corpus
at `/home/jonathan/synlig/third_party/surelog/third_party/tests/CoresSweRV`)
through `cva6_ram_scan.exe` to enumerate every `bmem` the meminfer pass
recognises.

The single richest file is `design/lib/mem_lib.sv` — it declares 41
SRAM macro modules (`ram_<depth>x<width>`) that the rest of SweRV
instantiates. Each is the canonical "registered output, sync read on
miss-cycle, sync write" single-port BRAM idiom, so they should all
classify as `single_port_bram`.

To run:

```
./test/swerv/run_swerv_rams.sh
```

The `flist` file lists the include paths and prerequisite source files
in the order verilator needs them. Edit if the synlig corpus moves.
