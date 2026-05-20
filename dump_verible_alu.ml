let () =
  let p = Verible_to_behavioral.convert_files ~top:"alu"
            ["/home/jonathan/System-Verilog-suite/sysver_tests/alu.sv"] in
  print_endline (Behavioral_ir.string_of_bprogram p)
