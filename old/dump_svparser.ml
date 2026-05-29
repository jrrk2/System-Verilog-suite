let () =
  let p = Sv_lua.load_frontend ~frontend:"sv-parser" ~top:"uart_interrupt"
            ~files:["/home/jonathan/System-Verilog-suite/sysver_tests/uart_interrupt.sv"] in
  print_endline (Behavioral_ir.string_of_bprogram p)
