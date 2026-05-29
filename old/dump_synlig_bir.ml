let () =
  let p = Sv_lua.load_frontend ~frontend:"synlig" ~top:"slib_counter"
            ~files:["/home/jonathan/System-Verilog-suite/sysver_tests/slib_counter.sv"] in
  print_endline (Behavioral_ir.string_of_bprogram p)
