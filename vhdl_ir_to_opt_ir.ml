(* Convert VHDL→IR output to opt_ir format for Z3 verification *)

(* This just delegates to the function in vhdl_to_ir_iterate *)
let convert_to_opt_ir = Vhdl_to_ir_iterate.context_to_opt_ir
