(* Behavioral IR to HardCaml Conversion
 *
 * Converts optimized behavioral IR to HardCaml circuits for synthesis
 * and formal verification.
 *)

open Behavioral_ir
open Hardcaml

(* Context for tracking signals during conversion *)
type conv_context = {
  mutable signals: (string * Signal.t) list;
  mutable variables: (string * Always.Variable.t) list;
  scope: Scope.t;
  mutable clock: Signal.t option;
  mutable reset: Signal.t option;
}

(* Get width from behavioral IR type *)
let rec width_of_btype = function
  | BInt { width; _ } -> width
  | BBool -> 1
  | BArray { element; size } -> size * (width_of_btype element)
  | BStruct _ -> 32  (* Default *)

(* Get signal from context *)
let get_signal ctx name =
  match List.assoc_opt name ctx.signals with
  | Some s -> s
  | None ->
      (* Create wire if not found *)
      let width = 32 in  (* Default width *)
      let wire = Signal.wire width in
      ctx.signals <- (name, wire) :: ctx.signals;
      wire

(* Get or create variable from context *)
let get_or_create_var ctx name width is_reg =
  match List.assoc_opt name ctx.variables with
  | Some v -> v
  | None ->
      let v =
        if is_reg then
          match ctx.clock, ctx.reset with
          | Some clk, Some rst ->
              let spec = Reg_spec.create ~clock:clk ~reset:rst () in
              Always.Variable.reg spec ~width
          | Some clk, None ->
              let spec = Reg_spec.create ~clock:clk () in
              Always.Variable.reg spec ~width
          | _ ->
              Always.Variable.wire ~default:(Signal.zero width)
        else
          Always.Variable.wire ~default:(Signal.zero width)
      in
      ctx.variables <- (name, v) :: ctx.variables;
      v

(* Convert behavioral IR expression to HardCaml Signal *)
let rec expr_to_signal ctx = function
  | BVar name ->
      get_signal ctx name

  | BConst { value; width } ->
      Signal.of_int ~width value

  | BBinOp { op; lhs; rhs; result_type } ->
      let s_lhs = expr_to_signal ctx lhs in
      let s_rhs = expr_to_signal ctx rhs in
      let result_width = width_of_btype result_type in
      (match op with
       | BAdd -> Signal.(s_lhs +: s_rhs)
       | BSub -> Signal.(s_lhs -: s_rhs)
       | BMul -> Signal.(s_lhs *: s_rhs)
       | BDiv ->
           (* Division not synthesizable in HardCaml - use placeholder *)
           Printf.eprintf "Warning: Division not synthesizable, using zero\n";
           Signal.zero result_width
       | BMod ->
           (* Modulo not synthesizable in HardCaml - use placeholder *)
           Printf.eprintf "Warning: Modulo not synthesizable, using zero\n";
           Signal.zero result_width
       | BAnd -> Signal.(s_lhs &: s_rhs)
       | BOr -> Signal.(s_lhs |: s_rhs)
       | BXor -> Signal.(s_lhs ^: s_rhs)
       | BShl -> Signal.(sll s_lhs (Signal.to_int s_rhs))
       | BShr -> Signal.(srl s_lhs (Signal.to_int s_rhs))
       | BAshr -> Signal.(sra s_lhs (Signal.to_int s_rhs))
       | BEq -> Signal.(s_lhs ==: s_rhs)
       | BNe -> Signal.(s_lhs <>: s_rhs)
       | BLt -> Signal.(s_lhs <: s_rhs)
       | BLe -> Signal.(s_lhs <=: s_rhs)
       | BGt -> Signal.(s_lhs >: s_rhs)
       | BGe -> Signal.(s_lhs >=: s_rhs))

  | BUnOp { op; operand; result_type } ->
      let s_operand = expr_to_signal ctx operand in
      (match op with
       | BNot -> Signal.(~: s_operand)
       | BNeg -> Signal.(~: s_operand +: (Signal.of_int ~width:(Signal.width s_operand) 1))
       | BRedAnd ->
           (* Reduce AND: split into bits and AND them all *)
           let bits = List.init (Signal.width s_operand) (fun i ->
             Signal.bit s_operand i) in
           Signal.reduce ~f:(Signal.(&:)) bits
       | BRedOr ->
           (* Reduce OR: split into bits and OR them all *)
           let bits = List.init (Signal.width s_operand) (fun i ->
             Signal.bit s_operand i) in
           Signal.reduce ~f:(Signal.(|:)) bits
       | BRedXor ->
           (* Reduce XOR: split into bits and XOR them all *)
           let bits = List.init (Signal.width s_operand) (fun i ->
             Signal.bit s_operand i) in
           Signal.reduce ~f:(Signal.(^:)) bits)

  | BCond { condition; then_val; else_val } ->
      let s_cond = expr_to_signal ctx condition in
      let s_then = expr_to_signal ctx then_val in
      let s_else = expr_to_signal ctx else_val in
      Signal.mux2 s_cond s_then s_else

  | BSelect { array; index } ->
      let s_array = expr_to_signal ctx array in
      let s_index = expr_to_signal ctx index in
      (* Simplified: treat as direct selection *)
      s_array

  | BSlice { signal; msb; lsb } ->
      let s = expr_to_signal ctx signal in
      Signal.select s lsb (msb - lsb + 1)

  | BConcat exprs ->
      let signals = List.map (expr_to_signal ctx) exprs in
      Signal.concat_msb signals

  | BReplicate { count; value } ->
      let s_value = expr_to_signal ctx value in
      let replicated = List.init count (fun _ -> s_value) in
      Signal.concat_msb replicated

  | BCall _ ->
      (* Unsupported for now *)
      Signal.zero 32

(* Convert behavioral statement to Always assignment *)
let rec stmt_to_always ctx alw = function
  | BAssign { lhs; rhs } ->
      let rhs_signal = expr_to_signal ctx rhs in
      let width = Signal.width rhs_signal in
      let var = get_or_create_var ctx lhs width false in
      Always.(var <-- rhs_signal) :: alw

  | BIf { condition; then_stmts; else_stmts } ->
      let cond_signal = expr_to_signal ctx condition in
      let then_alw = List.fold_left (stmt_to_always ctx) [] then_stmts in
      let else_alw = List.fold_left (stmt_to_always ctx) [] else_stmts in
      Always.(if_ cond_signal then_alw else_alw) :: alw

  | BCase { selector; cases; default } ->
      let sel_signal = expr_to_signal ctx selector in
      let case_list = List.map (fun (value, stmts) ->
        let val_signal = expr_to_signal ctx value in
        let case_alw = List.fold_left (stmt_to_always ctx) [] stmts in
        (val_signal, case_alw)
      ) cases in
      let default_alw = List.fold_left (stmt_to_always ctx) [] default in
      (* HardCaml doesn't have direct case, use nested if-else *)
      let rec build_cases = function
        | [] -> default_alw
        | (value, body) :: rest ->
            let cond = Signal.(sel_signal ==: value) in
            [Always.if_ cond body (build_cases rest)]
      in
      build_cases case_list @ alw

  | BWhile _ | BFor _ ->
      (* Loops need to be unrolled - not supported in direct synthesis *)
      alw

  | BBlock stmts ->
      List.fold_left (stmt_to_always ctx) alw stmts

  | BCallStmt _ | BReturn _ ->
      (* Skip unsupported statements *)
      alw

(* Convert behavioral process to HardCaml Always block *)
let process_to_always ctx = function
  | BCombinational { body; _ } ->
      let alw = List.fold_left (stmt_to_always ctx) [] body in
      Some (Always.compile alw)

  | BSequential { clock; body; _ } ->
      (* Update context with clock *)
      let clk_sig = get_signal ctx clock in
      let updated_ctx = { ctx with clock = Some clk_sig } in
      let alw = List.fold_left (stmt_to_always updated_ctx) [] body in
      Some (Always.compile alw)

(* Build input interface from behavioral IR module *)
let build_input_ports (bmod : Behavioral_ir.bmodule) =
  List.filter_map (fun (signal : Behavioral_ir.bsignal) ->
    match signal.direction with
    | `Input -> Some (signal.name, width_of_btype signal.stype)
    | _ -> None
  ) bmod.signals

(* Build output interface from behavioral IR module *)
let build_output_ports (bmod : Behavioral_ir.bmodule) =
  List.filter_map (fun (signal : Behavioral_ir.bsignal) ->
    match signal.direction with
    | `Output -> Some (signal.name, width_of_btype signal.stype)
    | _ -> None
  ) bmod.signals

(* Convert behavioral IR module to HardCaml create function *)
let module_to_create (bmod : Behavioral_ir.bmodule) inputs =
  let scope = Scope.create () in

  (* Initialize context with input signals *)
  let ctx = {
    signals = List.map (fun (name, width) ->
      (name, List.assoc name inputs)
    ) (build_input_ports bmod);
    variables = [];
    scope = scope;
    clock = None;
    reset = None;
  } in

  (* Look for clock and reset signals *)
  List.iter (fun (signal : Behavioral_ir.bsignal) ->
    match signal.name with
    | "clock" | "clk" | "CLK" ->
        ctx.clock <- Some (get_signal ctx signal.name)
    | "reset" | "rst" | "RST" ->
        ctx.reset <- Some (get_signal ctx signal.name)
    | _ -> ()
  ) bmod.signals;

  (* Process all processes *)
  List.iter (fun proc ->
    ignore (process_to_always ctx proc)
  ) bmod.processes;

  (* Build output signals *)
  let outputs = List.map (fun (name, width) ->
    let signal =
      match List.assoc_opt name ctx.variables with
      | Some var -> Always.Variable.value var
      | None -> get_signal ctx name
    in
    (name, signal)
  ) (build_output_ports bmod) in

  outputs

(* Create a HardCaml circuit from behavioral IR module *)
let create_circuit (bmod : Behavioral_ir.bmodule) =
  let input_ports = build_input_ports bmod in
  let output_ports = build_output_ports bmod in

  (* Build a simple wrapper for now *)
  Printf.printf "Module: %s\n" bmod.name;
  Printf.printf "Inputs: %d\n" (List.length input_ports);
  Printf.printf "Outputs: %d\n" (List.length output_ports);

  (* Return port information *)
  (input_ports, output_ports)

(* High-level conversion function *)
let convert_to_hardcaml (bprog : Behavioral_ir.bprogram) =
  match bprog.modules with
  | [] ->
      Printf.eprintf "No modules to convert\n";
      None
  | bmod :: _ ->
      let (inputs, outputs) = create_circuit bmod in
      Some (bmod, inputs, outputs)
