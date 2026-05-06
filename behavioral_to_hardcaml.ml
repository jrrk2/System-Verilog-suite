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
  (* Per-element width for BArray signals — populated by the pre-pass
     in [create_circuit] from each bsignal's stype.  Lets BSelect
     compute the right slice for dynamic indexing. *)
  array_elem_w: (string, int) Hashtbl.t;
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
      (* Variable lookup priority: ctx.variables (Always.Variable.t)
         carries the live value of registers and wires written by
         processes; ctx.signals carries Signal.input port wires.
         Falling back to get_signal (which mints a 32-bit default
         wire) is a last resort and signals a missing pre-pass. *)
      (match List.assoc_opt name ctx.variables with
       | Some v -> Always.Variable.value v
       | None -> get_signal ctx name)

  | BConst { value; width } ->
      Signal.of_int ~width value

  | BBinOp { op; lhs; rhs; result_type } ->
      let s_lhs0 = expr_to_signal ctx lhs in
      let s_rhs0 = expr_to_signal ctx rhs in
      (* Width-coerce both operands to a common width.  Same const-
         narrowing trick as BCond: if one operand is a BConst and the
         other isn't, use the non-constant's width and truncate the
         constant.  Otherwise fall back to max-width zero-extension. *)
      let is_const = function BConst _ -> true | _ -> false in
      let wl = Signal.width s_lhs0 and wr = Signal.width s_rhs0 in
      let common_w =
        match is_const lhs, is_const rhs with
        | true,  false -> wr
        | false, true  -> wl
        | _ -> max wl wr in
      let pad s =
        let sw = Signal.width s in
        if sw = common_w then s
        else if sw > common_w then Signal.select s (common_w - 1) 0
        else Signal.uresize s common_w in
      let s_lhs = pad s_lhs0 and s_rhs = pad s_rhs0 in
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
      let wt = Signal.width s_then and we = Signal.width s_else in
      (* When one arm is a BConst that's wider than the other arm
         (typical: a default-32-bit localparam reset_value feeding a
         2-bit register), narrow the constant to match instead of
         padding the non-constant up.  Without this we'd compute a
         32-bit mux per output bit and only use the bottom 2 — every
         bit above gets emitted as cells that downstream
         [eliminate_dead_logic] sweeps, but only after the route
         pass already had to handle the inflated wires. *)
      let is_const = function BConst _ -> true | _ -> false in
      let w =
        match is_const then_val, is_const else_val with
        | true,  false -> we
        | false, true  -> wt
        | _ -> max wt we
      in
      let coerce s =
        let sw = Signal.width s in
        if sw = w then s
        else if sw > w then Signal.select s (w - 1) 0
        else Signal.uresize s w in
      Signal.mux2 s_cond (coerce s_then) (coerce s_else)

  | BSelect { array; index } ->
      let s_array = expr_to_signal ctx array in
      let s_index = expr_to_signal ctx index in
      (* Need the per-element width to know how many bits to peel off
         per index value.  Look it up from [ctx.array_elem_w] when
         the array root is a BVar; otherwise fall back to width-1
         bit-select (legacy behaviour). *)
      let elem_w =
        match array with
        | BVar n -> Hashtbl.find_opt ctx.array_elem_w n
        | _ -> None in
      (match elem_w with
       | None -> s_array
       | Some elem_w ->
           let total_w = Signal.width s_array in
           let size = total_w / elem_w in
           if size <= 1 then s_array
           else
             (* Build a mux: for each index value k, the slice
                array[(k+1)*elem_w - 1 : k*elem_w]. *)
             let cases = List.init size (fun k ->
               let hi = (k + 1) * elem_w - 1 in
               let lo = k * elem_w in
               Signal.select s_array hi lo) in
             Signal.mux s_index cases)

  | BSlice { signal; msb; lsb } ->
      let s = expr_to_signal ctx signal in
      (* Hardcaml's [Signal.select s hi lo] expects hi >= lo and
         returns bits [hi:lo] inclusive.  BIR's BSlice uses (msb, lsb)
         with msb = high index, lsb = low index — but the converter
         occasionally emits little-endian (`[0:N]`) where msb < lsb.
         Normalise here. *)
      let hi = max msb lsb and lo = min msb lsb in
      let hi = min hi (Signal.width s - 1) in
      Signal.select s hi lo

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

(* Convert behavioral statement to Always assignment.
   [is_reg] tells [get_or_create_var] whether new variables here
   should be flip-flops (sequential context) or wires (combinational).
   Passed down through nested if/case/block bodies. *)
let rec stmt_to_always ~is_reg ctx alw = function
  | BAssign { lhs; rhs } ->
      let rhs_signal = expr_to_signal ctx rhs in
      let rhs_w = Signal.width rhs_signal in
      let var = get_or_create_var ctx lhs rhs_w is_reg in
      (* Resize the RHS to match the declared LHS width.  The pre-pass
         in [create_circuit] sets each Always.Variable to its bsignal-
         declared width — when the RHS is wider (e.g. a 32-bit default
         localparam being stored into a 2-bit reg) or narrower we need
         to coerce, otherwise [Always.compile] later builds a mux of
         mismatched arms.  Truncate when wider, zero-extend when
         narrower; both sides agree this is the SystemVerilog rule for
         non-blocking assigns of unsigned integers. *)
      let var_w = Signal.width (Always.Variable.value var) in
      let rhs_signal' =
        if rhs_w = var_w then rhs_signal
        else if rhs_w > var_w then Signal.select rhs_signal (var_w - 1) 0
        else Signal.uresize rhs_signal var_w in
      Always.(var <-- rhs_signal') :: alw

  | BIf { condition; then_stmts; else_stmts } ->
      let cond_signal = expr_to_signal ctx condition in
      let then_alw = List.fold_left (stmt_to_always ~is_reg ctx) [] then_stmts in
      let else_alw = List.fold_left (stmt_to_always ~is_reg ctx) [] else_stmts in
      Always.(if_ cond_signal then_alw else_alw) :: alw

  | BCase { selector; cases; default } ->
      let sel_signal = expr_to_signal ctx selector in
      let case_list = List.map (fun (value, stmts) ->
        let val_signal = expr_to_signal ctx value in
        let case_alw = List.fold_left (stmt_to_always ~is_reg ctx) [] stmts in
        (val_signal, case_alw)
      ) cases in
      let default_alw =
        List.fold_left (stmt_to_always ~is_reg ctx) [] default in
      let rec build_cases = function
        | [] -> default_alw
        | (value, body) :: rest ->
            let cond = Signal.(sel_signal ==: value) in
            [Always.if_ cond body (build_cases rest)]
      in
      build_cases case_list @ alw

  | BWhile _ | BFor _ ->
      (* Loops need to be unrolled by behavioral_unroll.ml first. *)
      alw

  | BBlock stmts ->
      List.fold_left (stmt_to_always ~is_reg ctx) alw stmts

  | BCallStmt _ | BReturn _ ->
      alw

(* Convert behavioral process to HardCaml Always block.
   Returns the compiled Always.t so the caller can keep a list
   of all the always-blocks that make up the module. *)
let process_to_always ctx = function
  | BCombinational { body; _ } ->
      let alw = List.fold_left (stmt_to_always ~is_reg:false ctx) [] body in
      Always.compile alw

  | BSequential { clock; reset; reset_async; body; _ } ->
      let clk_sig = get_signal ctx clock in
      ctx.clock <- Some clk_sig;
      (* Bind ctx.reset only when the reset is ASYNC.  Sync resets
         are encoded as data-path muxes in the BIR body and do
         not belong on the flip-flop's reset port. *)
      (match reset, reset_async with
       | Some rst_name, true -> ctx.reset <- Some (get_signal ctx rst_name)
       | _ -> ());
      let alw = List.fold_left (stmt_to_always ~is_reg:true ctx) [] body in
      Always.compile alw

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

(* Build the (name, Signal.t) input list from a bmodule's
   declared inputs.  Each becomes a hardcaml [Signal.input] which
   gets wired up to the surrounding scope when the Circuit is
   created. *)
let build_inputs (bmod : Behavioral_ir.bmodule) =
  List.filter_map (fun (s : Behavioral_ir.bsignal) ->
    match s.direction with
    | `Input ->
        let w = width_of_btype s.stype in
        Some (s.name, Signal.input s.name w)
    | _ -> None) bmod.signals

(* Backward-compat shim for callers that pass an inputs assoc-list
   and expect (name, Signal.t) outputs back.  New code should use
   [create_circuit] directly. *)
let module_to_create (bmod : Behavioral_ir.bmodule) inputs =
  let scope = Scope.create () in
  let ctx = {
    signals = inputs;
    variables = [];
    scope;
    clock = None;
    reset = None;
    array_elem_w = Hashtbl.create 8;
  } in
  (* Clock/reset get bound by [process_to_always] when each
     BSequential block tells us its clock and reset signal names
     directly.  We deliberately do NOT recognise them by name
     (clk/CLK/reset/rst) — pattern, not name, is the source of
     truth.  Combinational-only modules end up with both fields
     None, which is correct. *)
  let _ = List.map (process_to_always ctx) bmod.processes in
  List.filter_map (fun (s : Behavioral_ir.bsignal) ->
    match s.direction with
    | `Output ->
        let driver =
          match List.assoc_opt s.name ctx.variables with
          | Some var -> Always.Variable.value var
          | None ->
              (match List.assoc_opt s.name ctx.signals with
               | Some sig_ -> sig_
               | None -> Signal.zero (width_of_btype s.stype))
        in
        Some (s.name, driver)
    | _ -> None) bmod.signals

(* Convert a bmodule into a hardcaml [Circuit.t].  Produces a
   real circuit that can be passed to [Rtl.print] for Verilog
   emission and [Lib_map.map_bexpr] for technology mapping. *)
let create_circuit (bmod : Behavioral_ir.bmodule) =
  let scope = Scope.create () in
  let inputs = build_inputs bmod in
  let ctx = {
    signals  = inputs;
    variables = [];
    scope;
    clock = None;
    reset = None;
    array_elem_w = Hashtbl.create 8;
  } in

  (* Clock/reset get bound by [process_to_always] when each
     BSequential block names them via its [clock] / [reset] fields.
     Pattern, not name, is the source of truth. *)

  (* Pre-pass: classify each non-input signal and pre-declare an
     Always.Variable of the right shape (register vs wire) at the
     DECLARED width.

     For BSequential-driven signals we need clock and (optionally)
     reset to construct a Reg_spec.  Walk processes once,
     collecting (signal_name -> driving_BSequential_record).  Then
     create the Always.Variable per signal using either:
       Always.Variable.reg ~enable Reg_spec ~width    (register)
       Always.Variable.wire ~default                   (combinational)

     This is the hardcaml-lua pattern from Input_hardcaml.ml
     line 381-384, adapted to our BIR. *)
  (* Per-signal driving info: clock name, plus reset name only
     when the reset is ASYNC.  Sync reset is *not* a register-port
     property — the BIR body already encodes it as
       if (rst) q <= 0; else q <= D
     which becomes a data-path mux ahead of a plain DFF.  Mapping
     it as a register-port reset would be wrong (it'd stack two
     reset paths, one async by hardcaml default and one sync by
     the BIf in the body).  We only pass reset to Reg_spec when
     [reset_async = true]. *)
  let driver_proc : (string, (string * string option) option) Hashtbl.t =
    Hashtbl.create 16 in
  let rec scan_lhs ck_rst = function
    | [] -> ()
    | BAssign { lhs; _ } :: tl ->
        Hashtbl.replace driver_proc lhs ck_rst; scan_lhs ck_rst tl
    | BIf { then_stmts; else_stmts; _ } :: tl ->
        scan_lhs ck_rst then_stmts; scan_lhs ck_rst else_stmts; scan_lhs ck_rst tl
    | BCase { cases; default; _ } :: tl ->
        List.iter (fun (_, s) -> scan_lhs ck_rst s) cases;
        scan_lhs ck_rst default; scan_lhs ck_rst tl
    | BBlock s :: tl -> scan_lhs ck_rst s; scan_lhs ck_rst tl
    | _ :: tl -> scan_lhs ck_rst tl in
  List.iter (function
    | BSequential { clock; reset; reset_async; body; _ } ->
        let async_rst = if reset_async then reset else None in
        scan_lhs (Some (clock, async_rst)) body
    | BCombinational { body; _ } -> scan_lhs None body)
    bmod.processes;

  List.iter (fun (s : Behavioral_ir.bsignal) ->
    (* Record per-element width for BArray signals so [BSelect] can
       slice the flat representation correctly. *)
    (match s.stype with
     | BArray { element; _ } ->
         Hashtbl.replace ctx.array_elem_w s.name (width_of_btype element)
     | _ -> ());
    if s.direction <> `Input
       && not (List.mem_assoc s.name ctx.variables) then begin
      let w = width_of_btype s.stype in
      let var =
        match Hashtbl.find_opt driver_proc s.name with
        | Some (Some (clock, reset)) ->
            let clk = get_signal ctx clock in
            let spec = match reset with
              | Some rst -> Reg_spec.create ~clock:clk ~reset:(get_signal ctx rst) ()
              | None     -> Reg_spec.create ~clock:clk () in
            Always.Variable.reg spec ~width:w
        | _ ->
            Always.Variable.wire ~default:(Signal.zero w) in
      ctx.variables <- (s.name, var) :: ctx.variables
    end) bmod.signals;

  (* Coalesce all BCombinational processes into a single one before
     lowering.  Each [Always.compile] call drives the underlying wire
     of every Always.Variable assigned in its body — so two separate
     BCombinational processes that each assign the SAME variable
     would each try to drive the variable's wire, and hardcaml errors
     with "attempt to assign wire multiple times".  By merging, all
     assigns end up in one Always.compile and last-wins applies (which
     is the correct SV semantics for blocking continuous assigns).
     This also masks a known Verible-converter bug where indexed
     array-LHS like `assign arr[i] = …` loses the index and produces
     N separate BCombinational processes against the same bare name. *)
  let merged_combs =
    let bodies = List.filter_map (function
      | BCombinational { body; _ } -> Some body
      | _ -> None) bmod.processes in
    match bodies with
    | [] -> None
    | _ -> Some (BCombinational {
        name = "merged_comb";
        sensitivity = [BAny];
        body = List.concat bodies })
  in
  let other_processes = List.filter (function
    | BCombinational _ -> false
    | _ -> true) bmod.processes in
  let processes_to_run = match merged_combs with
    | Some p -> p :: other_processes
    | None -> other_processes in

  let _always_blocks =
    List.map (process_to_always ctx) processes_to_run in
  let _ = _always_blocks in    (* compiled side-effects are in ctx *)

  (* Wire up declared outputs to whatever variable / signal carries
     their final value.  An output that's never written becomes a
     zero so the Verilog still has a driver. *)
  let outputs = List.filter_map (fun (s : Behavioral_ir.bsignal) ->
    match s.direction with
    | `Output ->
        let w = width_of_btype s.stype in
        let driver =
          match List.assoc_opt s.name ctx.variables with
          | Some var -> Always.Variable.value var
          | None ->
              (match List.assoc_opt s.name ctx.signals with
               | Some sig_ -> sig_
               | None -> Signal.zero w)
        in
        Some (Signal.output s.name driver)
    | _ -> None) bmod.signals in

  Circuit.create_exn ~name:bmod.name outputs

(* Top-level: convert a whole bprogram, returning the FIRST
   module's circuit information.  Kept this shape for backward
   compatibility with z3_hardcaml_miter / interactive_client /
   test_surelog_hardcaml which all expect [option] of
   [(bmod, inputs, outputs)].  Use [convert_all_to_circuits] for
   the new-style multi-module case. *)
let convert_to_hardcaml (bprog : Behavioral_ir.bprogram) =
  match bprog.modules with
  | [] -> None
  | bmod :: _ ->
      let inputs = build_input_ports bmod in
      let outputs = build_output_ports bmod in
      Some (bmod, inputs, outputs)

(* New-style entry point: returns one Circuit.t per module. *)
let convert_all_to_circuits (bprog : Behavioral_ir.bprogram) =
  List.map (fun (m : Behavioral_ir.bmodule) -> (m.name, create_circuit m))
    bprog.modules

(* Convenience: emit a circuit's Verilog to a buffer via hardcaml's
   Rtl backend.  Mirrors hardcaml-lua's Input_hardcaml.ml line 768:

       Rtl.output ~output_mode:(Output_mode.To_buffer rtl) Verilog
         (Circuit.create_exn ~name:modnam outputs)

   The result string is what #100's cell-mapped post-pass and #101's
   yosys-as-oracle stage take as input. *)
let emit_verilog circuit =
  let buf = Buffer.create 4096 in
  Rtl.output ~output_mode:(Rtl.Output_mode.To_buffer buf) Verilog circuit;
  Buffer.contents buf
