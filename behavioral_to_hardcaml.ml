(* Behavioral IR to HardCaml Conversion
 *
 * Converts optimized behavioral IR to HardCaml circuits for synthesis
 * and formal verification.
 *)

open Behavioral_ir
open Hardcaml

(* Width-safe constant builder.  BConst.value is an arbitrary-precision Z.t, so
   a literal wider than 63 bits (common once whole SGMII/PCS designs flatten —
   96/128-bit shift-reg inits, wide config vectors) overflows Z.to_int and
   crashes create_circuit.  Build the exact bit pattern at the requested width
   directly instead of routing through int.  Z.testbit reads the correct low
   bits even for a negative (two's-complement) value. *)
let signal_of_z ~width value =
  let w = max 1 width in
  let s = String.init w (fun i -> if Z.testbit value (w - 1 - i) then '1' else '0') in
  Signal.of_constant (Constant.of_binary_string s)

(* Clamp a dynamic shift/index amount to clog2(width) bits before
   [Signal.log_shift].  log_shift builds one barrel stage per amount bit
   (stage k shifts by 2^k), so a 64-bit amount (e.g. a mis-widened
   `{bank, 6'b0}` bank-select shift in a multi-bank RAM) creates a 2^63
   stage that overflows OCaml's native int to a NEGATIVE shift and crashes
   Hardcaml's srl.  A shift >= the operand width yields 0 anyway, so
   ceil(log2 width) bits is exact for every real shift and removes the
   overflow. *)
let clog2 n = let rec f n a = if n <= 1 then a else f ((n + 1) / 2) (a + 1) in max 1 (f n 0)
(* NOTE the clamp must NOT silently truncate: for W=8 the amount 8 (4'b1000)
   truncated to clog2(8)=3 bits becomes 0 — a shift-by-8 WRAPPING to shift-by-0
   (found by the Vivado↔SVS cross-flow miter: `x >> x` with x=8 gave x, not 0).
   Amounts representable in `needed` bits are handled exactly by log_shift
   (stages compose; ≥W flushes naturally), so only the DROPPED high bits need
   handling: if any is set the true amount is ≥ 2^needed ≥ W — mux in the flush
   value (zeros for sll/srl, sign-replicate for sra). *)
let log_shift_clamped ?flush op s amt =
  let needed = clog2 (Signal.width s) in
  if Signal.width amt <= needed then Signal.log_shift op s amt
  else begin
    let low = Signal.select amt (needed - 1) 0 in
    let high = Signal.select amt (Signal.width amt - 1) needed in
    let overflow = Signal.(high <>:. 0) in
    let shifted = Signal.log_shift op s low in
    let fl = match flush with
      | Some f -> f
      | None -> Signal.zero (Signal.width s) in
    Signal.mux2 overflow fl shifted
  end

(* Context for tracking signals during conversion *)
type conv_context = {
  mutable signals: (string * Signal.t) list;
  mutable variables: (string * Always.Variable.t) list;
  scope: Scope.t;
  mutable clock: Signal.t option;
  mutable reset: Signal.t option;
  (* Reset is async + active-low when set (SV `negedge rstn` + `if (!rstn)`).
     Override Reg_spec's default Rising-edge reset so the emitted FF uses
     `negedge` sensitivity and the body's reset condition matches the
     source.  Otherwise picorv32's progmem o_ready (and any other negedge-
     async-reset FF) gets polarity-flipped and never leaves reset. *)
  mutable reset_falling: bool;
  (* Per-element width for BArray-typed signals — populated by the
     pre-pass in [create_circuit] from each bsignal's stype.  Lets
     [BSelect] compute the right slice when the index is dynamic. *)
  array_elem_w: (string, int) Hashtbl.t;
  (* BIR-level initial_value for each registered signal, keyed by
     signal name and stored as (width, int_value).  Set when the BIR
     bsignal has [initial_value = Some (BConst _)].  Threaded into
     each register's Reg_spec via [~reset_to] (with [reset] tied to
     gnd so runtime semantics are unchanged) so the constant survives
     onto the Reg signal's [reg_reset_value] field and downstream
     mappers (FPGA → FDRE INIT; ASIC → "would-need-explicit-reset"
     check) can read it back. *)
  initial_values: (string, int * Z.t) Hashtbl.t;  (* name -> (width, value) *)
  (* Net names tied to a constant by a GND/VCC primitive instance in the
     source (`GND GND_1 (.G(GND_2))` → GND_2 = 0, `VCC VCC_1 (.P(VCC_2))`
     → VCC_2 = 1).  Those tie cells are pruned during mapping, so a
     consumer resolving the net to the tie's dead box-output wire would
     land on a driverless net (Vivado Opt 31-2 / NDRV).  [get_signal]
     resolves any such name straight to a hardcaml constant. Value: true =
     one (VCC), false = zero (GND). *)
  const_nets: (string, bool) Hashtbl.t;
  (* Inferred ROMs (meminfer's `rom_<lhs>` decode tables + any BRom with an
     init image), keyed by name → (data_width, init_values).  behavioral_to_
     verilog emits these as `reg rom_x[]` + init block; the Hardcaml gate-map
     path has no memory model, so a `x = rom_x[sel]` BSelect is lowered to a
     combinational Signal.mux over the init constants (ROM-as-case). *)
  roms: (string, int * int list) Hashtbl.t;
}

(* Collect net names that a GND/VCC primitive instance ties to a constant,
   plus the well-known `<const0>`/`<const1>` sentinels. *)
let collect_const_nets (bmod : Behavioral_ir.bmodule) =
  let t = Hashtbl.create 16 in
  Hashtbl.replace t "GND" false; Hashtbl.replace t "<const0>" false;
  Hashtbl.replace t "VCC" true;  Hashtbl.replace t "<const1>" true;
  List.iter (fun (i : Behavioral_ir.binstance) ->
    let one = match i.module_name with "VCC" -> Some true | "GND" -> Some false | _ -> None in
    match one with
    | None -> ()
    | Some v ->
      (* the single output port (G for GND, P for VCC) names the const net *)
      List.iter (fun (_port, e) -> match e with
        | Behavioral_ir.BVar nm -> Hashtbl.replace t nm v
        | _ -> ()) i.port_connections) bmod.instances;
  t

(* Collect inferred ROMs (name -> data_width, init_values) so a
   `BSelect { array = BVar rom; index }` can be lowered to a Signal.mux
   over the init constants instead of hitting the unbound-identifier bomb. *)
let collect_roms (bmod : Behavioral_ir.bmodule) =
  let t = Hashtbl.create 8 in
  List.iter (fun (m : Behavioral_ir.bmem) ->
    if m.init_values <> [] then
      Hashtbl.replace t m.mname (m.data_width, m.init_values)) bmod.mems;
  t

(* Get width from behavioral IR type *)
let rec width_of_btype = function
  | BInt { width; _ } -> width
  | BBool -> 1
  | BArray { element; size } -> size * (width_of_btype element)
  | BStruct _ -> 32  (* Default *)

(* Get signal from context *)
let get_signal ctx name =
  (* Constant-sentinel nets: the source ties these with GND/VCC primitive
     instances (`GND GND (.G(<const0>))`).  Those tie cells are pruned during
     mapping, so a consumer that resolves `<const0>` to the tie's (now dead)
     box-output wire ends up on a DRIVERLESS net — Vivado flags it (e.g. an
     SRL D pin, Opt 31-2).  Resolve the sentinels straight to a hardcaml
     constant instead; of_circuit re-emits constant instance inputs as the
     "GND"/"VCC" nets that bir_to_edif/_json tie to n_GND/n_VCC. *)
  match Hashtbl.find_opt ctx.const_nets name with
  | Some true  -> Signal.vdd
  | Some false -> Signal.gnd
  | None ->
  match List.assoc_opt name ctx.signals with
  | Some s -> s
  | None ->
  (* A combinationally-driven wire lives in ctx.variables (an Always.Variable),
     not ctx.signals.  get_signal is used directly for a BSequential's CLOCK
     name (line ~1167); when a clock is such a wire — axi_vfifo derives a
     per-channel clock `axi_ch_0_ch_clk = m_axi_clk[0]` and clocks its FFs on it
     — missing the variables lookup fell through to the 32-bit-zero stub below,
     and Hardcaml rejected the resulting non-1-bit clock ("unexpected width").
     Resolve the variable's current value instead. *)
  match List.assoc_opt name ctx.variables with
  | Some v -> Always.Variable.value v
  | None ->
      (* Unknown name — almost always a bug upstream (missing input
         port, missing variable in pre-pass, expression referencing a
         free identifier).  Previously we minted an unassigned 32-bit
         wire; that produced the cryptic Hardcaml error
            "circuit input signal must have a port name (unassigned
             wire?)"
         at Circuit.create_exn time, with no useful pointer to which
         identifier was the offender.  Now we tie to a constant zero
         of width 32 so the circuit creates cleanly (the resulting
         design is almost certainly wrong wherever this fires, but at
         least layout proceeds and the diagnostic below names the
         identifier).                                                *)
      (* An unbound identifier here means create_circuit is about to build a
         SILENTLY WRONG gate netlist (the name reads as constant 0 wherever it
         fires).  That masked a real gate_map gap: inferred ROMs (`rom_*` from
         meminfer) and the program RAM are emitted by behavioral_to_verilog
         (verilog_of_mem) but create_circuit never consults bmod.mems, so every
         `x = rom_data_be[sel]` collapsed to 0 and the whole decode path went
         dead — with only a warning.  BOMB by default so the gap is caught, not
         shipped.  SVS_ALLOW_UNBOUND=1 restores the tie-to-zero fallback. *)
      let width = 32 in
      let z = Signal.zero width in
      let _ = Signal.(--) z ("__unbound_" ^ name) in
      ctx.signals <- (name, z) :: ctx.signals;
      if Sys.getenv_opt "SVS_ALLOW_UNBOUND" <> None then begin
        Printf.eprintf
          "[expr_to_signal] unbound identifier %s — tied to %d-bit zero \
           (SVS_ALLOW_UNBOUND; upstream BIR is missing a declaration or \
           driver — the gate netlist is WRONG wherever this fires)\n%!"
          name width;
        z
      end else
        failwith (Printf.sprintf
          "[behavioral_to_hardcaml] unbound identifier '%s' — create_circuit \
           has no signal/variable for it, so it would tie to constant 0 and \
           build a silently-wrong gate netlist.  Common cause: an inferred \
           ROM/RAM (rom_*/mem from meminfer) that the Hardcaml gate-map path \
           does not build (behavioral_to_verilog emits it via verilog_of_mem; \
           create_circuit must too), or a genuinely missing driver.  Set \
           SVS_ALLOW_UNBOUND=1 to tie to 32-bit zero and proceed anyway."
          name)

(* Non-bombing sibling of [get_signal]: resolve [name] to its Signal if one
   already exists (const-net sentinel / ctx.signals / ctx.variables), else
   None — no zero-stub, no failwith.  Used to detect a FORWARD REFERENCE when
   binding a register's clock/reset in the pre-pass: a clock/reset net can be
   declared AFTER the register that uses it (a flattened SoC's comb reset
   `rst_core_n = rst_sys_ni & ~ndmreset_req`), so its Always.Variable doesn't
   exist yet.  The caller then substitutes a placeholder Signal.wire and
   connects it once every variable has been created. *)
let get_signal_opt ctx name =
  match Hashtbl.find_opt ctx.const_nets name with
  | Some true  -> Some Signal.vdd
  | Some false -> Some Signal.gnd
  | None ->
  match List.assoc_opt name ctx.signals with
  | Some s -> Some s
  | None ->
  match List.assoc_opt name ctx.variables with
  | Some v -> Some (Always.Variable.value v)
  | None -> None

(* Get or create variable from context *)
let get_or_create_var ctx name width is_reg =
  match List.assoc_opt name ctx.variables with
  | Some v -> v
  | None ->
      let with_reset_edge spec =
        if ctx.reset_falling
        then Reg_spec.override spec ~reset_edge:Falling
        else spec in
      (* If this signal has a BIR-level initial_value, thread it into
         Reg_spec via [~reset_to] while tying [~reset] to gnd so the
         constant survives onto the Reg signal's [reg_reset_value] field
         (downstream mappers read it) without changing runtime
         semantics.  The actual runtime reset path (if any) is wired
         separately by [Reg_spec.create ~reset:rst]; for signals with
         an init but no explicit BIR reset, reset stays at gnd. *)
      let with_init spec =
        match Hashtbl.find_opt ctx.initial_values name with
        | None -> spec
        | Some (init_w, init_v) ->
          let const = signal_of_z ~width:(max width init_w) init_v in
          let const = if Signal.width const = width then const
                      else Signal.uresize const width in
          (* If no source-level reset is wired, point [~reset] at gnd
             (constant 0 — Hardcaml emits no reset logic).  Either way,
             [~reset_to] carries the init metadata.                   *)
          (match ctx.reset with
           | Some _ -> Reg_spec.override spec ~reset_to:const
           | None ->
             Reg_spec.override spec ~reset:Signal.gnd ~reset_to:const)
      in
      let v =
        if is_reg then
          match ctx.clock, ctx.reset with
          | Some clk, Some rst ->
              let spec = Reg_spec.create ~clock:clk ~reset:rst ()
                         |> with_reset_edge |> with_init in
              Always.Variable.reg spec ~width
          | Some clk, None ->
              let spec = Reg_spec.create ~clock:clk () |> with_init in
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
      (* clamp to >=1: a 0-width constant (degenerate slice / empty
         literal in flattened picorv32) would crash hardcaml's of_int. *)
      signal_of_z ~width value

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
      (* Optional dispatch to Hardcaml_circuits prefix-sum / Wallace
         trees when LIB_MAP_ADDER / LIB_MAP_MUL is set.  Default falls
         back to Hardcaml's `+:` / `*:` which `lib_map.gen_add` /
         `gen_mul` then bit-blast as ripple / Array.  The CLA path
         emits the explicit prefix-sum tree at the hardcaml level, so
         lib_map sees individual gates and the overall depth is O(log N)
         instead of O(N).  Trades ~10% area for ~30-40% adder delay. *)
      let prefix_sum_config () =
        match Sys.getenv_opt "LIB_MAP_ADDER" with
        | Some "sklansky"    -> Some Hardcaml_circuits.Prefix_sum.Config.Sklansky
        | Some "brent_kung"  -> Some Hardcaml_circuits.Prefix_sum.Config.Brent_kung
        | Some "kogge_stone" -> Some Hardcaml_circuits.Prefix_sum.Config.Kogge_stone
        | _ -> None
      in
      let mul_config () =
        match Sys.getenv_opt "LIB_MAP_MUL" with
        | Some "wallace" -> Some Hardcaml_circuits.Mul.Config.Wallace
        | Some "dadda"   -> Some Hardcaml_circuits.Mul.Config.Dadda
        | _ -> None
      in
      (* When the result is wider than the operands (`[8:0] y = a + b` with 8-bit
         a,b), widen the operands to the result width before adding so the
         carry-out lands in the high bit instead of being dropped by same-width
         `+:` (which would leave y[8] = 0). *)
      let add_w = max common_w result_width in
      (match op with
       | BAdd ->
           (match prefix_sum_config () with
            | None -> Signal.(uresize s_lhs add_w +: uresize s_rhs add_w)
            | Some config ->
                let s_full = Hardcaml_circuits.Prefix_sum.create
                  (module Signal) ~config
                  ~input1:(Signal.uresize s_lhs add_w)
                  ~input2:(Signal.uresize s_rhs add_w) ~carry_in:Signal.gnd in
                Signal.select s_full (add_w - 1) 0)
       | BSub ->
           (match prefix_sum_config () with
            | None -> Signal.(uresize s_lhs add_w -: uresize s_rhs add_w)
            | Some config ->
                (* a - b = a + ~b + 1, via prefix-sum with carry_in=1 *)
                let s_full = Hardcaml_circuits.Prefix_sum.create
                  (module Signal) ~config
                  ~input1:(Signal.uresize s_lhs add_w)
                  ~input2:Signal.(~: (uresize s_rhs add_w)) ~carry_in:Signal.vdd in
                Signal.select s_full (add_w - 1) 0)
       | BMul ->
           (match mul_config () with
            | None -> Signal.(s_lhs *: s_rhs)
            | Some config ->
                let s_full = Hardcaml_circuits.Mul.create ~config
                  (module Signal) s_lhs s_rhs in
                Signal.select s_full (common_w - 1) 0)
       | BDiv | BMod ->
           (* Hardcaml has no native /, %.  Constant/constant case has
            * already been folded by behavioral_const, so by the time
            * we get here the rhs is non-constant or the result depends
            * on a runtime lhs.  We accept one specialisation: divisor
            * is a positive power-of-two constant AND the operation is
            * unsigned — then `/` lowers to a logical right-shift and
            * `%` lowers to a low-bits mask.
            *
            * Signed `/` and `%` are NOT equivalent to shr/mask: signed
            * SV division truncates toward zero, whereas arithmetic
            * shift floors, so `-5 / 2 = -2` (SV) but `-5 >>> 1 = -3`
            * (sra).  We refuse the rewrite for signed ops and raise. *)
           let pow2_log2 n =
             if n <= 0 then None
             else if n land (n - 1) <> 0 then None
             else
               let rec lg i x = if x = 1 then Some i else lg (i + 1) (x lsr 1) in
               lg 0 n
           in
           let is_unsigned = match result_type with
             | BInt { signed = Unsigned; _ } -> true
             | _ -> false in
           (match rhs, is_unsigned with
            | BConst { value = c; _ }, true ->
                (match pow2_log2 (Z.to_int c) with
                 | Some k when op = BDiv -> Signal.srl s_lhs k
                 | Some k (* op = BMod *) ->
                     let mask = (1 lsl k) - 1 in
                     Signal.(s_lhs &: of_int ~width:common_w mask)
                 | None ->
                     failwith (Printf.sprintf
                       "behavioral_to_hardcaml: unsupported %s by non-pow2 constant %s"
                       (if op = BDiv then "/" else "%") (Z.to_string c)))
            | _ ->
                failwith (Printf.sprintf
                  "behavioral_to_hardcaml: unsupported %s — divisor not a positive power-of-two constant (or operation is signed)"
                  (if op = BDiv then "/" else "%")))
       | BAnd -> Signal.(s_lhs &: s_rhs)
       | BOr -> Signal.(s_lhs |: s_rhs)
       | BXor -> Signal.(s_lhs ^: s_rhs)
       (* Shifts: use [Signal.sll]/[srl]/[sra] (which take an int) when
          the amount is a constant; otherwise fall back to
          [Signal.log_shift] which accepts a dynamic amount.  cordic_sincos
          and similar shift-add iterative designs need dynamic shifts. *)
       | BShl ->
           (* SV evaluates the RHS of an assignment in the LHS context
              width, so a left shift like `decoded_imm <= slice[31:12] << 12`
              must shift in the LHS's 32-bit width — shifting in the
              slice's narrower self-determined width (20) loses every bit
              picorv32's decoder cares about (decoded_imm 0x03000 << 12
              becomes 0 instead of 0x03000000).  Widen s_lhs to result_width
              before the shift. *)
           let s = if result_width > Signal.width s_lhs
                   then Signal.uresize s_lhs result_width else s_lhs in
           (try Signal.(sll s (Signal.to_int s_rhs))
            with _ -> log_shift_clamped Signal.sll s s_rhs)
       | BShr ->
           (try Signal.(srl s_lhs (Signal.to_int s_rhs))
            with _ -> log_shift_clamped Signal.srl s_lhs s_rhs)
       | BAshr ->
           (try Signal.(sra s_lhs (Signal.to_int s_rhs))
            with _ ->
              (* overflow flush for arithmetic shift = all sign bits *)
              let fl = Signal.repeat (Signal.msb s_lhs) (Signal.width s_lhs) in
              log_shift_clamped ~flush:fl Signal.sra s_lhs s_rhs)
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
      (* mux2 needs a 1-bit selector.  Verilog's `cond ? a : b` accepts
         any width and treats non-zero as true; reduce a wider cond to
         OR-of-bits before muxing. *)
      let cond_w = Signal.width s_cond in
      let s_cond1 =
        if cond_w = 1 then s_cond
        else
          let bits = List.init cond_w (fun i -> Signal.bit s_cond i) in
          Signal.reduce ~f:(Signal.(|:)) bits in
      Signal.mux2 s_cond1 (coerce s_then) (coerce s_else)

  | BSelect { array = BVar rom_name; index }
      when Hashtbl.mem ctx.roms rom_name ->
      (* Inferred ROM read `rom[sel]` — the Hardcaml gate-map path has no
         memory model, so encode it as a combinational Signal.mux over the
         init constants (ROM-as-case), mirroring meminfer.build_rom_lookup and
         verilog_of_mem's reg-array + $readmemh on the behavioral side. *)
      let (dw, init) = Hashtbl.find ctx.roms rom_name in
      let dw = max 1 dw in
      let size = List.length init in
      let mask = if dw >= 62 then -1 else (1 lsl dw) - 1 in
      let cases = List.map (fun v -> Signal.of_int ~width:dw (v land mask)) init in
      if size <= 1 then (match cases with c :: _ -> c | [] -> Signal.zero dw)
      else begin
        let s_index = expr_to_signal ctx index in
        let need_w =
          let rec lg n = if n <= 1 then 0 else 1 + lg ((n + 1) / 2) in
          max 1 (lg size) in
        let s_index =
          let iw = Signal.width s_index in
          if iw = need_w then s_index
          else if iw > need_w then Signal.select s_index (need_w - 1) 0
          else Signal.uresize s_index need_w in
        Signal.mux s_index cases
      end

  | BSelect { array; index } ->
      let s_array = expr_to_signal ctx array in
      let s_index = expr_to_signal ctx index in
      (* Need elem_w to know the slice size per index value.  Look
         it up from [ctx.array_elem_w] (populated by the pre-pass)
         when the array root is a BVar; else fall back to the whole
         signal (legacy behaviour). *)
      let elem_w =
        match array with
        | BVar n -> Hashtbl.find_opt ctx.array_elem_w n
        | _ -> None in
      (match elem_w with
       | None ->
           (* Scalar (packed-reg) bit-select: BIR sometimes carries
              `reg[k]` as BSelect with a 32-bit constant index instead
              of the (msb=k,lsb=k) BSlice form.  Returning the whole
              signal is wrong: an outer `|reg[k]` then OR-reduces every
              bit and fires for any non-zero value (this is exactly the
              picorv32 MISALIGNED-INSTRUCTION trap firing on
              pc=0x00100000 — pc[0]=0 but |pc=1, so the gated trap
              branch wins on the first fetch).  Lower a constant index
              to a 1-bit slice, and a dynamic index to a 1-bit shift+
              mask so the result width matches SV `pc[idx]` semantics. *)
           let w_arr = Signal.width s_array in
           (match index with
            | BConst { value; _ } when Z.geq value Z.zero && Z.lt value (Z.of_int w_arr) ->
                Signal.select s_array (Z.to_int value) (Z.to_int value)
            | _ ->
                let shifted = log_shift_clamped Signal.srl s_array s_index in
                Signal.select shifted 0 0)
       | Some elem_w ->
           let total_w = Signal.width s_array in
           let size = total_w / elem_w in
           if size <= 1 then s_array
           else
             let cases = List.init size (fun k ->
               let hi = (k + 1) * elem_w - 1 in
               let lo = k * elem_w in
               Signal.select s_array hi lo) in
             (* Hardcaml's [mux] requires 2^width(sel) >= |cases|.
                The BIR index is often narrower than ceil_log2(size)
                (a 2-bit `i` reading a 64-entry array because i comes
                from a smaller iteration or dimension).  Pad the
                index up; out-of-range values get the highest case
                (Verilog spec: `arr[i]` with i out of range is x). *)
             let need_w =
               let rec lg n = if n <= 1 then 0 else 1 + lg ((n + 1) / 2) in
               lg size in
             (* Match the selector width to [need_w] = ceil_log2(size): pad a
                too-narrow index up, and — crucially — TRUNCATE a too-WIDE one
                (an `arr[idx]` where idx is a 32-/64-bit expression indexing a
                small array, e.g. ibex_alu's `imd_val_q_i[…]`).  Hardcaml's mux
                computes its bound as `1 lsl width(sel)`; a >=63-bit selector
                overflows OCaml's native int to a nonsense small value and it
                rejects the mux ("too many inputs (8) (maximum_expected 1)").
                need_w bits index exactly `size` cases; high bits are
                out-of-range (Verilog x) so dropping them is sound. *)
             let s_index =
               let iw = Signal.width s_index in
               if iw = need_w then s_index
               else if iw > need_w then Signal.select s_index (need_w - 1) 0
               else Signal.uresize s_index need_w in
             Signal.mux s_index cases)

  | BSlice { signal; msb; lsb } ->
      let s = expr_to_signal ctx signal in
      (* Hardcaml's [Signal.select s hi lo] expects hi >= lo and
         returns bits [hi:lo] inclusive.  BIR's BSlice uses (msb, lsb)
         with msb = high index, lsb = low index — but the converter
         occasionally emits little-endian (`[0:N]`) where msb < lsb.
         Normalise here.  When the slice goes entirely past the
         signal's width (e.g. width-21 signal selected as [20:82]
         after some bogus parse), clamp both ends so we never feed
         hi < lo into Signal.select; out-of-range bits read as 0.   *)
      let hi = max msb lsb and lo = min msb lsb in
      let w  = Signal.width s in
      if hi < 0 || lo >= w then
        (* Entirely out of range → Verilog reads x/0. *)
        Signal.zero (max 1 (hi - lo + 1))
      else if hi >= w || lo < 0 then
        (* PARTIALLY out of range on either end: zero-pad the underflow
           (lo < 0, e.g. a `[$bits(t)-1:0]` where a struct/param width folded
           to 0 → `[-1:0]`) and/or the overflow (hi >= w, e.g. arp_ctrl's
           MAC-hi `[47:32]` of a width-42 constant), keeping the valid middle.
           Verilog zero-extends out-of-range bits; a straight Signal.select
           would either crash (lo<0) or silently narrow the slice. *)
        let top = if hi > w - 1 then [Signal.zero (hi - (w - 1))] else [] in
        let bot = if lo < 0 then [Signal.zero (- lo)] else [] in
        let vhi = min hi (w - 1) and vlo = max lo 0 in
        Signal.concat_msb (top @ [Signal.select s vhi vlo] @ bot)
      else
        Signal.select s hi lo

  | BConcat exprs ->
      let signals = List.map (expr_to_signal ctx) exprs in
      Signal.concat_msb signals

  | BReplicate { count; value } ->
      let s_value = expr_to_signal ctx value in
      let replicated = List.init count (fun _ -> s_value) in
      Signal.concat_msb replicated

  (* `@signed(x)` is the converter's signedness annotation — it does not
     compute a different value, it just marks the operand as signed for
     downstream arithmetic.  Pass the operand through unchanged so the
     value survives; without this, every $signed(...) lowers to zero,
     which silently broke I-type immediate decoding (picorv32 line 1106
     decoded_imm := $signed(mem_rdata_q[31:20])) and propagated as a
     reg_op2 = 0 divergence at cyc 129 of the xsim co-sim. *)
  | BCall { func = "@signed"; args = [x] } -> expr_to_signal ctx x
  | BCall _ ->
      (* Unsupported for now *)
      Signal.zero 32

(* Convert behavioral statement to Always assignment.
   [is_reg] tells [get_or_create_var] whether new variables here
   should be flip-flops (sequential context) or wires (combinational).
   Passed down through nested if/case/block bodies. *)
let rec stmt_to_always ~is_reg ctx alw = function
  | BAssign { lhs; rhs } ->
      (* SystemVerilog `$signed(narrow_expr)` is a sign-extending cast:
         when assigned to a wider LHS, the upper bits are filled from
         the operand's MSB.  The Verible converter tags such RHSs as
         `BCall { func = "@signed"; args = [x] }`.  Detect that and
         use Signal.sresize instead of uresize when widening.
         picorv32's I-type/B-type/S-type immediate decoders all use
         $signed(...) on a narrower bit-select; without this they
         silently zero-extend and a negative branch offset like -8
         (B-type imm 13'h1ff8) lowers to +8184 in the gate, breaking
         the BLTU back-edge at cyc 159 of the xsim co-sim. *)
      let is_signed_cast = match rhs with
        | BCall { func = "@signed"; _ } -> true
        | _ -> false in
      let rhs_signal = expr_to_signal ctx rhs in
      let rhs_w = Signal.width rhs_signal in
      let var = get_or_create_var ctx lhs rhs_w is_reg in
      (* Resize the RHS to match the declared LHS width.  The pre-pass
         in [create_circuit] sets each Always.Variable to its bsignal-
         declared width — when the RHS is wider (e.g. a 32-bit default
         localparam being stored into a 2-bit reg) or narrower we need
         to coerce, otherwise [Always.compile] later builds a mux of
         mismatched arms.  Truncate when wider; for narrower, sign-
         extend if the RHS was `@signed(...)`, else zero-extend. *)
      let var_w = Signal.width (Always.Variable.value var) in
      let rhs_signal' =
        if rhs_w = var_w then rhs_signal
        else if rhs_w > var_w then Signal.select rhs_signal (var_w - 1) 0
        else if is_signed_cast then Signal.sresize rhs_signal var_w
        else Signal.uresize rhs_signal var_w in
      Always.(var <-- rhs_signal') :: alw

  | BIf { condition; then_stmts; else_stmts } ->
      let cond_signal = expr_to_signal ctx condition in
      (* Always.if_ needs a 1-bit guard; Verilog `if (cond)` accepts
         any width with non-zero meaning true.  Reduce wider conds. *)
      let cond_w = Signal.width cond_signal in
      let cond_signal =
        if cond_w = 1 then cond_signal
        else
          let bits = List.init cond_w (fun i -> Signal.bit cond_signal i) in
          Signal.reduce ~f:(Signal.(|:)) bits in
      let then_alw = List.fold_left (stmt_to_always ~is_reg ctx) [] then_stmts in
      let else_alw = List.fold_left (stmt_to_always ~is_reg ctx) [] else_stmts in
      (* Restore body order inside each branch — same reasoning as
         in process_to_always. *)
      Always.(if_ cond_signal (List.rev then_alw) (List.rev else_alw)) :: alw

  | BCase { selector; cases; default } ->
      let sel_signal = expr_to_signal ctx selector in
      let sel_w = Signal.width sel_signal in
      (* Coerce each case-key to the selector's width.  Verible-derived
         BIR routinely tags case constants with the largest BInt width
         in scope (commonly 32) even when the selector is much narrower
         (e.g. a 2-bit state register), and Hardcaml's [==:] rejects
         operand-width mismatches.  Truncate the constant if it's
         wider, zero-extend if narrower. *)
      let coerce_to_sel s =
        let w = Signal.width s in
        if w = sel_w then s
        else if w > sel_w then Signal.select s (sel_w - 1) 0
        else Signal.uresize s sel_w in
      let case_list = List.map (fun (value, stmts) ->
        let val_signal = coerce_to_sel (expr_to_signal ctx value) in
        let case_alw = List.fold_left (stmt_to_always ~is_reg ctx) [] stmts in
        (val_signal, List.rev case_alw)
      ) cases in
      let default_alw =
        List.rev (List.fold_left (stmt_to_always ~is_reg ctx) [] default) in
      (* Verilog `case` is PRIORITY (first match wins; casez/casex and
         duplicate/overlapping keys are not disjoint).  A balanced (parallel)
         mux tree is only SOUND when mutual exclusion is proven.  We prove it
         the cheap, certain way: every key is a constant and, after coercion to
         the selector width, all keys are pairwise distinct.  Otherwise keep the
         linear priority chain (exact Verilog semantics). *)
      let key_const (value, _) = match value with
        | BConst { value = v; _ } ->
            if sel_w >= 62 then Some v else Some (Z.logand v (Z.of_int ((1 lsl sel_w) - 1)))
        | _ -> None in
      let keys = List.map key_const cases in
      (* Opt-in (BALANCED_CASE=1): default keeps the exact-Verilog priority
         chain so other flows are untouched. *)
      let balanced_case_enabled = Sys.getenv_opt "BALANCED_CASE" = Some "1" in
      let mutually_exclusive =
        balanced_case_enabled
        && List.for_all (fun k -> k <> None) keys
        && (let ks = List.filter_map (fun x -> x) keys in
            List.length (List.sort_uniq compare ks) = List.length ks) in
      let split_half l =
        let n = List.length l in
        let rec go i acc = function
          | x :: xs when i < n / 2 -> go (i + 1) (x :: acc) xs
          | rest -> (List.rev acc, rest) in
        go 0 [] l in
      let rec build_priority = function
        | [] -> default_alw
        | (value, body) :: rest ->
            [ Always.if_ Signal.(sel_signal ==: value) body (build_priority rest) ]
      in
      let rec build_balanced = function
        | [] -> default_alw
        | [ (value, body) ] ->
            [ Always.if_ Signal.(sel_signal ==: value) body default_alw ]
        | cs ->
            let lo, hi = split_half cs in
            let lo_match =
              List.fold_left (fun acc (v, _) -> Signal.(acc |: (sel_signal ==: v)))
                Signal.gnd lo in
            [ Always.if_ lo_match (build_balanced lo) (build_balanced hi) ]
      in
      (if mutually_exclusive then build_balanced case_list
       else build_priority case_list) @ alw

  | BWhile _ | BFor _ ->
      (* Loops need to be unrolled by behavioral_unroll.ml first. *)
      alw

  | BBlock stmts ->
      List.fold_left (stmt_to_always ~is_reg ctx) alw stmts

  | BCallStmt { func; args } when func = "@mem_write" ->
      (* Array write: @mem_write(arr, idx, data).  Translate to a
         full-bus update of arr where the slot at idx becomes data
         and other slots keep their current values.  Needs elem_w
         from [ctx.array_elem_w] (populated by the pre-pass). *)
      (match args with
       | [BVar arr; idx_e; data_e] ->
           (match Hashtbl.find_opt ctx.array_elem_w arr with
            | None ->
                (* No BArray type info — caller pre-scan promoted a
                   sliced-LHS write on a flat BInt signal, which
                   needs a different translation (read-modify-write
                   into the underlying bus).  TODO #117: emit that
                   shape; for now drop silently and the parent
                   miter / synth will surface the gap. *)
                alw
            | Some elem_w ->
                let var = get_or_create_var ctx arr 0 is_reg in
                let total_w = Signal.width (Always.Variable.value var) in
                let size = total_w / elem_w in
                let s_idx = expr_to_signal ctx idx_e in
                let s_data0 = expr_to_signal ctx data_e in
                let s_data =
                  let dw = Signal.width s_data0 in
                  if dw = elem_w then s_data0
                  else if dw > elem_w then Signal.select s_data0 (elem_w - 1) 0
                  else Signal.uresize s_data0 elem_w in
                let cur = Always.Variable.value var in
                (* For each slot k, pick (idx==k ? data : cur_slot_k). *)
                let slots = List.init size (fun k ->
                  let hi = (k + 1) * elem_w - 1 in
                  let lo = k * elem_w in
                  let cur_slot = Signal.select cur hi lo in
                  let k_const =
                    Signal.of_int ~width:(Signal.width s_idx) k in
                  Signal.mux2 (Signal.(s_idx ==: k_const)) s_data cur_slot
                ) in
                (* Concat MSB-first: slots[size-1] is the MSB slice. *)
                let new_val =
                  Signal.concat_msb (List.rev slots) in
                Always.(var <-- new_val) :: alw)
       | _ -> alw)
  | BCallStmt { func; args }
    when func = "@part_sel_write_up" || func = "@part_sel_write_down" ->
      (* `name[base +: width] <= rhs` (up) or `name[base -: width] <= rhs`
         (down).  width is constant; base may be dynamic.  Translate to a
         full-bus update of name where the slot whose lsb equals base
         gets rhs and other slots self-read.  Aligned (base is an integer
         multiple of width); otherwise fall through and let synth surface
         the gap. *)
      (match args with
       | [BVar lhs; base_e; BConst { value = w; _ }; data_e]
         when Z.gt w Z.zero ->
           let w = Z.to_int w in
           let var = get_or_create_var ctx lhs 1 is_reg in
           let total_w = Signal.width (Always.Variable.value var) in
           if total_w = 0 || total_w mod w <> 0 then alw
           else
             let n = total_w / w in
             let cur = Always.Variable.value var in
             let s_base = expr_to_signal ctx base_e in
             let s_data = expr_to_signal ctx data_e in
             let s_data =
               let dw = Signal.width s_data in
               if dw = w then s_data
               else if dw > w then Signal.select s_data (w - 1) 0
               else Signal.uresize s_data w in
             let slots = List.init n (fun k ->
               let lsb = match func with
                 | "@part_sel_write_up"   -> k * w
                 | "@part_sel_write_down" -> (k + 1) * w - 1
                 | _ -> k * w in
               let hi = (k + 1) * w - 1 and lo = k * w in
               let cur_slot = Signal.select cur hi lo in
               let bw = Signal.width s_base in
               let k_const = Signal.of_int ~width:bw lsb in
               Signal.mux2 (Signal.(s_base ==: k_const)) s_data cur_slot) in
             let new_val = Signal.concat_msb (List.rev slots) in
             Always.(var <-- new_val) :: alw
       | _ -> alw)
  | BCallStmt _ | BReturn _ ->
      alw

(* Thread Verilog blocking-assignment semantics over a process body.
 *
 * Hardcaml's [Always] reads a variable's [.value] as its *final*
 * post-compile wire and lets a later *unconditional* assignment override
 * an earlier one.  But iflift encodes a no-else `if (c) x = v;` as the
 * unconditional self-referencing assignment `x := (c ? v : x)`.  Fed
 * naively to [Always] a `default; conditional-override` run collapses to
 * just the last assignment with [x.value] in its else — a combinational
 * *loop* (and the default is silently dropped).  The same happens for a
 * signal re-assigned several times inside one `if`/`case` branch (e.g.
 * picorv32's unrolled nibble-carry multiplier `next_rd = f(next_rd,…)`).
 *
 * This pass threads a value-so-far environment (signal -> expression)
 * through the body, *including into* BIf/BCase branches (each branch
 * inherits the entering environment), so every read resolves to the
 * value computed so far and each straight-line run collapses to one
 * self-reference-free assignment.  Crucially it *keeps* the BIf/BCase
 * control flow rather than flattening it to BConds: a branch that
 * assigns a signal with no prior default must fall through to Hardcaml's
 * implicit-keep, which bottoms out at the wire's 0 default (matching a
 * `(* full_case *)` synthesis) instead of a self-referencing latch.
 *
 * Before each conditional we materialise the current value of every
 * signal the conditional re-assigns (so the implicit else picks up the
 * intended default), then drop those signals from the environment so
 * later reads fall through to [BVar] (= Hardcaml's merged value / a
 * register's Q).  Array/slice [BCallStmt]s pass through with their
 * argument expressions threaded.  Works for both combinational and
 * sequential bodies. *)
let thread_body ?(blocking_vars = [])
    ?(width_of = (fun (_ : string) -> (None : int option)))
    ?(elem_w_of = (fun (_ : string) -> (None : int option)))
    ?(is_comb = false)
    body =
  let is_blocking name = List.mem name blocking_vars in
  let rec subst env e =
    match e with
    | BVar n -> (match Hashtbl.find_opt env n with Some v -> v | None -> e)
    | BConst _ -> e
    | BBinOp { op; lhs; rhs; result_type } ->
        BBinOp { op; lhs = subst env lhs; rhs = subst env rhs; result_type }
    | BUnOp { op; operand; result_type } ->
        BUnOp { op; operand = subst env operand; result_type }
    | BCond { condition; then_val; else_val } ->
        BCond { condition = subst env condition;
                then_val = subst env then_val;
                else_val = subst env else_val }
    | BConcat es -> BConcat (List.map (subst env) es)
    | BReplicate { count; value } -> BReplicate { count; value = subst env value }
    | BSelect { array; index } ->
        BSelect { array = subst env array; index = subst env index }
    | BSlice { signal; msb; lsb } -> BSlice { signal = subst env signal; msb; lsb }
    | BCall { func; args } -> BCall { func; args = List.map (subst env) args }
  in
  (* All signal names assigned anywhere in a stmt list, incl. nested. *)
  let assigned_scalars stmts =
    let acc = ref [] in
    let add nm = if not (List.mem nm !acc) then acc := nm :: !acc in
    let rec go = function
      | BAssign { lhs; _ } -> add lhs
      (* Partial writes fold into the same threaded value (see the BCallStmt
         handler), so their target is "assigned" for branch-materialisation. *)
      | BCallStmt { func; args = BVar name :: _ }
        when func = "@mem_write" || func = "@slice_write"
          || func = "@part_sel_write_up" || func = "@part_sel_write_down" ->
          add name
      | BIf { then_stmts; else_stmts; _ } -> List.iter go then_stmts; List.iter go else_stmts
      | BCase { cases; default; _ } ->
          List.iter (fun (_, ss) -> List.iter go ss) cases; List.iter go default
      | BBlock ss -> List.iter go ss
      | _ -> ()
    in
    List.iter go stmts; !acc
  in
  (* Splice top-level begin/end groups so a run is seen as one list. *)
  let rec flatten_top = function
    | [] -> []
    | BBlock ss :: rest -> flatten_top (ss @ rest)
    | s :: rest -> s :: flatten_top rest
  in
  (* env: shared mutable value-so-far, snapshotted/restored across branches
     so each branch threads from the entering environment. *)
  let env : (string, bexpr) Hashtbl.t = Hashtbl.create 64 in
  let snapshot () = Hashtbl.fold (fun k v a -> (k, v) :: a) env [] in
  let restore snap = Hashtbl.reset env; List.iter (fun (k, v) -> Hashtbl.replace env k v) snap in
  (* Un-lift iflift's no-else encoding: a self-referencing conditional
     `lhs := (c ? v : lhs)` is `if (c) lhs := v;` with the else meaning
     "keep prior".  Emitting it as a flat (unconditional) assignment makes
     Hardcaml's last-write-wins OVERRIDE any earlier conditional drive of
     lhs (e.g. picorv32's `if (ecall) cpu_state <= trap` would wipe out
     the whole FSM `case`).  Re-expressing it as a guarded `if_` lets
     Hardcaml's priority chain compose it on top of the prior value. *)
  let rec unlift lhs rhs =
    match rhs with
    | BCond { condition; then_val; else_val = BVar n } when n = lhs ->
        Some (BIf { condition; then_stmts = [ BAssign { lhs; rhs = then_val } ];
                    else_stmts = [] })
    | BCond { condition; then_val = BVar n; else_val } when n = lhs ->
        Some (BIf { condition = BUnOp { op = BNot; operand = condition; result_type = BBool };
                    then_stmts = [ BAssign { lhs; rhs = else_val } ]; else_stmts = [] })
    | BCond { condition; then_val; else_val } ->
        (match unlift lhs else_val with
         | Some inner ->
             Some (BIf { condition; then_stmts = [ BAssign { lhs; rhs = then_val } ];
                         else_stmts = [ inner ] })
         | None -> None)
    | _ -> None
  in
  let emit_assign out lhs rhs =
    match unlift lhs rhs with
    | Some bif -> out := bif :: !out
    | None -> out := BAssign { lhs; rhs } :: !out
  in
  (* Emit the value-so-far of each [name] still pending in env, as the
     materialised default for the implicit else of a following
     conditional.  Keep it in env so the branches inherit it; the caller
     drops them after threading the branches. *)
  let materialise out names =
    List.iter (fun k ->
      match Hashtbl.find_opt env k with
      | Some v -> emit_assign out k v
      | None -> ()) names
  in
  let rec thread stmts =
    let out = ref [] in
    let local = ref [] in   (* signals flat-assigned at this level *)
    List.iter (fun s ->
      match s with
      | BAssign { lhs; rhs } ->
          let rhs' = subst env rhs in
          (* Only blocking (`=`) LHSes get threaded into env so subsequent
             reads see the in-cycle value.  Non-blocking (`<=`) LHSes keep
             SV semantics — reads see the registered Q, not the value
             scheduled in this cycle — by staying out of env entirely; we
             emit them directly via emit_assign (which still un-lifts
             self-referencing BConds so multi-drive composes correctly). *)
          if is_blocking lhs then begin
            Hashtbl.replace env lhs rhs';
            if not (List.mem lhs !local) then local := lhs :: !local
          end else
            emit_assign out lhs rhs'
      | BIf { condition; then_stmts; else_stmts } ->
          let assigned = assigned_scalars [s] in
          materialise out assigned;
          let cond' = subst env condition in
          let snap = snapshot () in
          let te = thread then_stmts in
          let env_then = snapshot () in
          restore snap;
          let ee = thread else_stmts in
          let env_else = snapshot () in
          restore snap;
          out := BIf { condition = cond'; then_stmts = te; else_stmts = ee } :: !out;
          (* For SV blocking (`=`) vars: their in-cycle value is whatever
             the branch computed, merged via BCond on the threaded guard.
             Keep that in env so later reads inline the merged expression
             (matching SV blocking semantics; the FF stays driven by the
             emitted BIf and is dead-code-eliminated if no external read).
             For non-blocking (`<=`) vars: reads after the barrier should
             see Q (the previous clock's value), so just drop them from
             env and let BVar fall through to the register output. *)
          List.iter (fun k ->
            if is_blocking k then
              let pv () = match Hashtbl.find_opt env k with
                | Some v -> v | None -> BVar k in
              let vt = match List.assoc_opt k env_then with Some v -> v | None -> pv () in
              let ve = match List.assoc_opt k env_else with Some v -> v | None -> pv () in
              let merged = if vt = ve then vt
                           else BCond { condition = cond'; then_val = vt; else_val = ve } in
              Hashtbl.replace env k merged
            else
              Hashtbl.remove env k
          ) assigned
      | BCase { selector; cases; default } ->
          let assigned = assigned_scalars [s] in
          materialise out assigned;
          let sel' = subst env selector in
          let snap = snapshot () in
          let case_envs = List.map (fun (k, ss) ->
            let k' = subst env k in
            let ss' = thread ss in
            let env_case = snapshot () in
            restore snap;
            (k', ss', env_case)) cases in
          let default' = thread default in
          let env_default = snapshot () in
          restore snap;
          let cases' = List.map (fun (k', ss', _) -> (k', ss')) case_envs in
          out := BCase { selector = sel'; cases = cases'; default = default' } :: !out;
          (* Same blocking/non-blocking split as BIf, merged across all
             arms: blocking k threads `sel'==k0 ? v0 : sel'==k1 ? v1 : ...
             : default`; non-blocking k drops from env. *)
          List.iter (fun k ->
            if is_blocking k then
              let pv () = match Hashtbl.find_opt env k with
                | Some v -> v | None -> BVar k in
              let v_def = match List.assoc_opt k env_default with
                | Some v -> v | None -> pv () in
              let merged =
                List.fold_right (fun (k_val, _, env_case) acc ->
                  let v_arm = match List.assoc_opt k env_case with
                    | Some v -> v | None -> pv () in
                  if v_arm = acc then acc
                  else BCond {
                    condition = BBinOp { op = BEq; lhs = sel'; rhs = k_val;
                                         result_type = BBool };
                    then_val = v_arm; else_val = acc }
                ) case_envs v_def
              in
              Hashtbl.replace env k merged
            else
              Hashtbl.remove env k
          ) assigned
      | BCallStmt { func; args } ->
          (* Fold a partial write into the value-so-far, so a combinational
             `default; part-write` run collapses to one self-reference-free
             BAssign instead of a full-bus read-modify-write against the
             Variable's empty block-entry value (→ Hardcaml "Combinational
             loop"; lowRISC dm_sba's `be_mask[be_idx]`/`be_mask[base+:2]`).
             `set_field name lsb fw data` overwrites `fw` bits at bit-offset
             `lsb` of the current threaded value via mask/shift (dynamic lsb
             OK).  Real multi-bit unpacked arrays are handled elsewhere
             (coalesce_comb_mem_writes / lower_mem_writes_in_seq); only
             packed-vector-style writes fold here. *)
          let set_field name lsb_expr fw data =
            match width_of name with
            | None -> None
            | Some w when fw >= 1 && fw <= w ->
                let ty = BInt { width = w; signed = Unsigned } in
                (* Base value the writes modify.  Use the threaded value-so-far
                   if present; otherwise for COMBINATIONAL logic an as-yet-
                   unwritten target is undriven → default ZERO, NOT a self-read
                   (`BVar name`), which would be a structural combinational loop
                   even when every bit is later overwritten (ibex_alu's bit-
                   reverse `for(i) rev[i]=src[31-i]`).  Sequential targets keep
                   the self-read (register keep-old-value). *)
                let cur =
                  match Hashtbl.find_opt env name with
                  | Some v -> v
                  | None -> if is_comb then BConst { value = Z.zero; width = w }
                            else BVar name in
                let mask = BConst {
                  value = Z.sub (Z.shift_left Z.one fw) Z.one; width = w } in
                let mask_sh = BBinOp { op = BShl; lhs = mask;
                                       rhs = lsb_expr; result_type = ty } in
                let cleared = BBinOp { op = BAnd; lhs = cur;
                  rhs = BUnOp { op = BNot; operand = mask_sh; result_type = ty };
                  result_type = ty } in
                let data_m = BBinOp { op = BAnd; lhs = data; rhs = mask;
                                      result_type = ty } in
                let data_sh = BBinOp { op = BShl; lhs = data_m;
                                       rhs = lsb_expr; result_type = ty } in
                Some (BBinOp { op = BOr; lhs = cleared; rhs = data_sh;
                               result_type = ty })
            | _ -> None in
          let u32 = BInt { width = 32; signed = Unsigned } in
          let folded =
            match func, args with
            | "@part_sel_write_up",
              [BVar name; base; BConst { value = w; _ }; data]
              when Z.gt w Z.zero ->
                set_field name (subst env base) (Z.to_int w) (subst env data)
            | "@part_sel_write_down",
              [BVar name; base; BConst { value = w; _ }; data]
              when Z.gt w Z.zero ->
                let wi = Z.to_int w in
                let lsb = BBinOp { op = BSub; lhs = subst env base;
                  rhs = BConst { value = Z.of_int (wi - 1); width = 32 };
                  result_type = u32 } in
                set_field name lsb wi (subst env data)
            | "@mem_write", [BVar name; idx; data]
              when (match elem_w_of name with Some e -> e = 1 | None -> true) ->
                (* packed-vector single-bit write (real arrays pre-folded). *)
                set_field name (subst env idx) 1 (subst env data)
            (* @slice_write (constant bit-range) — fold into the running value the
               SAME way as @part_sel_write (COMBINATIONAL only; sequential targets
               stay with merge_slice_writes_deep, whose nested-branch handling is
               silicon-validated).  merge_slice_writes batch-merged and APPENDED the
               collapsed base behind a later conditional override (`ld[..]=base;
               if(c) ld[..]=v` → base ran last, always winning) — wrong for the
               not-taken branch (ibex_counter counter_load).  set_field threads it in
               order and uses comb-ZERO for unwritten bits (no self-read loop). *)
            | "@slice_write", [BVar name; BConst { value = hi; _ }; BConst { value = lo; _ }; data]
              when is_comb && Z.geq hi lo ->
                let lo_i = Z.to_int lo and hi_i = Z.to_int hi in
                set_field name (BConst { value = Z.of_int lo_i; width = 32 })
                  (hi_i - lo_i + 1) (subst env data)
            | _ -> None in
          (match folded, args with
           | Some newv, BVar name :: _ ->
               Hashtbl.replace env name newv;
               if not (List.mem name !local) then local := name :: !local
           | _ ->
               out := BCallStmt { func; args = List.map (subst env) args }
                      :: !out)
      | other -> out := other :: !out
    ) (flatten_top stmts);
    (* flush signals assigned at this level that survived to the end *)
    let finals = ref [] in
    List.iter (fun k ->
      match Hashtbl.find_opt env k with
      | Some v -> emit_assign finals k v
      | None -> ()) (List.rev !local);
    List.rev !out @ List.rev !finals
  in
  thread body

(* Pre-process a process body to merge @slice_write calls per
   target into single full-bus read-modify-write BAssigns.  Each
   `name[hi:lo] <= data` pattern at the converter level becomes
   a `@slice_write(name, hi, lo, data)` BCallStmt; gathering all
   such calls per (name, body) and combining into one
   `name := BConcat[...]` makes the resulting hardcaml Always block
   produce one `var <-- ...` with the right read-modify-write
   semantics — multiple stacked `var <-- ...` calls on the same
   variable would otherwise overwrite each other.  Self-reads of
   uncovered bits are emitted via [BSlice]. *)
let merge_slice_writes ctx body =
  let groups : (string, (int * int * bexpr) list) Hashtbl.t = Hashtbl.create 4 in
  let rest = ref [] in
  let const_of = function
    | BConst { value; _ } -> Some value
    | _ -> None in
  List.iter (fun stmt ->
    match stmt with
    | BCallStmt { func = "@slice_write";
                  args = [BVar arr; m; l; data] } ->
        (match const_of m, const_of l with
         | Some msb, Some lsb ->
             let prev = try Hashtbl.find groups arr with Not_found -> [] in
             Hashtbl.replace groups arr ((Z.to_int msb, Z.to_int lsb, data) :: prev)
         | _ -> rest := stmt :: !rest)
    | other -> rest := other :: !rest
  ) body;
  let merged_assigns = Hashtbl.fold (fun arr writes acc ->
    let total_w =
      match List.assoc_opt arr ctx.variables with
      | Some v -> Signal.width (Always.Variable.value v)
      | None ->
          (match List.assoc_opt arr ctx.signals with
           | Some s -> Signal.width s
           | None -> 0) in
    if total_w = 0 then acc
    else
      let sorted = List.sort (fun (a,_,_) (b,_,_) -> compare b a) writes in
      let parts = ref [] in
      let cursor = ref (total_w - 1) in
      List.iter (fun (msb, lsb, data) ->
        if msb < !cursor then begin
          let hi = !cursor and lo = msb + 1 in
          parts := BSlice { signal = BVar arr; msb = hi; lsb = lo } :: !parts;
        end;
        parts := data :: !parts;
        cursor := lsb - 1
      ) sorted;
      if !cursor >= 0 then begin
        let hi = !cursor and lo = 0 in
        parts := BSlice { signal = BVar arr; msb = hi; lsb = lo } :: !parts;
      end;
      let rhs = match List.rev !parts with
        | [single] -> single
        | many -> BConcat many in
      BAssign { lhs = arr; rhs } :: acc
  ) groups [] in
  List.rev !rest @ merged_assigns

(* @slice_write calls live NESTED in case arms / if branches (arp_ctrl's
   `target_ip[31:24] <= rdata[55:48]` sits three levels deep) — the
   top-level-only merge left them to stmt_to_always' silent BCallStmt
   catch-all: the write's ENABLE materialised but its DATA vanished
   (FDCE with D=1'b0; ARP target-IP compare always failed on silicon).
   Recurse into every branch: same-branch writes to one register merge
   into a single read-modify-write BAssign, exactly NBA semantics. *)
let rec merge_slice_writes_deep ctx body =
  let body = merge_slice_writes ctx body in
  List.map (function
    | BIf r ->
        BIf { r with then_stmts = merge_slice_writes_deep ctx r.then_stmts;
                     else_stmts = merge_slice_writes_deep ctx r.else_stmts }
    | BCase r ->
        BCase { selector = r.selector;
                cases = List.map (fun (k, b) ->
                  (k, merge_slice_writes_deep ctx b)) r.cases;
                default = merge_slice_writes_deep ctx r.default }
    | BBlock ss -> BBlock (merge_slice_writes_deep ctx ss)
    | s -> s) body

(* Coalesce guarded, multi-bit ARRAY element writes inside a SEQUENTIAL
   (clocked) process body into one full-array [BAssign] per array — the
   [lower_mem_writes_in_seq] the comb path's [coalesce_comb_mem_writes]
   comment refers to.  Without this, several `if(en_i) arr[i] <= data_i`
   statements each lower (in stmt_to_always) to an INDEPENDENT full-bus
   `arr <-- {other slots = arr's block-entry Q, slot i = data_i}` guarded
   by en_i.  Hardcaml/Always then chains them as last-wins muxes whose
   NON-target slots all read Q — so when two enables fire in the SAME
   cycle (ibex_fetch_fifo pushes a new word while shifting the queue:
   entry_en can be 0b011/0b110) the lower-priority slot's write is
   CLOBBERED and its data freezes.  (thread_body already folds elem_w=1
   packed-vector writes correctly via set_field; only real multi-bit
   unpacked arrays fall through here.)

   Fix: rebuild each written array as ONE `arr := {slot_{n-1}..slot_0}`
   where slot k folds its writes in source order (later = higher
   priority) with the ELSE leg = self-read `arr[k]` (register keep-old).
   Every slot is therefore INDEPENDENT — simultaneous enables to
   different slots no longer interfere.  Single-write arrays reduce to
   the identical logic the old path produced, so silicon-validated
   sequential netlists are unchanged. *)
let coalesce_seq_mem_writes ctx body =
  let bool1 = BInt { width = 1; signed = Unsigned } in
  (* elem-width + slot-count of an array target, from the pre-pass
     array_elem_w and the (already-created) Always.Variable width. *)
  let arr_ew_size arr =
    match Hashtbl.find_opt ctx.array_elem_w arr,
          List.assoc_opt arr ctx.variables with
    | Some ew, Some v when ew > 0 ->
        let t = Signal.width (Always.Variable.value v) in
        if t > 0 && t mod ew = 0 then Some (ew, t / ew) else None
    | _ -> None in
  let order = ref [] in
  let writes : (string, (bexpr option * bexpr * bexpr) list) Hashtbl.t =
    Hashtbl.create 8 in
  let and_g g c = match g with
    | None -> Some c
    | Some g0 -> Some (BBinOp { op = BAnd; lhs = g0; rhs = c; result_type = bool1 }) in
  let rec collect guard = function
    | BBlock b -> List.iter (collect guard) b
    | BCallStmt { func = "@mem_write"; args = [BVar arr; idx; data] }
      when arr_ew_size arr <> None ->
        if not (Hashtbl.mem writes arr) then order := arr :: !order;
        let cur = try Hashtbl.find writes arr with Not_found -> [] in
        Hashtbl.replace writes arr (cur @ [(guard, idx, data)])
    | BIf { condition; then_stmts; else_stmts } ->
        let notc = BUnOp { op = BNot; operand = condition; result_type = bool1 } in
        List.iter (collect (and_g guard condition)) then_stmts;
        List.iter (collect (and_g guard notc)) else_stmts
    | _ -> () in
  List.iter (collect None) body;
  if Hashtbl.length writes = 0 then body
  else begin
    let const_idx = function
      | BConst { value; _ } -> Some (Z.to_int value) | _ -> None in
    let folded = Hashtbl.create 8 in
    let coalesced = List.filter_map (fun arr ->
      match arr_ew_size arr with
      | Some (ew, size) when size > 0 ->
          Hashtbl.replace folded arr ();
          let pairs = Hashtbl.find writes arr in
          (* self-read of slot idx (register keep-old) resolves an RMW
             `arr[i] <= f(arr[i])` write to the slot's value-so-far. *)
          let rec subst_self idx acc e = match e with
            | BSelect { array = BVar a; index } when a = arr && index = idx -> acc
            | BSelect { array; index } ->
                BSelect { array = subst_self idx acc array;
                          index = subst_self idx acc index }
            | BBinOp r -> BBinOp { r with lhs = subst_self idx acc r.lhs;
                                          rhs = subst_self idx acc r.rhs }
            | BUnOp r -> BUnOp { r with operand = subst_self idx acc r.operand }
            | BCond r -> BCond { condition = subst_self idx acc r.condition;
                                 then_val = subst_self idx acc r.then_val;
                                 else_val = subst_self idx acc r.else_val }
            | BConcat es -> BConcat (List.map (subst_self idx acc) es)
            | BSlice r -> BSlice { r with signal = subst_self idx acc r.signal }
            | BReplicate r -> BReplicate { r with value = subst_self idx acc r.value }
            | BCall r -> BCall { r with args = List.map (subst_self idx acc) r.args }
            | e -> e in
          let parts = List.init size (fun j ->
            let k = size - 1 - j in
            let kc = BConst { value = Z.of_int k; width = 32 } in
            (* ELSE default = self-read arr[k] (register hold). *)
            let init = BSelect { array = BVar arr; index = kc } in
            List.fold_left (fun acc (guard, idx, data) ->
              match const_idx idx with
              | Some ci when ci <> k -> acc      (* other slot — no effect *)
              | Some _ ->                         (* this slot; idx == k *)
                  let d = BSlice { signal = subst_self idx acc data;
                                   msb = ew - 1; lsb = 0 } in
                  (match guard with
                   | None -> d
                   | Some g -> BCond { condition = g; then_val = d; else_val = acc })
              | None ->                           (* dynamic index *)
                  let eq = BBinOp { op = BEq; lhs = idx; rhs = kc; result_type = bool1 } in
                  let cond = match guard with
                    | None -> eq
                    | Some g -> BBinOp { op = BAnd; lhs = g; rhs = eq; result_type = bool1 } in
                  BCond { condition = cond;
                          then_val = BSlice { signal = subst_self idx acc data;
                                              msb = ew - 1; lsb = 0 };
                          else_val = acc }) init pairs) in
          Some (BAssign { lhs = arr; rhs = BConcat parts })
      | _ -> None) (List.rev !order) in
    (* Strip the coalesced arrays' @mem_writes wherever nested, keeping
       BIf structure that still holds other (scalar / non-folded) writes. *)
    let rec strip ss = List.filter_map (fun s -> match s with
      | BCallStmt { func = "@mem_write"; args = [BVar arr; _; _] }
        when Hashtbl.mem folded arr -> None
      | BBlock b -> (match strip b with [] -> None | b' -> Some (BBlock b'))
      | BIf { condition; then_stmts; else_stmts } ->
          let t = strip then_stmts and e = strip else_stmts in
          if t = [] && e = [] then None
          else Some (BIf { condition; then_stmts = t; else_stmts = e })
      | other -> Some other) ss in
    strip body @ coalesced
  end

(* Convert behavioral process to HardCaml Always block.
   Returns the compiled Always.t so the caller can keep a list
   of all the always-blocks that make up the module. *)
let process_to_always ctx = function
  | BCombinational { body; _ } ->
      let width_of name =
        match List.assoc_opt name ctx.variables with
        | Some v -> Some (Signal.width (Always.Variable.value v))
        | None ->
            (match List.assoc_opt name ctx.signals with
             | Some s -> Some (Signal.width s) | None -> None) in
      let elem_w_of name = Hashtbl.find_opt ctx.array_elem_w name in
      (* thread_body only threads `blocking_vars` through the value-so-far env;
         the partial-write fold needs its targets threaded so a comb
         `default; part-write` collapses to one BAssign (dm_sba be_mask).  The
         nested-self-ref case (dmi_jtag dtmcs) is handled earlier by the
         blocking→non-blocking conversion pass, so leaving ordinary comb signals
         off the blocking list keeps silicon-validated netlists (eth-arp)
         bit-for-bit unchanged. *)
      let partial_targets =
        let acc = ref [] in
        let add n = if not (List.mem n !acc) then acc := n :: !acc in
        let rec go = function
          | BCallStmt { func; args = BVar n :: _ }
            when func = "@mem_write"
              || func = "@part_sel_write_up" || func = "@part_sel_write_down"
              || func = "@slice_write" ->
              add n
          | BIf { then_stmts; else_stmts; _ } ->
              List.iter go then_stmts; List.iter go else_stmts
          | BCase { cases; default; _ } ->
              List.iter (fun (_, ss) -> List.iter go ss) cases; List.iter go default
          | BBlock ss -> List.iter go ss
          | _ -> () in
        List.iter go body; !acc in
      let body =
        thread_body ~blocking_vars:partial_targets ~width_of ~elem_w_of
          ~is_comb:true body in
      (* shallow only: the merged assign SELF-READS uncovered bits, which
         is FF-old-value semantics — valid for NBA registers, a
         combinational LOOP here (axis_gmii_tx gate_map crashed) *)
      let body = merge_slice_writes ctx body in
      let alw = List.fold_left (stmt_to_always ~is_reg:false ctx) [] body in
      (* stmt_to_always prepends each compiled statement to the
         accumulator, so [alw] is in reverse body order.  Hardcaml's
         Always.compile processes the list forward and lets later
         statements override earlier ones — reversing here restores
         Verilog's body-order (last-wins) semantics. *)
      Always.compile (List.rev alw)

  | BSequential { clock; reset; reset_async; reset_edge; body; blocking_vars; _ } ->
      let clk_sig = get_signal ctx clock in
      ctx.clock <- Some clk_sig;
      (* For an async negedge reset (`always @(posedge clk or negedge rstn)
         if (!rstn) ...`) flip Reg_spec's default Rising-edge reset to
         Falling, so the emitted FF uses `negedge rstn` sensitivity and
         the body's reset condition matches the source.  Without this,
         picorv32's progmem o_ready stays in reset forever and the picosoc
         mem_ready handshake stalls. *)
      (match reset, reset_async with
       | Some rst_name, true ->
           ctx.reset <- Some (get_signal ctx rst_name);
           ctx.reset_falling <- (reset_edge = Some `Neg)
       | _ -> ctx.reset_falling <- false);
      let width_of name =
        match List.assoc_opt name ctx.variables with
        | Some v -> Some (Signal.width (Always.Variable.value v))
        | None ->
            (match List.assoc_opt name ctx.signals with
             | Some s -> Some (Signal.width s) | None -> None) in
      let elem_w_of name = Hashtbl.find_opt ctx.array_elem_w name in
      let body = thread_body ~blocking_vars ~width_of ~elem_w_of body in
      let body = merge_slice_writes_deep ctx body in
      (* Coalesce guarded multi-bit array element writes per array so
         simultaneous entry-enables (ibex_fetch_fifo push+shift) do not
         clobber each other — see [coalesce_seq_mem_writes]. *)
      let body = coalesce_seq_mem_writes ctx body in
      let alw = List.fold_left (stmt_to_always ~is_reg:true ctx) [] body in
      Always.compile (List.rev alw)

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
    array_elem_w = Hashtbl.create 8; reset_falling = false;
    initial_values = Hashtbl.create 16;
    const_nets = collect_const_nets bmod;
    roms = collect_roms bmod;
  } in
  List.iter (fun (s : Behavioral_ir.bsignal) ->
    match s.initial_value with
    | Some (BConst { value; width }) ->
        Hashtbl.replace ctx.initial_values s.name (width, value)
    | _ -> ()) bmod.signals;
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
let create_circuit ?(emit_instances = false) ?(detect_loops = true)
    ?(port_dir : (string -> string -> [ `Input | `Output ] option) = fun _ _ -> None)
    (bmod : Behavioral_ir.bmodule) =
  let scope = Scope.create () in
  let inputs = build_inputs bmod in
  let ctx = {
    signals  = inputs;
    variables = [];
    scope;
    clock = None;
    reset = None;
    array_elem_w = Hashtbl.create 8; reset_falling = false;
    initial_values = Hashtbl.create 16;
    const_nets = collect_const_nets bmod;
    roms = collect_roms bmod;
  } in

  (* Clock/reset get bound by [process_to_always] when each
     BSequential block names them via its [clock] / [reset] fields.
     Pattern, not name, is the source of truth. *)

  (* FORWARD-REFERENCE placeholders for clock/reset nets.  The register
     pre-pass below builds each FF's Reg_spec while iterating signals, and a
     clock/reset net may be declared/created LATER (a flattened SoC's comb
     reset `rst_core_n`).  When [resolve_clkrst] can't yet see the net it hands
     back a fresh Signal.wire recorded here, and [connect_deferred_clkrst]
     drives every such wire from the net's real signal once all variables and
     processes exist — so binding is independent of declaration order and of
     whether the net turns out to be a wire or a register.  Nets that resolve
     immediately (inputs, already-created wires) never enter this table, so
     conflict-free designs are byte-for-byte unchanged. *)
  let deferred_clkrst : (string, Signal.t) Hashtbl.t = Hashtbl.create 8 in
  let sig_width name =
    match List.find_opt (fun (s : Behavioral_ir.bsignal) -> s.name = name)
            bmod.signals with
    | Some s -> max 1 (width_of_btype s.stype) | None -> 1 in
  let resolve_clkrst name =
    match Hashtbl.find_opt deferred_clkrst name with
    | Some ph -> ph
    | None ->
      (match get_signal_opt ctx name with
       | Some s -> s
       | None ->
           let ph = Signal.wire (sig_width name) in
           Hashtbl.replace deferred_clkrst name ph;
           ignore (Signal.(--) ph ("__deferred_clkrst_" ^ name));
           ph) in
  let connect_deferred_clkrst () =
    Hashtbl.iter (fun name ph ->
      match get_signal_opt ctx name with
      | Some s ->
          let s =
            if Signal.width s = Signal.width ph then s
            else if Signal.width s > Signal.width ph
            then Signal.select s (Signal.width ph - 1) 0
            else Signal.uresize s (Signal.width ph) in
          Signal.(ph <== s)
      | None ->
          failwith (Printf.sprintf
            "[behavioral_to_hardcaml] clock/reset net '%s' is used by a \
             register but has no driver anywhere in the module — genuinely \
             undriven (not just a forward reference)." name))
      deferred_clkrst in

  (* Collect BIR-level initial_value per signal.  Used in
     [get_or_create_var] to thread an init through Reg_spec.            *)
  List.iter (fun (s : Behavioral_ir.bsignal) ->
    match s.initial_value with
    | Some (BConst { value; width }) ->
        Hashtbl.replace ctx.initial_values s.name (width, value)
    | _ -> ()) bmod.signals;

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
  (* Per-signal driver info: (clock_name, async_reset_name_opt,
     reset_falling).  `reset_falling = true` means the source SV says
     `negedge rstn` + `if (!rstn)` — we must override Reg_spec's default
     Rising-edge async reset to Falling, else the FF gets stuck in
     reset (active polarity is flipped). *)
  let driver_proc : (string, (string * string option * bool) option) Hashtbl.t =
    Hashtbl.create 16 in
  let rec scan_lhs ck_rst = function
    | [] -> ()
    | BAssign { lhs; _ } :: tl ->
        Hashtbl.replace driver_proc lhs ck_rst; scan_lhs ck_rst tl
    | BCallStmt { func = "@mem_write"; args = (BVar arr) :: _ } :: tl
    | BCallStmt { func = "@slice_write"; args = (BVar arr) :: _ } :: tl
    | BCallStmt { func = "@part_sel_write_up"; args = (BVar arr) :: _ } :: tl
    | BCallStmt { func = "@part_sel_write_down"; args = (BVar arr) :: _ } :: tl ->
        (* @mem_write / @slice_write / @part_sel_write_* inside a
           BSequential body means [arr] is sequentially driven — pre-
           pass needs to create it as a register, not a wire,
           otherwise the resulting translation forms a combinational
           loop. *)
        Hashtbl.replace driver_proc arr ck_rst; scan_lhs ck_rst tl
    | BIf { then_stmts; else_stmts; _ } :: tl ->
        scan_lhs ck_rst then_stmts; scan_lhs ck_rst else_stmts; scan_lhs ck_rst tl
    | BCase { cases; default; _ } :: tl ->
        List.iter (fun (_, s) -> scan_lhs ck_rst s) cases;
        scan_lhs ck_rst default; scan_lhs ck_rst tl
    | BBlock s :: tl -> scan_lhs ck_rst s; scan_lhs ck_rst tl
    | _ :: tl -> scan_lhs ck_rst tl in
  (* Combinational FIRST, then sequential, so a signal driven in BOTH (a
     defect — resolved elsewhere by dropping the comb driver) is classified as
     a REGISTER, not a wire.  A seq @part_sel/@mem RMW reads `BVar lhs` as its
     Q feedback; if the signal were mis-created as a wire (comb classification
     winning) that feedback becomes a genuine combinational loop.  For a
     conflict-free design (eth-arp) each signal is in one process, so order is
     irrelevant — no behavioural change. *)
  List.iter (function
    | BCombinational { body; _ } -> scan_lhs None body
    | BSequential _ -> ()) bmod.processes;
  List.iter (function
    | BSequential { clock; reset; reset_async; reset_edge; body; _ } ->
        let async_rst = if reset_async then reset else None in
        let rst_falling = reset_async && reset_edge = Some `Neg in
        scan_lhs (Some (clock, async_rst, rst_falling)) body
    | BCombinational _ -> ()) bmod.processes;

  (* Per-register ASYNC-RESET VALUE.  iflift renders `if (rst) q<=V; else q<=E`
     as `q := (rst-cond) ? V : E`, so V (the then-branch) is the reset value.
     Downstream picks the FF primitive from Reg_spec.reset_to: value 0 -> FDCE
     (async clear), value 1 -> FDPE (async preset).  Without capturing V a
     reset-to-NONZERO register (e.g. the PCS pma_reset_pipe <= 4'b1111 reset
     stretch) defaults reset_to to 0 and mis-maps to FDCE, so the FF clears to
     0 on reset instead of presetting to 1 — the reset pulse never asserts. *)
  let rec bexpr_mentions nm = function
    | Behavioral_ir.BVar v -> v = nm
    | BConst _ -> false
    | BBinOp { lhs; rhs; _ } -> bexpr_mentions nm lhs || bexpr_mentions nm rhs
    | BUnOp { operand; _ } -> bexpr_mentions nm operand
    | BCond { condition; then_val; else_val } ->
        bexpr_mentions nm condition || bexpr_mentions nm then_val
        || bexpr_mentions nm else_val
    | BSlice { signal; _ } -> bexpr_mentions nm signal
    | BSelect { array; index } -> bexpr_mentions nm array || bexpr_mentions nm index
    | BConcat es -> List.exists (bexpr_mentions nm) es
    (* `!rst_n` reaches iflifted ternaries wrapped as
       BUnOp(BNot, BCall("or_reduce",[rst_n])) — without descending into
       BCall args the reset-value capture missed every such register and
       async-reset-to-NONZERO regs mis-mapped to FDCE (arp_ctrl
       framing_sel<=1 / core_lsu_be<=8'hFF stuck at 0 on silicon). *)
    | BCall { args; _ } -> List.exists (bexpr_mentions nm) args
    | BReplicate { value; _ } -> bexpr_mentions nm value
    | _ -> false in
  let reset_values : (string, Behavioral_ir.bexpr) Hashtbl.t = Hashtbl.create 16 in
  (* registers assigned ONLY in the reset branch never get an iflift
     ternary — they stay as plain BAssigns inside the `if (!rst_n)` BIf.
     Harvest that branch directly (polarity-aware) or those regs default
     to reset_to=0 and mis-map to FDCE (arp_ctrl framing_sel<=1'b1 /
     core_lsu_be<=8'hFF held 0 forever on silicon). *)
  let rec strip_wrap = function
    | Behavioral_ir.BCall { func = ("or_reduce" | "and_reduce"); args = [x] } ->
        strip_wrap x
    | BUnOp { op = (BRedOr | BRedAnd); operand; _ } -> strip_wrap operand
    | e -> e in
  let rec harvest stmts =
    List.iter (function
      | Behavioral_ir.BAssign { lhs; rhs } ->
          if not (Hashtbl.mem reset_values lhs) then
            Hashtbl.replace reset_values lhs rhs
      | BIf { then_stmts; else_stmts; _ } ->
          harvest then_stmts; harvest else_stmts
      | BCase { cases; default; _ } ->
          List.iter (fun (_, s) -> harvest s) cases; harvest default
      | BBlock ss -> harvest ss
      | _ -> ()) stmts in
  let rec scan_rst ~falling rst stmts =
    List.iter (function
      | Behavioral_ir.BAssign { lhs; rhs = BCond { condition; then_val; _ } }
        when bexpr_mentions rst condition ->
          Hashtbl.replace reset_values lhs then_val
      | BIf { condition; then_stmts; else_stmts } ->
          (match strip_wrap condition with
           | BUnOp { op = BNot; operand; _ }
             when strip_wrap operand = Behavioral_ir.BVar rst && falling ->
               harvest then_stmts; scan_rst ~falling rst else_stmts
           | BVar v when v = rst && not falling ->
               harvest then_stmts; scan_rst ~falling rst else_stmts
           | BVar v when v = rst && falling ->
               harvest else_stmts; scan_rst ~falling rst then_stmts
           | BUnOp { op = BNot; operand; _ }
             when strip_wrap operand = Behavioral_ir.BVar rst && not falling ->
               harvest else_stmts; scan_rst ~falling rst then_stmts
           | _ ->
               scan_rst ~falling rst then_stmts;
               scan_rst ~falling rst else_stmts)
      | BBlock ss -> scan_rst ~falling rst ss
      | _ -> ()) stmts in
  List.iter (function
    | BSequential { reset = Some rst; reset_async = true; reset_edge; body; _ } ->
        scan_rst ~falling:(reset_edge = Some `Neg) rst body
    | _ -> ())
    bmod.processes;

  (* Black-box instance OUTPUT wires are pre-created HERE, before the
     register pre-pass, so a register clocked by an internal box output
     (a BUFG/MMCM driving `cpu_clk` used by top-level FFs) binds its
     Reg_spec clock to the actual box-output wire instead of an unbound
     free input (which flatten later promotes to a driverless top port,
     tripping Vivado NSTD-1/UCIO-1 and orphaning the clock tree).
     [box_out_nets] records these net names so the register pre-pass
     skips them (they are driven by the Inst, not by a process). *)
  let box_out_nets = ref [] in
  let inst_infos =
    if not emit_instances then []
    else begin
      let module_inputs =
        List.filter_map (fun (s : Behavioral_ir.bsignal) ->
          if s.direction = `Input then Some s.name else None) bmod.signals in
      let sig_decl_width nm =
        match
          List.find_opt (fun (s : Behavioral_ir.bsignal) -> s.name = nm) bmod.signals
        with
        | Some s -> width_of_btype s.stype
        | None -> 1 in
      let base_is_box_output v =
        not (Hashtbl.mem driver_proc v) && not (List.mem v module_inputs) in
      (* Classify an instance PORT.  Authoritative source: the primitive's
         declared port direction (from library_cells / Vivado unisim VHDL) —
         essential because the net heuristic below misclassifies an instance
         INPUT that reads a net driven by ANOTHER instance (e.g. MMCM.CLKIN1
         reading sysclk from a BUFG) as an output, which multi-drives the net
         and disconnects the clock tree.  Falls back to the net-usage heuristic
         only when the direction is unknown. *)
      let heuristic_output = function
        | BVar v -> base_is_box_output v
        (* A primitive output wired through a bus-slice — e.g. a multi-bank
           RAM's `.DOA(douta_w[b*64 +: 32])` or a GT output into a wide bus —
           is still an OUTPUT.  Without this the box loses its outputs and
           bir_to_aig prunes it as dead (dropping GT/RAMB/IO). *)
        | BSlice { signal = BVar v; _ } -> base_is_box_output v
        | _ -> false in
      let port_is_output mn port e =
        match port_dir mn port with
        | Some `Output -> true
        | Some `Input -> false
        | None -> heuristic_output e in
      (* sliced box-output drivers, grouped by the bus var they drive *)
      let partial : (string, (int * int * Signal.t) list) Hashtbl.t =
        Hashtbl.create 16 in
      let infos = List.map (fun (i : Behavioral_ir.binstance) ->
        let outs, ins =
          List.partition (fun (port, e) -> port_is_output i.module_name port e)
            i.port_connections in
        let out_wires =
          List.filter_map (fun (port, e) ->
            match e with
            | BVar v ->
                let wire = Signal.wire (sig_decl_width v) in
                (* Name the box-output wire with its net name so a register
                   clocked by it (e.g. a top-level FF on a BUFG's O net
                   `cpu_clk`) reports rb_clock = "cpu_clk" downstream, letting
                   fpga_map bridge the FF clock to the on-chip driver instead
                   of minting a driverless clock pad. *)
                let _ = Signal.(--) wire v in
                ctx.signals <- (v, wire) :: ctx.signals;
                box_out_nets := v :: !box_out_nets;
                Some (port, wire)
            | BSlice { signal = BVar base; msb; lsb } ->
                let wire = Signal.wire (msb - lsb + 1) in
                let prev = try Hashtbl.find partial base with Not_found -> [] in
                Hashtbl.replace partial base ((lsb, msb, wire) :: prev);
                box_out_nets := base :: !box_out_nets;
                Some (port, wire)
            | BSelect { array = BVar base; index = BConst { value = k; _ } } ->
                (* A box output driving one constant-indexed ARRAY element,
                   `arr[k]` — how each master/device drives its lane of a
                   multi-port bus array (host_addr[0] from ibex_top, host_addr[1]
                   from dm_top) or a CSR array element (mhpmcounter[0],
                   tmatch_value_q[k]).  Element k occupies bits
                   [k*elem_w +: elem_w] (matches BSelect read lowering), so this
                   is just a slice driver: route it through the same [partial]
                   bus-assembly path that reassembles the array from its lanes. *)
                let k = Z.to_int k in
                let ew =
                  match List.find_opt
                          (fun (s : Behavioral_ir.bsignal) -> s.name = base)
                          bmod.signals with
                  | Some { stype = BArray { element; _ }; _ } -> width_of_btype element
                  | _ -> 1 (* packed bit-select arr[k]: single bit *) in
                let lsb = k * ew in
                let msb = lsb + ew - 1 in
                let wire = Signal.wire ew in
                let prev = try Hashtbl.find partial base with Not_found -> [] in
                Hashtbl.replace partial base ((lsb, msb, wire) :: prev);
                box_out_nets := base :: !box_out_nets;
                Some (port, wire)
            | BConcat parts ->
                (* A struct-typed instance OUTPUT scalarized into per-field
                   nets: `{f1,f2,f3}` is MSB-first (f1 = high bits).  How a
                   struct-typed output like ibex id_stage's exc_cause_o
                   (`{exc_cause$irq_int, exc_cause$irq_ext, exc_cause$lower_cause}`)
                   or dmi_jtag's dmi_req_o arrives.  Silently dropping it lost
                   the instance's driver — the sink net defaulted to 0 and the
                   port was misnamed after the net — which is what zeroed
                   `mcause`.  Instead create one output wire spanning all fields
                   and drive each field net from its slice, so downstream reads
                   of the fields resolve to the box output. *)
                let elem_width = function
                  | BVar v -> sig_decl_width v
                  | BSlice { msb; lsb; _ } -> msb - lsb + 1
                  | BConst { width; _ } -> width
                  | e -> failwith (Printf.sprintf
                      "create_circuit: instance %S (module %S) output port %S: \
                       concat element %s is not a net/slice/const\nJSON: %s"
                      i.inst_name i.module_name port
                      (Behavioral_ir.string_of_bexpr e)
                      (Behavioral_ir.json_string_of_bexpr e)) in
                let total = List.fold_left (fun a e -> a + elem_width e) 0 parts in
                let owire = Signal.wire total in
                let hi = ref (total - 1) in
                List.iter (fun e ->
                  let w = elem_width e in
                  let lo = !hi - w + 1 in
                  (match e with
                   | BVar v ->
                       let named = Signal.(Signal.select owire !hi lo -- v) in
                       ctx.signals <- (v, named) :: ctx.signals;
                       box_out_nets := v :: !box_out_nets
                   | BSlice { signal = BVar base; msb; lsb } ->
                       let slice = Signal.select owire !hi lo in
                       let prev = try Hashtbl.find partial base with Not_found -> [] in
                       Hashtbl.replace partial base ((lsb, msb, slice) :: prev);
                       box_out_nets := base :: !box_out_nets
                   | BConst _ -> ()  (* constant padding bits: nothing to drive *)
                   | _ -> ());
                  hi := lo - 1) parts;
                Some (port, owire)
            | _ ->
                (* An instance OUTPUT port must connect to a net (BVar), a
                   bus-slice of one (BSlice), or a scalarized-struct concat
                   (BConcat, handled above).  Any other shape is not handled;
                   silently dropping it loses the instance's driver and yields
                   a silently-wrong netlist.  Bomb loudly instead. *)
                let msg = Printf.sprintf
                  "create_circuit: instance %S (module %S) output port %S \
                   connects to an unsupported expression shape %s — expected \
                   a net, bus-slice, or scalarized-struct concat\nJSON: %s"
                  i.inst_name i.module_name port
                  (Behavioral_ir.string_of_bexpr e)
                  (Behavioral_ir.json_string_of_bexpr e) in
                if Sys.getenv_opt "SVS_SURVEY_UNHANDLED" <> None then
                  (Printf.eprintf "[SURVEY] %s\n" msg; None)
                else failwith msg) outs in
        (i, out_wires, ins)) bmod.instances in
      (* Assemble each bus that several boxes drive slices of into one signal
         (LSB..MSB, gaps tied 0) and register it so processes reading the bus
         resolve to the box outputs. *)
      Hashtbl.iter (fun base drivers ->
        let max_msb = List.fold_left (fun a (_, m, _) -> max a m) 0 drivers in
        let width =
          let d = sig_decl_width base in if d > max_msb then d else max_msb + 1 in
        let sorted = List.sort (fun (l1, _, _) (l2, _, _) -> compare l1 l2) drivers in
        let pieces = ref [] and pos = ref 0 in
        List.iter (fun (lsb, msb, w) ->
          if lsb > !pos then pieces := Signal.zero (lsb - !pos) :: !pieces;
          pieces := w :: !pieces;
          pos := msb + 1) sorted;
        if !pos < width then pieces := Signal.zero (width - !pos) :: !pieces;
        (* !pieces is MSB-first (ascending-lsb inserts prepend the highest last) *)
        ctx.signals <- (base, Signal.concat_msb !pieces) :: ctx.signals) partial;
      infos
    end in

  List.iter (fun (s : Behavioral_ir.bsignal) ->
    (* Record per-element width for BArray signals so [BSelect] can
       slice the flat representation correctly when index is dynamic. *)
    (match s.stype with
     | BArray { element; _ } ->
         Hashtbl.replace ctx.array_elem_w s.name (width_of_btype element)
     | _ -> ());
    if s.direction <> `Input
       && not (List.mem_assoc s.name ctx.variables)
       (* Skip nets driven by a black-box instance output (pre-created
          above as wires in ctx.signals): they must NOT get a competing
          Always.Variable / never-written zero stub, which would shadow
          the box-output wire and re-orphan a clock net like cpu_clk. *)
       && not (List.mem s.name !box_out_nets) then begin
      let w = width_of_btype s.stype in
      (* If this signal is never written by ANY process — outputs left
         dangling by the source RTL (gqa_attention's kv_wr_addr,
         kv_rd_addr), or signals that only appear on the RHS of reads
         in modules-under-construction — skip the Always.Variable
         dance entirely.  Always.Variable.wire's default-on-no-write
         only kicks in when at least one Always.compile runs against
         it, so a wire that never gets a `<--` ends up with empty
         data_in and hardcaml rejects the resulting circuit.  Just
         park a constant zero in ctx.signals; the Output handler
         later reads it via the ctx.signals fallback. *)
      let never_written = not (Hashtbl.mem driver_proc s.name) in
      if never_written then begin
        ctx.signals <- (s.name, Signal.zero w) :: ctx.signals;
        ()
      end else
      (* SSA-version detection: a name like `<base>_<N>` whose `<base>`
         is also a signal in the module is an intermediate produced by
         Behavioral_ssa.module_to_ssa.  These must be COMBINATIONAL
         wires (driven by the same always block as the final reg),
         not separate FFs — otherwise picorv32 reg_pc grows a 5-deep
         FF pipeline (reg_pc_7 → _10 → _29 → _32 → _38 → reg_pc) that
         takes ~5 cycles for the reset value to propagate, and X-bits
         from intermediate stages leak through during the reset
         period.  Detection is by suffix; we keep the original
         `<base>` name as a reg. *)
      let is_ssa_version name =
        match String.rindex_opt name '_' with
        | None -> false
        | Some i ->
            let suffix = String.sub name (i + 1) (String.length name - i - 1) in
            if String.length suffix = 0
            || not (String.for_all (fun c -> c >= '0' && c <= '9') suffix)
            then false
            else
              let base = String.sub name 0 i in
              List.exists (fun (s' : Behavioral_ir.bsignal) -> s'.name = base)
                bmod.signals
      in
      (* Apply BIR initial_value into the Reg_spec at variable
         creation time — this is the pre-pass that runs BEFORE the
         body processing (where [get_or_create_var]'s [with_init]
         normally fires).  Mirror that logic here so the resulting
         Reg signal carries [reg_reset_value], which bir_to_aig and
         downstream FPGA mappers read for FDRE INIT. *)
      (* Apply BIR initial_value via reset_to.  Don't touch the
         [~reset] field — overriding it with gnd makes Hardcaml emit
         a non-empty reg_reset signal, which bir_to_aig then treats
         as a real async reset and mis-maps the FF to FDCE.  Setting
         reset_to alone leaves reg_reset empty (so FDRE is selected)
         while still recording the initial value on the Signal. *)
      let apply_init_pre spec =
        (* Prefer the ASYNC-RESET value (from the reset branch) when present and
           constant: it drives the FDCE-vs-FDPE choice.  Fall back to the BIR
           initial (power-on) value for INIT-only registers. *)
        let reset_const =
          match Hashtbl.find_opt reset_values s.name with
          | Some (BConst { value; width }) -> Some (width, value)
          | _ -> None in
        let chosen =
          match reset_const with
          | Some _ -> reset_const
          | None ->
            (match Hashtbl.find_opt ctx.initial_values s.name with
             | Some (iw, iv) -> Some (iw, iv)
             | None -> None) in
        match chosen with
        | None -> spec
        | Some (init_w, init_v) ->
          let const = signal_of_z ~width:(max w init_w) init_v in
          let const = if Signal.width const = w then const
                      else Signal.uresize const w in
          Reg_spec.override spec ~reset_to:const
      in
      let is_reg, var =
        match Hashtbl.find_opt driver_proc s.name with
        | Some (Some (clock, reset, rst_falling)) when not (is_ssa_version s.name) ->
            (* [resolve_clkrst], not [get_signal]: the clock/reset net may be
               declared after this register (forward reference) — placeholder
               now, connected by [connect_deferred_clkrst] at the end. *)
            let clk = resolve_clkrst clock in
            let spec = match reset with
              | Some rst ->
                  let s = Reg_spec.create ~clock:clk ~reset:(resolve_clkrst rst) () in
                  if rst_falling then Reg_spec.override s ~reset_edge:Falling else s
              | None     -> Reg_spec.create ~clock:clk () in
            let spec = apply_init_pre spec in
            (true, Always.Variable.reg spec ~width:w)
        | _ ->
            (false, Always.Variable.wire ~default:(Signal.zero w)) in
      (* Tag the variable's underlying signal with the BIR name so
         lib_map's [net_for_signal] picks it up instead of falling
         back to a hardcaml-minted `_n_N_`.  Only do this for
         REGISTERS — naming combinational wires can collide when
         hier_synth's phantom-IO promotion brings names from child
         instance connections into the parent's variable list. *)
      if is_reg || Sys.getenv_opt "BIR_NAME_WIRES" <> None then
        (let _ = Signal.(--) (Always.Variable.value var) s.name in ());
      ctx.variables <- (s.name, var) :: ctx.variables
    end) bmod.signals;

  (* Black-box instances (binstances) -> hardcaml Inst.  Opt-in via
     [emit_instances] (the FPGA back end); the ASIC/miter paths handle
     instances elsewhere (hier_synth) and keep the historical drop.
     Self-contained direction inference: a port is an OUTPUT iff its
     connected net is a simple name that no process writes and that isn't
     a module input — so it can only be driven by the box (exactly how
     behavioral_memlower wires a macro: inputs via combinational assigns,
     outputs left undriven).  Output wires are pre-created and registered
     in ctx.signals so reads inside the processes resolve to them; the
     Inst itself is built after the processes run (its inputs need the
     process-computed driver signals). *)
  (* [box_out_nets] / [inst_infos] are built EARLIER (just after the
     driver_proc scan, before the register pre-pass) so that a register
     whose CLOCK is an internal instance output (e.g. a top-level FF
     clocked by a BUFG's O net named `cpu_clk`) can bind its Reg_spec to
     that box-output wire.  See the hoisted block above. *)

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
  (* Coalesce combinational array-element writes into one full-bus BAssign per
     array.  After unroll, `always_comb` blocks that write array elements —
     `for(i) arr[i]=f(i)`, `arr[k]=v`, and crucially GUARDED / DYNAMIC-index
     writes like lowRISC bus.sv's `if (accept) host_gnt_o[host_sel_req]=1` —
     arrive as several @mem_write(arr, idx, data), some nested in BIf.  Lowering
     each @mem_write independently makes every one read the array Variable's
     block-entry value (an empty comb wire) and rewrite a slot, leaving the
     others driverless → Hardcaml aborts ("circuit input signal must have a
     port name").  Instead, walk the whole block collecting each write's
     (guard, index, data) — guard = AND of the enclosing BIf conditions — and
     rebuild each array as one BAssign whose slot k is a last-write-wins
     BCond chain: `if (guard_n && idx_n==k) then data_n else …` down to a
     zero default.  Handles constant AND dynamic indices, conditional writes,
     and full or partial coverage. *)
  let coalesce_comb_mem_writes stmts =
    let is_arr arr = match List.find_opt
      (fun (sg : Behavioral_ir.bsignal) -> sg.name = arr) bmod.signals with
      | Some { stype = BArray { element = BInt _; _ }; _ } -> true
      | _ -> false in
    let bool1 = BInt { width = 1; signed = Unsigned } in
    let order = ref [] in
    (* per array: ordered (guard option, idx, data). *)
    let writes : (string,
                  (Behavioral_ir.bexpr option * Behavioral_ir.bexpr
                   * Behavioral_ir.bexpr) list) Hashtbl.t = Hashtbl.create 8 in
    let and_g g c = match g with
      | None -> Some c
      | Some g0 -> Some (Behavioral_ir.BBinOp { op = BAnd; lhs = g0; rhs = c;
                                                result_type = bool1 }) in
    let rec collect guard = function
      | Behavioral_ir.BBlock b -> List.iter (collect guard) b
      | BCallStmt { func = "@mem_write"; args = [BVar arr; idx; data] }
        when is_arr arr ->
          if not (Hashtbl.mem writes arr) then order := arr :: !order;
          let cur = try Hashtbl.find writes arr with Not_found -> [] in
          Hashtbl.replace writes arr (cur @ [(guard, idx, data)])
      | BIf { condition; then_stmts; else_stmts } ->
          let notc = Behavioral_ir.BUnOp { op = BNot; operand = condition;
                                           result_type = bool1 } in
          List.iter (collect (and_g guard condition)) then_stmts;
          List.iter (collect (and_g guard notc)) else_stmts
      | _ -> () in
    List.iter (collect None) stmts;
    if Hashtbl.length writes = 0 then stmts
    else begin
      let folded = Hashtbl.create 8 in
      let coalesced = List.filter_map (fun arr ->
        match List.find_opt (fun (sg : Behavioral_ir.bsignal) -> sg.name = arr)
                bmod.signals with
        | Some { stype = BArray { size; element = BInt { width = ew; _ } }; _ }
          when size > 0 ->
            Hashtbl.replace folded arr ();
            let pairs = Hashtbl.find writes arr in
            (* A later write's DATA can read the element it is modifying —
               `arr[i][j]=1` lowers to @mem_write(arr, i, arr[i] | (1<<j)),
               whose `arr[i]` is a read-modify-write of the same slot (ibex's
               `mhpmevent[i]='0; mhpmevent[i][i-3]=1'b1`).  That self-read is a
               combinational loop unless it resolves to the slot's value SO FAR
               (the fold accumulator).  Substitute `BSelect(BVar arr, idx)` in a
               write's data with `acc`. *)
            let rec subst_self idx acc e =
              match e with
              | BSelect { array = BVar a; index } when a = arr && index = idx ->
                  acc
              | BSelect { array; index } ->
                  BSelect { array = subst_self idx acc array;
                            index = subst_self idx acc index }
              | BBinOp r ->
                  BBinOp { r with lhs = subst_self idx acc r.lhs;
                                  rhs = subst_self idx acc r.rhs }
              | BUnOp r -> BUnOp { r with operand = subst_self idx acc r.operand }
              | BCond r ->
                  BCond { condition = subst_self idx acc r.condition;
                          then_val = subst_self idx acc r.then_val;
                          else_val = subst_self idx acc r.else_val }
              | BConcat es -> BConcat (List.map (subst_self idx acc) es)
              | BSlice r -> BSlice { r with signal = subst_self idx acc r.signal }
              | BReplicate r ->
                  BReplicate { r with value = subst_self idx acc r.value }
              | BCall r -> BCall { r with args = List.map (subst_self idx acc) r.args }
              | e -> e in
            (* MSB-first: slot size-1 … 0.  Each slot folds its writes in
               source order (later write = outer = higher priority).  A write
               with a CONSTANT index only affects its own slot — fold it in
               directly for the matching slot and SKIP it for others (no BCond,
               no accumulator duplication).  This is essential: keeping a BCond
               (and re-substituting `acc` into each self-referencing write's
               then_val) for every write in every slot is O(2^writes) in memory
               (ibex's mhpmevent has ~30 self-referencing writes → 25 GB). *)
            let const_idx = function
              | Behavioral_ir.BConst { value; _ } -> Some (Z.to_int value)
              | _ -> None in
            let parts = List.init size (fun j ->
              let k = size - 1 - j in
              let kc = Behavioral_ir.BConst { value = Z.of_int k; width = 32 } in
              let init = Behavioral_ir.BConst { value = Z.zero; width = ew } in
              List.fold_left (fun acc (guard, idx, data) ->
                match const_idx idx with
                | Some ci when ci <> k -> acc   (* different slot — no effect *)
                | Some _ ->                       (* this slot; idx == k *)
                    let d = BSlice { signal = subst_self idx acc data;
                                     msb = ew - 1; lsb = 0 } in
                    (match guard with
                     | None -> d
                     | Some g -> Behavioral_ir.BCond { condition = g;
                                   then_val = d; else_val = acc })
                | None ->                         (* dynamic index *)
                    let eq = Behavioral_ir.BBinOp { op = BEq; lhs = idx; rhs = kc;
                                                    result_type = bool1 } in
                    let cond = match guard with
                      | None -> eq
                      | Some g -> Behavioral_ir.BBinOp { op = BAnd; lhs = g;
                                    rhs = eq; result_type = bool1 } in
                    Behavioral_ir.BCond { condition = cond;
                      then_val = BSlice { signal = subst_self idx acc data;
                                          msb = ew - 1; lsb = 0 };
                      else_val = acc }) init pairs) in
            Some (Behavioral_ir.BAssign { lhs = arr; rhs = BConcat parts })
        | _ -> None) (List.rev !order) in
      (* Strip the folded arrays' @mem_writes from the body (wherever nested),
         keeping BIf structure that still holds non-array statements. *)
      let rec strip ss = List.filter_map (fun s -> match s with
        | Behavioral_ir.BCallStmt { func = "@mem_write"; args = [BVar arr; _; _] }
          when Hashtbl.mem folded arr -> None
        | BBlock b -> (match strip b with [] -> None | b' -> Some (BBlock b'))
        | BIf { condition; then_stmts; else_stmts } ->
            let t = strip then_stmts and e = strip else_stmts in
            if t = [] && e = [] then None
            else Some (BIf { condition; then_stmts = t; else_stmts = e })
        | other -> Some other) ss in
      strip stmts @ coalesced
    end
  in
  (* A signal driven in BOTH a combinational and a sequential process is a
     defect (a _q register combinationally re-driven) that Hardcaml rejects as
     "assign wire multiple times".  Resolve by letting the REGISTER win: strip
     the comb assignments to any seq-driven name.  This only fires on designs
     that would otherwise crash (eth-arp has no such conflict, so it is a
     no-op there); it makes an unbuildable module build with the register-file
     path inert — enough for a minimal debug module (dmactive/dmstatus/halt
     don't use these arrays; abstract-data/progbuf/sysbus do). *)
  let seq_driven =
    let acc = Hashtbl.create 64 in
    let rec w = function
      | BAssign { lhs; _ } -> Hashtbl.replace acc lhs ()
      | BCallStmt { args = (BVar n) :: _; _ } -> Hashtbl.replace acc n ()
      | BIf { then_stmts; else_stmts; _ } -> List.iter w then_stmts; List.iter w else_stmts
      | BCase { cases; default; _ } ->
          List.iter (fun (_, s) -> List.iter w s) cases; List.iter w default
      | BBlock s -> List.iter w s
      | _ -> () in
    List.iter (function BSequential { body; _ } -> List.iter w body | _ -> ())
      bmod.processes;
    acc in
  let dropped = ref [] in
  let strip_seq_driven body =
    let rec f = function
      | BAssign { lhs; _ } when Hashtbl.mem seq_driven lhs ->
          dropped := lhs :: !dropped; None
      | BCallStmt { args = (BVar n) :: _; _ } when Hashtbl.mem seq_driven n ->
          dropped := n :: !dropped; None
      | BIf r -> Some (BIf { r with
          then_stmts = List.filter_map f r.then_stmts;
          else_stmts = List.filter_map f r.else_stmts })
      | BCase r -> Some (BCase { r with
          cases = List.map (fun (g, s) -> (g, List.filter_map f s)) r.cases;
          default = List.filter_map f r.default })
      | BBlock s -> Some (BBlock (List.filter_map f s))
      | s -> Some s in
    List.filter_map f body in
  let merged_combs =
    let bodies = List.filter_map (function
      | BCombinational { body; _ } -> Some body
      | _ -> None) bmod.processes in
    match bodies with
    | [] -> None
    | _ ->
      let body = coalesce_comb_mem_writes (List.concat bodies) in
      let body = strip_seq_driven body in
      (if Sys.getenv_opt "SELFREF_DEBUG" <> None then begin
        let rec w = function
          | BAssign { lhs; rhs } ->
              let s = try Behavioral_ir.string_of_bexpr rhs with _ -> "" in
              (* crude self-ref: lhs token appears in its own RHS *)
              let re = Str.regexp_string lhs in
              (try ignore (Str.search_forward re s 0);
                 Printf.eprintf "[selfref] %s := (self-referential, %d chars)\n%!"
                   lhs (String.length s)
               with Not_found -> ())
          | BIf { then_stmts; else_stmts; _ } -> List.iter w then_stmts; List.iter w else_stmts
          | BCase { cases; default; _ } ->
              List.iter (fun (_, s) -> List.iter w s) cases; List.iter w default
          | BBlock s -> List.iter w s
          | _ -> () in
        List.iter w body
      end);
      (if !dropped <> [] then
         Printf.eprintf
           "[seq-wins] %s: dropped comb driver(s) for seq-driven signal(s) %s \
            (register wins; would otherwise be a multiple-driver error)\n%!"
           bmod.name
           (String.concat "," (List.sort_uniq compare !dropped)));
      Some (BCombinational {
        name = "merged_comb";
        sensitivity = [BAny];
        body })
  in
  (* Group BSequential processes by (clock, clock_edge, reset,
     reset_edge, reset_async) and merge each group's bodies — same
     logic as the BCombinational merge but per-FF-domain.  The
     motivating case is multi-slot @mem_write to the same array,
     where the Verible converter emits one BSequential per slot;
     each gets translated to a full-bus var<--new_val and Always.
     compile would otherwise drive the underlying wire multiple
     times. *)
  let seq_key (s : Behavioral_ir.bprocess) =
    match s with
    | BSequential { clock; clock_edge; reset; reset_edge; reset_async; _ } ->
        Some (clock, clock_edge, reset, reset_edge, reset_async)
    | _ -> None in
  let seq_groups = Hashtbl.create 4 in
  List.iter (fun p ->
    match p with
    | BSequential { body; blocking_vars; _ } ->
        (match seq_key p with
         | Some k ->
             let (prev_body, prev_bv) =
               try Hashtbl.find seq_groups k with Not_found -> ([], []) in
             Hashtbl.replace seq_groups k
               (prev_body @ body,
                List.sort_uniq compare (prev_bv @ blocking_vars))
         | None -> ())
    | _ -> ()
  ) bmod.processes;
  let merged_seqs =
    Hashtbl.fold (fun (clock, clock_edge, reset, reset_edge, reset_async)
                     (body, blocking_vars) acc ->
      BSequential {
        name = "merged_seq_" ^ clock;
        clock; clock_edge; reset; reset_edge; reset_async;
        body;
        blocking_vars;
      } :: acc
    ) seq_groups [] in
  let other_processes = List.filter (function
    | BCombinational _ | BSequential _ -> false
    | _ -> true) bmod.processes in
  let processes_to_run =
    (match merged_combs with Some p -> [p] | None -> [])
    @ merged_seqs
    @ other_processes in

  (if Sys.getenv_opt "DUP_LHS" <> None then begin
    let names_of body =
      let acc = ref [] in
      let rec w = function
        | BAssign { lhs; _ } -> acc := lhs :: !acc
        | BCallStmt { args = (BVar n) :: _; _ } -> acc := n :: !acc
        | BIf { then_stmts; else_stmts; _ } -> List.iter w then_stmts; List.iter w else_stmts
        | BCase { cases; default; _ } ->
            List.iter (fun (_, s) -> List.iter w s) cases; List.iter w default
        | BBlock s -> List.iter w s
        | _ -> () in
      List.iter w body; List.sort_uniq compare !acc in
    let comb_names = match merged_combs with
      | Some (BCombinational { body; _ }) -> names_of body | _ -> [] in
    let seq_names = List.concat_map (function
      | BSequential { body; _ } -> names_of body | _ -> []) merged_seqs in
    let both = List.filter (fun n -> List.mem n seq_names) comb_names in
    Printf.eprintf "[DUP_LHS] %s: comb-driven=%d seq-driven=%d comb∩seq=[%s]\n%!"
      bmod.name (List.length comb_names) (List.length seq_names)
      (String.concat "," both);
    (if both <> [] && Sys.getenv_opt "DUP_LHS_STMTS" <> None then begin
      let show n body =
        let rec w = function
          | BAssign { lhs; rhs } when lhs = n ->
              Printf.eprintf "    COMB %s := %s\n%!" lhs
                (try Behavioral_ir.string_of_bexpr rhs with _ -> "?")
          | BCallStmt { func; args = (BVar a) :: _ } when a = n ->
              Printf.eprintf "    COMB %s(%s, ...)\n%!" func a
          | BIf { then_stmts; else_stmts; _ } -> List.iter w then_stmts; List.iter w else_stmts
          | BCase { cases; default; _ } ->
              List.iter (fun (_, s) -> List.iter w s) cases; List.iter w default
          | BBlock s -> List.iter w s
          | _ -> () in
        List.iter w body in
      match merged_combs with
      | Some (BCombinational { body; _ }) ->
          List.iter (fun n -> show n body) both
      | _ -> ()
    end)
  end);
  let _always_blocks =
    List.map (process_to_always ctx) processes_to_run in
  let _ = _always_blocks in    (* compiled side-effects are in ctx *)

  (* Every variable and process now exists, so every clock/reset net's real
     signal is resolvable — connect the forward-reference placeholders that the
     register pre-pass created for nets declared after their FFs. *)
  connect_deferred_clkrst ();

  (* Build each opt-in instance now that the processes have computed its
     input drivers; drive the pre-created output wires from the Inst. *)
  List.iter (fun ((i : Behavioral_ir.binstance), out_wires, ins) ->
    if out_wires <> [] then begin
      let inputs = List.map (fun (port, e) -> port, expr_to_signal ctx e) ins in
      let outputs = List.map (fun (port, w) -> port, Signal.width w) out_wires in
      let int_params =
        List.map (fun (n, v) ->
          Parameter.create ~name:n ~value:(Parameter.Value.Int v)) i.param_values in
      (* string params: an all-0/1 value is a bit-vector (RAMB INIT_xx),
         anything else a Verilog string (RAM_MODE="TDP", …). *)
      let str_params =
        List.map (fun (n, s) ->
          let is_bits =
            String.length s > 0 && String.for_all (fun c -> c = '0' || c = '1') s in
          let value =
            if is_bits then
              Parameter.Value.Std_logic_vector (Logic.Std_logic_vector.of_string s)
            else Parameter.Value.String s in
          Parameter.create ~name:n ~value) i.param_strs in
      let parameters = int_params @ str_params in
      (* Hardcaml instance/module names may only contain alphanumerics or
         `_ $ . [ ]`; flattened Xilinx-IP hierarchy names carry '/' path
         separators (e.g. pcs_pma_block_i/transceiver_inst/.../gtxe2_i) — map
         them to '_' so Instantiation.create accepts them. *)
      let hc_name s = String.map (fun c -> if c = '/' then '_' else c) s in
      let omap =
        Instantiation.create () ~name:(hc_name i.module_name)
          ~instance:(hc_name i.inst_name)
          ~parameters ~inputs ~outputs in
      List.iter (fun (port, ow) ->
        let osig = Base.Map.find_exn omap port in
        Signal.(ow <== osig)) out_wires
    end) inst_infos;

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

  (* Retain instance-boundary outputs that no declared output reaches — the
     clk_p→IBUFDS→BUFG→MMCM clock tree drives clock nets only, so Hardcaml
     would prune those buffers.  Expose each as a `__keep_<net>` output so the
     box survives; bir_to_nextpnr_json strips `__keep_` ports from the emitted
     netlist so they never become IO pads.  Legacy drop-the-buffers behaviour
     is restored by defining SVS_LEGACY_CLOCKBUF_DROP. *)
  let outputs =
    if Sys.getenv_opt "SVS_LEGACY_CLOCKBUF_DROP" <> None then outputs
    else begin
      (* Every flip-flop's clock net is reachable by construction; flag it so
         the clock tree that drives it (IBUFDS→BUFG→MMCM→BUFG) survives
         Hardcaml's data-output-only pruning.  Retain ONLY the FF-clock nets
         (which are box outputs — a BUFG.O): retaining intermediate box outputs
         like the IBUFDS→BUFG link would add an output-buffer LUT that splits
         that net. *)
      let clock_names =
        List.fold_left (fun acc p -> match p with
          | BSequential { clock; _ } -> clock :: acc
          | _ -> acc) [] bmod.processes in
      (* FF ASYNC-RESET nets are box outputs just like clocks: e.g. sgmii_soc's
         `always_ff @(posedge userclk2 or negedge mmcm_locked)` takes the PCS
         box output `mmcm_locked` as its reset.  Such a net feeds only a reset
         (no data output), so Hardcaml's data-output-only DCE prunes the box
         output driving it → the FF reset dangles (ERC: undriven mmcm_locked).
         Retain them too. *)
      let reset_names =
        List.fold_left (fun acc p -> match p with
          | BSequential { reset = Some r; _ } -> r :: acc
          | _ -> acc) [] bmod.processes in
      (* Manually-instantiated flip-flop primitives (Vivado-flattened netlists
         carry these as explicit FD/FDRE/FDPE/… cells) must pass through gate_map
         UNCHANGED — like the clock-tree boxes above.  Retain each FF cell's Q
         output net so Hardcaml's data-output-only DCE can't prune a const-clock
         FF whose output feeds only other resets (e.g. the PCS RX-recclk
         reset synchronisers `SYNC_ASYNC_RESET_RECCLK`, INIT=1, .C(<const0>),
         .PRE(reset)).  Dropping those inverted the RX reset and killed sync. *)
      let is_ff_prim = function
        | "FD" | "FD_1" | "FDE" | "FDE_1" | "FDRE" | "FDSE" | "FDCE" | "FDPE"
        | "FDC" | "FDC_1" | "FDP" | "FDP_1" | "FDRSE" -> true | _ -> false in
      let ff_out_nets =
        List.concat_map (fun (i : Behavioral_ir.binstance) ->
          if is_ff_prim i.module_name then
            List.filter_map (fun (port, e) ->
              (* retain Q outputs AND async control-pin nets (R/S/CLR/PRE) of
                 manually-instantiated FFs — a box output feeding only these
                 would otherwise be DCE-pruned. *)
              match port with
              | "Q" | "R" | "S" | "CLR" | "PRE" ->
                  (match e with BVar n -> Some n | _ -> None)
              | _ -> None) i.port_connections
          else []) bmod.instances in
      let declared name =
        List.exists (fun (s : Behavioral_ir.bsignal) ->
          s.name = name && s.direction = `Output) bmod.signals in
      let seen = Hashtbl.create 16 in
      let extra = List.filter_map (fun name ->
        if Hashtbl.mem seen name || declared name
           || not (List.mem name clock_names || List.mem name ff_out_nets
                   || List.mem name reset_names) then None
        else begin
          Hashtbl.add seen name ();
          match List.assoc_opt name ctx.signals with
          | Some sig_ -> Some (Signal.output ("__keep_" ^ name) sig_)
          | None -> None
        end) !box_out_nets in
      outputs @ extra
    end in

  let config = { Circuit.Config.default with detect_combinational_loops = detect_loops } in
  Circuit.create_exn ~config ~name:bmod.name outputs

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
