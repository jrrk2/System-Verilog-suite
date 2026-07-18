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
      let width = 32 in
      let z = Signal.zero width in
      let _ = Signal.(--) z ("__unbound_" ^ name) in
      Printf.eprintf
        "[expr_to_signal] unbound identifier %s — tied to %d-bit zero \
         (upstream BIR is missing a declaration or driver)\n%!"
        name width;
      ctx.signals <- (name, z) :: ctx.signals;
      z

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
             let s_index =
               let iw = Signal.width s_index in
               if iw >= need_w then s_index
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
      if lo >= w then
        let nbits = max 1 (hi - lo + 1) in
        Signal.zero nbits
      else
        let hi = min hi (w - 1) in
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
let thread_body ?(blocking_vars = []) body =
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
    let rec go = function
      | BAssign { lhs; _ } -> if not (List.mem lhs !acc) then acc := lhs :: !acc
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
          out := BCallStmt { func; args = List.map (subst env) args } :: !out
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

(* Convert behavioral process to HardCaml Always block.
   Returns the compiled Always.t so the caller can keep a list
   of all the always-blocks that make up the module. *)
let process_to_always ctx = function
  | BCombinational { body; _ } ->
      let body = thread_body body in
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
      let body = thread_body ~blocking_vars body in
      let body = merge_slice_writes ctx body in
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
  } in

  (* Clock/reset get bound by [process_to_always] when each
     BSequential block names them via its [clock] / [reset] fields.
     Pattern, not name, is the source of truth. *)

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
  List.iter (function
    | BSequential { clock; reset; reset_async; reset_edge; body; _ } ->
        let async_rst = if reset_async then reset else None in
        let rst_falling = reset_async && reset_edge = Some `Neg in
        scan_lhs (Some (clock, async_rst, rst_falling)) body
    | BCombinational { body; _ } -> scan_lhs None body)
    bmod.processes;

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
    | _ -> false in
  let reset_values : (string, Behavioral_ir.bexpr) Hashtbl.t = Hashtbl.create 16 in
  let rec scan_rst rst stmts =
    List.iter (function
      | Behavioral_ir.BAssign { lhs; rhs = BCond { condition; then_val; _ } }
        when bexpr_mentions rst condition ->
          Hashtbl.replace reset_values lhs then_val
      | BIf { then_stmts; else_stmts; _ } ->
          scan_rst rst then_stmts; scan_rst rst else_stmts
      | BBlock ss -> scan_rst rst ss
      | _ -> ()) stmts in
  List.iter (function
    | BSequential { reset = Some rst; reset_async = true; body; _ } ->
        scan_rst rst body
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
            | _ -> None) outs in
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
            let clk = get_signal ctx clock in
            let spec = match reset with
              | Some rst ->
                  let s = Reg_spec.create ~clock:clk ~reset:(get_signal ctx rst) () in
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

  let _always_blocks =
    List.map (process_to_always ctx) processes_to_run in
  let _ = _always_blocks in    (* compiled side-effects are in ctx *)

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
