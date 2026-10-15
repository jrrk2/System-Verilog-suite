(* BMul -> DSP48E1 inference for the FPGA flow.
 *
 * Every `a * b` in a module's processes whose operands fit one DSP48E1
 * multiplier (signed 25 x 18, i.e. unsigned 24 x 17) is replaced by an
 * instance of the primitive in multiply-only configuration (all pipeline
 * registers off, OPMODE = 0000101 so P = A x B), with the product read back
 * through a slice of the 48-bit P port at the expression's context width.
 * The operands are widened to the A/B port widths with zero- or sign-
 * extension according to Verilog's rule: the product is signed only when
 * BOTH operands are signed.
 *
 * What is NOT handled yet (left as the array multiplier the AIG builds):
 *   - operands wider than 25 x 18 (yosys splits these across several DSPs)
 *   - context widths above 48 bits
 *   - operands whose signedness cannot be read off the BIR (calls, array
 *     reads) -- treating those as unsigned could be wrong, so they are skipped
 *   - register absorption (AREG/BREG/MREG/PREG) and the P += A*B accumulate
 *     form; both come out as the DSP plus external FDRE/CARRY4 logic, which
 *     is correct but not the densest packing.
 *
 * The Z3 oracle gets a matching behavioural model from
 * [Xil_prim_models.synth] ("DSP48E1"), so a mapped netlist carrying these
 * instances still miters against the source. *)

open Behavioral_ir

let dsp_in n w = { port_name = n; port_direction = `Input; port_width = w }
let dsp_out n w = { port_name = n; port_direction = `Output; port_width = w }

(* Full DSP48E1 interface (UG479), so build_port_dir never has to consult the
   unisim VHDL for the primitive. *)
let dsp48e1_ports : library_port list =
  [ dsp_in "A" 30; dsp_in "B" 18; dsp_in "C" 48; dsp_in "D" 25
  ; dsp_in "ACIN" 30; dsp_in "BCIN" 18; dsp_in "PCIN" 48
  ; dsp_in "ALUMODE" 4; dsp_in "CARRYIN" 1; dsp_in "CARRYINSEL" 3
  ; dsp_in "INMODE" 5; dsp_in "OPMODE" 7
  ; dsp_in "CARRYCASCIN" 1; dsp_in "MULTSIGNIN" 1; dsp_in "CLK" 1 ]
  @ List.map (fun n -> dsp_in n 1)
      [ "CEA1"; "CEA2"; "CEAD"; "CEALUMODE"; "CEB1"; "CEB2"; "CEC"; "CECARRYIN"
      ; "CECTRL"; "CED"; "CEINMODE"; "CEM"; "CEP"
      ; "RSTA"; "RSTALLCARRYIN"; "RSTALUMODE"; "RSTB"; "RSTC"; "RSTCTRL"
      ; "RSTD"; "RSTINMODE"; "RSTM"; "RSTP" ]
  @ [ dsp_out "P" 48; dsp_out "ACOUT" 30; dsp_out "BCOUT" 18; dsp_out "PCOUT" 48
    ; dsp_out "CARRYOUT" 4; dsp_out "CARRYCASCOUT" 1; dsp_out "MULTSIGNOUT" 1
    ; dsp_out "OVERFLOW" 1; dsp_out "PATTERNBDETECT" 1; dsp_out "PATTERNDETECT" 1
    ; dsp_out "UNDERFLOW" 1 ]

(* Multiply-only configuration: same parameter set yosys's synth_xilinx emits
   for a bare $mul, so nextpnr-xilinx / Vivado see a familiar cell. *)
let int_params =
  List.map (fun n -> (n, 0))
    [ "ACASCREG"; "ADREG"; "ALUMODEREG"; "AREG"; "BCASCREG"; "BREG"; "CARRYINREG"
    ; "CARRYINSELREG"; "CREG"; "DREG"; "INMODEREG"; "MREG"; "OPMODEREG"; "PREG" ]

let str_params =
  [ ("A_INPUT", "DIRECT"); ("B_INPUT", "DIRECT"); ("USE_DPORT", "FALSE")
  ; ("USE_MULT", "MULTIPLY"); ("USE_SIMD", "ONE48") ]

(* OPMODE = 000_01_01: Z = 0, Y:X = M (the multiplier output). *)
let opmode_mult = 0b0000101

let strip_signed = function
  | BCall { func = "@signed"; args = [ x ] } -> x
  | e -> e

let infer_module ~min_width (m : bmodule) : bmodule * int =
  let sig_widths = Behavioral_const.signal_widths_of_module m in
  let sig_signed : (string, signedness) Hashtbl.t = Hashtbl.create 32 in
  List.iter (fun (s : bsignal) ->
    match s.stype with
    | BInt { signed; _ } -> Hashtbl.replace sig_signed s.name signed
    | _ -> ()) m.signals;
  let new_sigs = ref [] and new_assigns = ref [] and new_insts = ref [] in
  let n = ref 0 in
  (* See through the frontend's explicit sign extension
     {{n{x[w-1]}}, x} so the DSP operand stays x's own width (signed). *)
  let peel_sext e =
    match e with
    | BConcat [ BReplicate { value = BSlice { signal = x; msb; lsb }; _ }; x' ]
      when msb = lsb && x = x' ->
        (match Behavioral_const.width_of_expr_with sig_widths x with
         | Some w when w = msb + 1 -> Some x
         | _ -> None)
    | _ -> None in
  let w_of e =
    match peel_sext e with
    | Some x -> Behavioral_const.width_of_expr_with sig_widths x
    | None -> Behavioral_const.width_of_expr_with sig_widths (strip_signed e) in
  (* Signedness we can trust.  None -> skip the multiplier rather than guess. *)
  let sign_of e =
    match e with
    | _ when peel_sext e <> None -> Some Signed
    | BVar nm -> Hashtbl.find_opt sig_signed nm
    | BCall { func = "@signed"; args = [ _ ] } -> Some Signed
    | BConst _ | BSlice _ | BConcat _ | BReplicate _ -> Some Unsigned
    | _ -> None in
  let try_dsp a b result_type =
    match w_of a, w_of b, sign_of a, sign_of b with
    | Some aw, Some bw, Some sa, Some sb
      when aw >= min_width && bw >= min_width && aw > 0 && bw > 0 ->
        let signed = (sa = Signed && sb = Signed) in
        let amax, bmax = if signed then 25, 18 else 24, 17 in
        (* wider operand on the 25/30-bit A side *)
        let a, aw, b, bw = if aw >= bw then a, aw, b, bw else b, bw, a, aw in
        if aw > amax || bw > bmax then None
        else begin
          let rw = match result_type with
            | BInt { width; _ } -> width
            | _ -> aw + bw in
          if rw > 48 || rw <= 0 then None
          else begin
            incr n;
            let base = Printf.sprintf "__dsp%d" !n in
            let a_name = base ^ "_a" and b_name = base ^ "_b" and p_name = base ^ "_p" in
            let mk name w =
              { name; stype = BInt { width = w; signed = Unsigned }
              ; direction = `Internal; initial_value = None; attrs = [] } in
            new_sigs := mk p_name 48 :: mk b_name 18 :: mk a_name 30 :: !new_sigs;
            let ext e from_w to_w =
              let e = match peel_sext e with Some x -> x | None -> strip_signed e in
              if from_w >= to_w then e
              else if signed then Behavioral_const.sign_extend ~from_w ~to_w e
              else BConcat [ bconst_int 0 (to_w - from_w); e ] in
            new_assigns :=
              BAssign { lhs = b_name; rhs = ext b bw 18 }
              :: BAssign { lhs = a_name; rhs = ext a aw 30 }
              :: !new_assigns;
            let k v w = bconst_int v w in
            new_insts :=
              { inst_name = base; module_name = "DSP48E1"
              ; param_values = int_params; param_strs = str_params
              ; port_connections =
                  [ ("A", BVar a_name); ("B", BVar b_name); ("C", k 0 48)
                  ; ("P", BVar p_name)
                  ; ("ACIN", k 0 30); ("BCIN", k 0 18); ("PCIN", k 0 48)
                  ; ("ALUMODE", k 0 4); ("CARRYIN", k 0 1); ("CARRYINSEL", k 0 3)
                  ; ("D", k 0 25); ("INMODE", k 0 5); ("OPMODE", k opmode_mult 7) ] }
              :: !new_insts;
            Some (if rw = 48 then BVar p_name
                  else BSlice { signal = BVar p_name; msb = rw - 1; lsb = 0 })
          end
        end
    | _ -> None in
  let rec rw e =
    match e with
    | BBinOp { op = BMul; lhs; rhs; result_type } ->
        let lhs = rw lhs and rhs = rw rhs in
        (match try_dsp lhs rhs result_type with
         | Some e' -> e'
         | None -> BBinOp { op = BMul; lhs; rhs; result_type })
    | BBinOp r -> BBinOp { r with lhs = rw r.lhs; rhs = rw r.rhs }
    | BUnOp r -> BUnOp { r with operand = rw r.operand }
    | BSelect { array; index } -> BSelect { array = rw array; index = rw index }
    | BSlice r -> BSlice { r with signal = rw r.signal }
    | BConcat l -> BConcat (List.map rw l)
    | BReplicate r -> BReplicate { r with value = rw r.value }
    | BCond r ->
        BCond { condition = rw r.condition; then_val = rw r.then_val
              ; else_val = rw r.else_val }
    | BCall r -> BCall { r with args = List.map rw r.args }
    | BVar _ | BConst _ -> e in
  let rec rw_stmt s =
    match s with
    | BAssign { lhs; rhs } -> BAssign { lhs; rhs = rw rhs }
    | BIf { condition; then_stmts; else_stmts } ->
        BIf { condition = rw condition
            ; then_stmts = List.map rw_stmt then_stmts
            ; else_stmts = List.map rw_stmt else_stmts }
    | BCase { selector; cases; default } ->
        BCase { selector = rw selector
              ; cases = List.map (fun (k, b) -> (rw k, List.map rw_stmt b)) cases
              ; default = List.map rw_stmt default }
    | BWhile { condition; body } ->
        BWhile { condition = rw condition; body = List.map rw_stmt body }
    | BFor { init; condition; update; body } ->
        BFor { init = rw_stmt init; condition = rw condition
             ; update = rw_stmt update; body = List.map rw_stmt body }
    | BBlock l -> BBlock (List.map rw_stmt l)
    | BCallStmt { func; args } -> BCallStmt { func; args = List.map rw args }
    | BReturn e -> BReturn (Option.map rw e) in
  let rw_proc = function
    | BCombinational r -> BCombinational { r with body = List.map rw_stmt r.body }
    | BSequential r -> BSequential { r with body = List.map rw_stmt r.body } in
  let processes = List.map rw_proc m.processes in
  let instances =
    List.map (fun (i : binstance) ->
      { i with port_connections =
                 List.map (fun (p, e) -> (p, rw e)) i.port_connections })
      m.instances in
  if !n = 0 then m, 0
  else begin
    let driver =
      BCombinational { name = "__dsp_infer"; sensitivity = [ BAny ]
                     ; body = List.rev !new_assigns } in
    { m with signals = m.signals @ List.rev !new_sigs
           ; processes = processes @ [ driver ]
           ; instances = instances @ List.rev !new_insts }, !n
  end

let infer_program ?(min_width = 4) (p : bprogram) : bprogram =
  let total = ref 0 in
  let modules =
    List.map (fun (m : bmodule) ->
      let m', k = infer_module ~min_width m in
      if k > 0 then begin
        total := !total + k;
        Printf.eprintf "[dsp_infer] %s: %d multiplier(s) -> DSP48E1\n%!" m.name k
      end;
      m') p.modules in
  let library_cells =
    if !total > 0 && not (List.mem_assoc "DSP48E1" p.library_cells)
    then p.library_cells @ [ ("DSP48E1", dsp48e1_ports) ]
    else p.library_cells in
  { modules; library_cells }
