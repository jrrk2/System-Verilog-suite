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
 * Wider multiplies are split (DSP_SPLIT=0 disables): the product is only
 * needed modulo 2^r, r being its context width narrowed by a demanded-bits
 * scan (a 72-bit `mul_p` read only as `mul_p[31:0]` needs r = 32).  Each
 * operand is cut to r bits -- sign-extended when the product is signed and
 * the operand is shorter -- and split into unsigned 24-bit (A side) and
 * 17-bit (B side) limbs; every limb pair whose weight 2^(24i+17j) is below
 * 2^r becomes one DSP48E1 (pairs with a limb under min_width bits stay a
 * small LUT multiply), and the shifted partial products are summed in
 * fabric.  At most DSP_SPLIT_MAX (default 8) DSPs per multiply; past that
 * the multiplier is left to the AIG.
 *
 * What is NOT handled yet (left as the array multiplier the AIG builds):
 *   - splits needing more than DSP_SPLIT_MAX DSPs (a full 72-bit signed
 *     36 x 36 product)
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
  let n = ref 0 and nsplit = ref 0 in
  (* Demanded bits: one past the highest bit of each signal any read can
     observe.  A read through constant slices (nested too: x[34:0][31:0])
     sees only those bits; any other read, and every port, sees all of it. *)
  let demand : (string, int) Hashtbl.t = Hashtbl.create 64 in
  let need nm k =
    let k = match Hashtbl.find_opt sig_widths nm with Some w -> min w k | None -> k in
    match Hashtbl.find_opt demand nm with
    | Some k0 when k0 >= k -> ()
    | _ -> Hashtbl.replace demand nm k in
  let rec scan e =
    match e with
    | BVar nm -> need nm max_int
    | BSlice _ ->
        (* peel nested constant slices down to a variable *)
        let rec base e top =
          match e with
          | BSlice { signal; msb; lsb } ->
              let top = match top with None -> msb | Some t -> lsb + t in
              base signal (Some top)
          | _ -> e, top in
        (match base e None with
         | BVar nm, Some top -> need nm (top + 1)
         | e', _ -> scan e')
    | BConst _ -> ()
    | BBinOp { lhs; rhs; _ } -> scan lhs; scan rhs
    | BUnOp { operand; _ } -> scan operand
    | BSelect { array; index } -> scan array; scan index
    | BConcat l -> List.iter scan l
    | BReplicate { value; _ } -> scan value
    | BCond { condition; then_val; else_val } ->
        scan condition; scan then_val; scan else_val
    | BCall { args; _ } -> List.iter scan args in
  let rec scan_stmt = function
    | BAssign { rhs; _ } -> scan rhs
    | BIf { condition; then_stmts; else_stmts } ->
        scan condition; List.iter scan_stmt then_stmts; List.iter scan_stmt else_stmts
    | BCase { selector; cases; default } ->
        scan selector;
        List.iter (fun (k, b) -> scan k; List.iter scan_stmt b) cases;
        List.iter scan_stmt default
    | BWhile { condition; body } -> scan condition; List.iter scan_stmt body
    | BFor { init; condition; update; body } ->
        scan_stmt init; scan condition; scan_stmt update; List.iter scan_stmt body
    | BBlock l -> List.iter scan_stmt l
    | BCallStmt { args; _ } -> List.iter scan args
    | BReturn e -> Option.iter scan e in
  List.iter (function
    | BCombinational { body; _ } | BSequential { body; _ } -> List.iter scan_stmt body)
    m.processes;
  List.iter (fun (i : binstance) -> List.iter (fun (_, e) -> scan e) i.port_connections)
    m.instances;
  List.iter (fun (s : bsignal) -> if s.direction <> `Internal then need s.name max_int)
    m.signals;
  let demanded nm = Option.value (Hashtbl.find_opt demand nm) ~default:0 in
  let split_on = Sys.getenv_opt "DSP_SPLIT" <> Some "0" in
  let split_max =
    match Option.bind (Sys.getenv_opt "DSP_SPLIT_MAX") int_of_string_opt with
    | Some k -> k | None -> 8 in
  (* unsigned limb widths; DSP_SPLIT_LIMBS=a,b shrinks them so the self-test
     exercises the split on multiplies small enough for Z3 *)
  let limb_a, limb_b =
    match Option.map (String.split_on_char ',') (Sys.getenv_opt "DSP_SPLIT_LIMBS") with
    | Some [ x; y ] ->
        (match int_of_string_opt x, int_of_string_opt y with
         | Some x, Some y when x > 0 && y > 0 && x <= 24 && y <= 17 -> x, y
         | _ -> 24, 17)
    | _ -> 24, 17 in
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
  let mk name w =
    { name; stype = BInt { width = w; signed = Unsigned }
    ; direction = `Internal; initial_value = None; attrs = [] } in
  let k v w = bconst_int v w in
  (* one multiply-only DSP48E1 on already port-width A (30) / B (18) exprs *)
  let dsp_inst a30 b18 =
    incr n;
    let base = Printf.sprintf "__dsp%d" !n in
    let a_name = base ^ "_a" and b_name = base ^ "_b" and p_name = base ^ "_p" in
    new_sigs := mk p_name 48 :: mk b_name 18 :: mk a_name 30 :: !new_sigs;
    new_assigns :=
      BAssign { lhs = b_name; rhs = b18 } :: BAssign { lhs = a_name; rhs = a30 }
      :: !new_assigns;
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
    p_name in
  let zpad e from_w to_w =
    if from_w >= to_w then e else BConcat [ bconst_int 0 (to_w - from_w); e ] in
  let uint w = BInt { width = w; signed = Unsigned } in
  (* a * b mod 2^r from unsigned limb products (see the header) *)
  let try_split a aw b bw signed r =
    let operand e w =
      let e = match peel_sext e with Some x -> x | None -> strip_signed e in
      if w >= r then (if w = r then e else BSlice { signal = e; msb = r - 1; lsb = 0 }), r
      else if signed then Behavioral_const.sign_extend ~from_w:w ~to_w:r e, r
      else e, w in
    let a, aw = operand a aw and b, bw = operand b bw in
    let limbs w lw = List.init ((w + lw - 1) / lw) (fun i -> i * lw, min lw (w - i * lw)) in
    (* a pair at weight 2^off only contributes its low r - off bits, which
       depend only on that many low bits of each limb *)
    let pairs la lb =
      List.concat_map (fun (oa, wa) ->
        List.filter_map (fun (ob, wb) ->
          let t = r - (oa + ob) in
          if t > 0 then Some (oa, min wa t, ob, min wb t) else None) lb) la in
    let cost ps = List.length (List.filter (fun (_, wa, _, wb) ->
      wa >= min_width && wb >= min_width) ps) in
    (* the wider operand on the 24-bit A side unless the other way needs fewer DSPs *)
    let p1 = pairs (limbs aw limb_a) (limbs bw limb_b)
    and p2 = pairs (limbs bw limb_a) (limbs aw limb_b) in
    let swap = cost p2 < cost p1 in
    let ps = if swap then p2 else p1 in
    let a, b = if swap then b, a else a, b in
    if cost ps > split_max || cost ps = 0 then None
    else begin
      incr nsplit;
      let tag = Printf.sprintf "__dspw%d" !nsplit in
      let xa = tag ^ "_x" and xb = tag ^ "_y" in
      let wa_tot = if swap then bw else aw and wb_tot = if swap then aw else bw in
      new_sigs := mk xb wb_tot :: mk xa wa_tot :: !new_sigs;
      new_assigns := BAssign { lhs = xb; rhs = b } :: BAssign { lhs = xa; rhs = a }
                     :: !new_assigns;
      let limb nm o w = BSlice { signal = BVar nm; msb = o + w - 1; lsb = o } in
      let terms = List.map (fun (oa, wa, ob, wb) ->
        let off = oa + ob in
        let tw = min (wa + wb) (r - off) in
        let prod =
          if wa >= min_width && wb >= min_width then begin
            let p = dsp_inst (zpad (limb xa oa wa) wa 30) (zpad (limb xb ob wb) wb 18) in
            BSlice { signal = BVar p; msb = tw - 1; lsb = 0 }
          end else
            (* tiny pair: tw-bit LUT multiply on limbs cut to tw bits *)
            let cut nm o w = let w' = min w tw in zpad (limb nm o w') w' tw in
            BBinOp { op = BMul; lhs = cut xa oa wa; rhs = cut xb ob wb
                   ; result_type = uint tw } in
        let parts = (if r - off - tw > 0 then [ bconst_int 0 (r - off - tw) ] else [])
                    @ [ prod ] @ (if off > 0 then [ bconst_int 0 off ] else []) in
        match parts with [ x ] -> x | l -> BConcat l) ps in
      match terms with
      | [] -> None
      | t :: rest ->
          Some (List.fold_left (fun acc t ->
            BBinOp { op = BAdd; lhs = acc; rhs = t; result_type = uint r }) t rest)
    end in
  (* x * K folds to shifts and adds in the AIG; a DSP would waste the slice *)
  let is_const e =
    match (match peel_sext e with Some x -> x | None -> strip_signed e) with
    | BConst _ -> true
    | _ -> false in
  let try_dsp ?ctx a b result_type =
    match w_of a, w_of b, sign_of a, sign_of b with
    | Some aw, Some bw, Some sa, Some sb
      when aw >= min_width && bw >= min_width && aw > 0 && bw > 0
           && not (is_const a || is_const b) ->
        let signed = (sa = Signed && sb = Signed) in
        let amax, bmax = if signed then limb_a + 1, limb_b + 1 else limb_a, limb_b in
        (* wider operand on the 25/30-bit A side *)
        let a, aw, b, bw = if aw >= bw then a, aw, b, bw else b, bw, a, aw in
        let rw = match result_type with
          | BInt { width; _ } -> width
          | _ -> aw + bw in
        (* r: the bits anyone reads (ctx), at most the expression's width *)
        let r = match ctx with Some c when c > 0 && c < rw -> c | _ -> rw in
        let fits = aw <= amax && bw <= bmax && rw <= 48 && rw > 0 in
        if fits && r = rw then begin
          let ext e from_w to_w =
            let e = match peel_sext e with Some x -> x | None -> strip_signed e in
            if from_w >= to_w then e
            else if signed then Behavioral_const.sign_extend ~from_w ~to_w e
            else BConcat [ bconst_int 0 (to_w - from_w); e ] in
          let p_name = dsp_inst (ext a aw 30) (ext b bw 18) in
          Some (if rw = 48 then BVar p_name
                else BSlice { signal = BVar p_name; msb = rw - 1; lsb = 0 })
        end
        else if not split_on || rw <= 0 then None
        else
          (match try_split a aw b bw signed r with
           | None -> None
           | Some e -> Some (zpad e r rw))
    | _ -> None in
  let rec rw ?ctx e =
    match e with
    | BBinOp { op = BMul; lhs; rhs; result_type } ->
        let lhs = rw lhs and rhs = rw rhs in
        (match try_dsp ?ctx lhs rhs result_type with
         | Some e' -> e'
         | None -> BBinOp { op = BMul; lhs; rhs; result_type })
    | BSlice ({ signal = BBinOp { op = BMul; _ } as mul; msb; lsb = 0 } as r) ->
        BSlice { r with signal = rw ~ctx:(msb + 1) mul }
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
    | BAssign { lhs; rhs } -> BAssign { lhs; rhs = rw ~ctx:(demanded lhs) rhs }
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
