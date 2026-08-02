(* behavioral_srl_infer.ml
 *
 * Recognise STATIC SHIFT REGISTERS and map each read tap to an SRL16E (depth
 * <= 16) or SRLC32E (depth <= 32) primitive instance.  This matches Vivado's
 * SRL inference and replaces W flip-flops with ONE LUT-based shift register —
 * the correct, high-density FPGA mapping, and critical in the PCS/PMA where
 * reset pipes / RX-data delays / clock-correction paths are shift registers.
 *
 * Pattern (post unroll/inline/blocking_subst, no reset):
 *     always_ff @(posedge CLK)  sig := (CE ? {sig[W-2:0], D} : sig);   (CE optional)
 * with every OTHER read of `sig` being a single-bit static tap  sig[k:k].
 * Each distinct tap k -> an SRL with address A = k (static), Q -> the tapped
 * net.  A depth-N delay taps sig[N-1] (delay N, matching the FF chain).      *)

open Behavioral_ir

let bconst v w = BConst { value = Z.of_int v; width = w }
let c1 = bconst 1 1

let sig_width (m : bmodule) name =
  match List.find_opt (fun (s : bsignal) -> s.name = name) m.signals with
  | Some s -> (match s.stype with BInt { width; _ } -> Some width | BBool -> Some 1 | _ -> None)
  | None -> None

(* Power-on value of a shift register, full width -- `cpllpd_wait` is 96 bits
   and `cpllreset_wait` 128, so this must stay arbitrary-precision. *)
let init_of (m : bmodule) name =
  match List.find_opt (fun (s : bsignal) -> s.name = name) m.signals with
  | Some { initial_value = Some (BConst { value; _ }); _ } -> value
  | _ -> Z.zero

(* Shift core:
 *   shift-UP   {sig[W-2:0], D}   -- D enters bit 0, tap k has delay k+1
 *   shift-DOWN {D, sig[W-1:1]}   -- D enters bit W-1, tap k has delay W-k
 * optionally CE-gated (else = sig).  Returns (ce_opt, d, `Up | `Down).       *)
let and_ce a b = match a, b with
  | None, x | x, None -> x
  | Some ca, Some cb -> Some (BBinOp { op = BAnd; lhs = ca; rhs = cb;
                                       result_type = BInt { width = 1; signed = Unsigned } })

let match_shift ~sg ~w rhs =
  let core = function
    | BConcat [ BSlice { signal = BVar s; msb; lsb = 0 }; d ]
        when s = sg && msb = w - 2 -> Some (d, `Up)
    | BConcat [ d; BSlice { signal = BVar s; msb; lsb = 1 } ]
        when s = sg && msb = w - 1 -> Some (d, `Down)
    | _ -> None in
  match rhs with
  | BCond { condition = ce; then_val; else_val = BVar s } when s = sg ->
      (match core then_val with Some (d, dir) -> Some (Some ce, d, dir) | None -> None)
  | r -> (match core r with Some (d, dir) -> Some (None, d, dir) | None -> None)

(* ---- generic bottom-up expr / stmt rewriters --------------------------- *)
let rec map_expr f e =
  let e' = match e with
    | BBinOp r -> BBinOp { r with lhs = map_expr f r.lhs; rhs = map_expr f r.rhs }
    | BUnOp r -> BUnOp { r with operand = map_expr f r.operand }
    | BSelect r -> BSelect { array = map_expr f r.array; index = map_expr f r.index }
    | BSlice r -> BSlice { r with signal = map_expr f r.signal }
    | BConcat es -> BConcat (List.map (map_expr f) es)
    | BReplicate r -> BReplicate { r with value = map_expr f r.value }
    | BCond r -> BCond { condition = map_expr f r.condition;
                         then_val = map_expr f r.then_val; else_val = map_expr f r.else_val }
    | BCall r -> BCall { r with args = List.map (map_expr f) r.args }
    | x -> x in
  f e'

let rec map_stmt fe s = match s with
  | BAssign r -> BAssign { r with rhs = map_expr fe r.rhs }
  | BIf r -> BIf { condition = map_expr fe r.condition;
                   then_stmts = List.map (map_stmt fe) r.then_stmts;
                   else_stmts = List.map (map_stmt fe) r.else_stmts }
  | BCase r -> BCase { selector = map_expr fe r.selector;
                       cases = List.map (fun (c, ss) -> (map_expr fe c, List.map (map_stmt fe) ss)) r.cases;
                       default = List.map (map_stmt fe) r.default }
  | BWhile r -> BWhile { condition = map_expr fe r.condition; body = List.map (map_stmt fe) r.body }
  | BFor r -> BFor { init = map_stmt fe r.init; condition = map_expr fe r.condition;
                     update = map_stmt fe r.update; body = List.map (map_stmt fe) r.body }
  | BBlock ss -> BBlock (List.map (map_stmt fe) ss)
  | BCallStmt r -> BCallStmt { r with args = List.map (map_expr fe) r.args }
  | BReturn (Some e) -> BReturn (Some (map_expr fe e))
  | BReturn None -> BReturn None

let map_proc fe = function
  | BCombinational c -> BCombinational { c with body = List.map (map_stmt fe) c.body }
  | BSequential s -> BSequential { s with body = List.map (map_stmt fe) s.body }

(* single-bit static taps sig[k:k] over an expr *)
let rec taps sg acc = function
  | BSlice { signal = BVar s; msb; lsb } when s = sg && msb = lsb -> msb :: acc
  | BSlice { signal; _ } -> taps sg acc signal
  | BBinOp { lhs; rhs; _ } -> taps sg (taps sg acc lhs) rhs
  | BUnOp { operand; _ } -> taps sg acc operand
  | BSelect { array; index } -> taps sg (taps sg acc array) index
  | BConcat es -> List.fold_left (taps sg) acc es
  | BReplicate { value; _ } -> taps sg acc value
  | BCond { condition; then_val; else_val } -> taps sg (taps sg (taps sg acc condition) then_val) else_val
  | BCall { args; _ } -> List.fold_left (taps sg) acc args
  | _ -> acc

(* read of sig by anything OTHER than a single-bit static tap blocks SRL use *)
let rec bad_read sg = function
  | BVar s -> s = sg
  | BSlice { signal = BVar s; msb; lsb } when s = sg -> msb <> lsb
  | BSlice { signal; _ } -> bad_read sg signal
  | BBinOp { lhs; rhs; _ } -> bad_read sg lhs || bad_read sg rhs
  | BUnOp { operand; _ } -> bad_read sg operand
  | BSelect { array; index } -> bad_read sg array || bad_read sg index
  | BConcat es -> List.exists (bad_read sg) es
  | BReplicate { value; _ } -> bad_read sg value
  | BCond { condition; then_val; else_val } -> bad_read sg condition || bad_read sg then_val || bad_read sg else_val
  | BCall { args; _ } -> List.exists (bad_read sg) args
  | _ -> false

let rec stmt_exprs = function
  | BAssign { rhs; _ } -> [rhs]
  | BIf { condition; then_stmts; else_stmts } -> condition :: List.concat_map stmt_exprs (then_stmts @ else_stmts)
  | BCase { selector; cases; default } -> selector :: List.concat_map (fun (c, ss) -> c :: List.concat_map stmt_exprs ss) cases @ List.concat_map stmt_exprs default
  | BWhile { condition; body } -> condition :: List.concat_map stmt_exprs body
  | BFor { init; condition; update; body } -> condition :: stmt_exprs init @ stmt_exprs update @ List.concat_map stmt_exprs body
  | BBlock ss -> List.concat_map stmt_exprs ss
  | BCallStmt { args; _ } -> args
  | BReturn (Some e) -> [e]
  | BReturn None -> []

let proc_exprs = function
  | BCombinational c -> List.concat_map stmt_exprs c.body
  | BSequential s -> List.concat_map stmt_exprs s.body

let counter = ref 0

(* find shift-register assigns anywhere in a stmt list.  Recurse into BBlock;
 * an `if(C) <shift>` (no else) gates the shift, so C AND's into the CE. *)
let rec find_shifts m clock ce_ctx acc = function
  | BAssign { lhs = sg; rhs } :: tl ->
      let acc = (match sig_width m sg with
        | Some w when w >= 2 && w <= 4096 ->
            (match match_shift ~sg ~w rhs with
             | Some (ce, d, dir) -> (sg, w, and_ce ce_ctx ce, d, dir, clock) :: acc
             | None -> acc)
        | _ -> acc) in
      find_shifts m clock ce_ctx acc tl
  | BBlock ss :: tl -> find_shifts m clock ce_ctx (find_shifts m clock ce_ctx acc ss) tl
  | BIf { condition; then_stmts; else_stmts = [] } :: tl ->
      let acc = find_shifts m clock (and_ce ce_ctx (Some condition)) acc then_stmts in
      find_shifts m clock ce_ctx acc tl
  | _ :: tl -> find_shifts m clock ce_ctx acc tl
  | [] -> acc

let infer_module (m : bmodule) : bmodule =
  (* 1. gather candidate shift registers (sg, w, ce, d, dir, clock) — any depth;
     the SRL emission cascades SRLC32E for depth > 32. *)
  let cands = List.concat_map (function
    | BSequential { clock; reset = None; body; _ } -> find_shifts m clock None [] body
    | _ -> []) m.processes in
  if cands = [] then m
  else begin
    let cand_sgs = List.map (fun (sg,_,_,_,_,_) -> sg) cands in
    (* drop shift-assigns of the given signals, recursing into BBlock / BIf;
       prune blocks/ifs that become empty. *)
    let rec strip_shift sgs stmts = List.filter_map (fun s -> match s with
      | BAssign { lhs; _ } when List.mem lhs sgs -> None
      | BBlock ss -> (match strip_shift sgs ss with [] -> None | ss' -> Some (BBlock ss'))
      | BIf ({ then_stmts; else_stmts; _ } as r) ->
          let t = strip_shift sgs then_stmts and e = strip_shift sgs else_stmts in
          if t = [] && e = [] then None
          else Some (BIf { r with then_stmts = t; else_stmts = e })
      | s -> Some s) stmts in
    (* processes with ALL candidate shift-assigns stripped, for read analysis *)
    let procs_noshift = List.map (function
      | BSequential s -> BSequential { s with body = strip_shift cand_sgs s.body }
      | p -> p) m.processes in
    let all_exprs = List.concat_map proc_exprs procs_noshift in
    (* 2. validate each candidate: only single-bit taps, no bad reads *)
    let valid = List.filter (fun (sg,_,_,_,_,_) ->
      not (List.exists (bad_read sg) all_exprs)
      && List.exists (fun e -> taps sg [] e <> []) all_exprs) cands in
    if valid = [] then m
    else begin
      (* 3. build SRL instances + tap-net rewrites *)
      let new_insts = ref [] and new_sigs = ref [] in
      let tap_net = Hashtbl.create 32 in  (* (sg,k) -> net name *)
      let mk_sig name =
        new_sigs := { name; stype = BInt { width = 1; signed = Unsigned };
                      direction = `Internal; initial_value = None; attrs = [] } :: !new_sigs in
      let add_inst ?(init = 0) name ty ports =
        new_insts := { inst_name = name; module_name = ty;
                       param_values = [ ("INIT", init) ]; param_strs = [];
                       port_connections = ports } :: !new_insts in
      (* emit the SRL(s) for a single tap at absolute depth k (delay k+1):
         SRL16E for k<16, one SRLC32E for 16<=k<32, cascaded SRLC32E (Q31->D)
         for k>=32.  Returns the Q net for the tap. *)
      let emit_tap ~sg ~a ~ce_e ~d ~clock ~initv ~w ~dir =
        (* Preload the SRL with the register's power-on pattern, or the startup
           sequence is lost: cpll_railing holds the GT CPLL down for 96 cycles
           from `reg [95:0] cpllpd_wait = 96'hFFF...F`, and with INIT(0)
           cpll_pd_out reads 0 from the first cycle.
           Chain depth j is original bit j for a shift-UP (D enters bit 0), and
           bit w-1-j for a shift-DOWN (D enters bit w-1, so the pattern is seen
           in reverse). *)
        let init_bit j =
          let src = match dir with `Up -> j | `Down -> w - 1 - j in
          src >= 0 && src < w && Z.testbit initv src in
        let init_word ~base ~bits =
          let v = ref 0 in
          for j = bits - 1 downto 0 do
            v := (!v lsl 1) lor (if init_bit (base + j) then 1 else 0)
          done; !v in
        if a < 16 then begin
          incr counter;
          let q = Printf.sprintf "%s__srlq%d_%d" sg a !counter in
          mk_sig q;
          let addr = List.init 4 (fun b -> (Printf.sprintf "A%d" b, bconst ((a lsr b) land 1) 1)) in
          add_inst ~init:(init_word ~base:0 ~bits:16) q "SRL16E"
            (("Q", BVar q) :: ("CLK", BVar clock) :: ("CE", ce_e) :: ("D", d) :: addr);
          q
        end else begin
          let nchunks = (a / 32) + 1 in
          let rec build i din =
            incr counter;
            let is_last = (i = nchunks - 1) in
            let av = if is_last then a mod 32 else 31 in
            let q = Printf.sprintf "%s__srlq%d_c%d_%d" sg a i !counter in
            let q31 = Printf.sprintf "%s__srl31_c%d_%d" sg i !counter in
            mk_sig q; mk_sig q31;
            (* cell i holds chain depths 32*i .. 32*i+31 (cell 0 is nearest D) *)
            add_inst ~init:(init_word ~base:(32 * i) ~bits:32) q "SRLC32E"
              [ ("Q", BVar q); ("Q31", BVar q31); ("CLK", BVar clock);
                ("CE", ce_e); ("D", din); ("A", bconst av 5) ];
            if is_last then q else build (i + 1) (BVar q31)
          in
          build 0 d
        end in
      List.iter (fun (sg, w, ce, d, dir, clock) ->
        let ts = List.sort_uniq compare (List.concat_map (taps sg []) all_exprs) in
        let ce_e = match ce with Some e -> e | None -> c1 in
        (* absolute SRL depth for a tap bit k depends on shift direction:
           shift-up inserts at bit 0 so bit k has delay k; shift-down inserts
           at bit w-1 so bit k has delay w-1-k. *)
        List.iter (fun k ->
          let a = match dir with `Up -> k | `Down -> w - 1 - k in
          Hashtbl.replace tap_net (sg, k)
            (emit_tap ~sg ~a ~ce_e ~d ~clock ~initv:(init_of m sg) ~w ~dir)) ts) valid;
      (* 4. rewrite tap reads sig[k:k] -> BVar net; drop shift-assigns; drop sigs *)
      let valid_sgs = List.map (fun (sg,_,_,_,_,_) -> sg) valid in
      let fe = function
        | BSlice { signal = BVar s; msb; lsb } when msb = lsb ->
            (match Hashtbl.find_opt tap_net (s, msb) with
             | Some q -> BVar q | None -> BSlice { signal = BVar s; msb; lsb })
        | e -> e in
      let procs' = List.filter_map (function
        | BSequential s ->
            let body = strip_shift valid_sgs s.body in
            let body = List.filter (fun st -> st <> BBlock []) body in
            if body = [] then None else Some (map_proc fe (BSequential { s with body }))
        | p -> Some (map_proc fe p)) m.processes in
      let signals' = List.filter (fun (s : bsignal) -> not (List.mem s.name valid_sgs)) m.signals in
      Printf.eprintf "[srl_infer] %s: mapped %d shift register(s) -> %d SRL cell(s)\n"
        m.name (List.length valid) (List.length !new_insts);
      { m with processes = procs'; signals = signals' @ !new_sigs;
               instances = m.instances @ !new_insts }
    end
  end

let infer_program (p : bprogram) : bprogram =
  { p with modules = List.map infer_module p.modules }
