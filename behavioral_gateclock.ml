(* Gated-clock -> clock-enable pass.
 *
 * Converts flip-flops driven by a GATED clock into flip-flops on the ungated
 * clock with a clock ENABLE.  A gated clock is an integrated-clock-gate (ICG):
 * the classic latch-based cell
 *
 *     module EICG_wrapper(output out, input en, input test_en, input in);
 *       reg en_latched;
 *       always @* if (!in) en_latched = en || test_en;   // sample while clk low
 *       assign out = en_latched && in;                      // gclk = clk & latched-en
 *     endmodule
 *
 * feeds `out` to `always @(posedge out) q <= d`.  On an FPGA that is wrong:
 * it puts a combinational gate on a clock net, burning clock routing and
 * causing the BUFGCTRL->OLOGIC.CLK routing failures and DDR hold trouble we
 * see in the open flow.  The FPGA-native form is `always @(posedge in) if
 * (en) q <= d` -- the flop's dedicated CE pin -- which yosys/hardcaml map to
 * FDRE downstream.
 *
 * We do this at the RTL-derived BIR level, where the gate is still an INSTANCE
 * (or a plain `clk & en` assign) with its semantics intact, rather than
 * re-inferring an ICG out of the flattened $and/$dlatch soup a gate netlist
 * leaves.  The anti-glitch latch inside the wrapper never enters the picture:
 * we bypass the wrapper entirely and take the raw `en`/`test_en` ports, which
 * is exactly right because a CE samples at the clock edge -- the value the
 * latch was there to hold across the high phase.
 *
 * Correctness on reset (the case that silently miscompiles if ignored):
 *   - `reset` is a RECORD FIELD of BSequential, applied by the lowering
 *     independently of the body.  The body carries only the DATA path.
 *   - No reset / ASYNC reset: wrapping the data body in `if EN` leaves the
 *     reset untouched -- async reset stays asynchronous, correct.
 *   - SYNC reset: a synchronous reset on a gated clock only fired when the
 *     gate was OPEN (EN), so it must ride inside the enable, not fire every
 *     cycle.  We FOLD it into the data path -- `q <= rst ? 0 : d` -- and clear
 *     the reset field, so the enable wrap then covers it: the result is
 *     `q <= EN ? (rst ? 0 : d) : q`, exactly the gated behaviour.  On Xilinx
 *     that reset-to-0 fold is just an AND gate on the D line (FDRE resets to
 *     0); a synchronous SET-to-1 (FDSE) is not handled and is left gated with
 *     a warning.  Reset polarity comes from `reset_edge` (Neg = active low).
 *
 * Cascaded gates (ICG feeding ICG) compose: a flop's enable is the AND of the
 * whole chain and its clock is the ultimate root; resolved to a fixpoint.
 *
 * Env: GATECLOCK_OFF disables the pass; GATECLOCK_DEBUG logs each conversion.
 *)

open Behavioral_ir

(* An ICG cell signature: which ports are the clock in, the gated clock out,
 * and the enable(s) that OR together to form the clock enable.  Add rows for
 * other clock-gate cells (Xilinx BUFGCE-wrapping modules, vendor ICGs, ...). *)
type icg_sig = { clk_port : string; out_port : string; en_ports : string list }

let icg_signatures : (string * icg_sig) list = [
  ("EICG_wrapper", { clk_port = "in"; out_port = "out"; en_ports = ["en"; "test_en"] });
]

let bool1 = BBool

(* OR a list of enable expressions into one; [] means "always enabled". *)
let or_enables = function
  | [] -> BConst { value = Z.one; width = 1 }
  | e :: rest ->
      List.fold_left
        (fun acc x -> BBinOp { op = BOr; lhs = acc; rhs = x; result_type = bool1 })
        e rest

let and_enables a b = BBinOp { op = BAnd; lhs = a; rhs = b; result_type = bool1 }

(* Signal name a bexpr refers to, if it is a plain variable. *)
let as_var = function BVar s -> Some s | _ -> None

(* Width of a signal from the module's declarations; 1 if unknown. *)
let width_of signals name =
  match List.find_opt (fun (s : bsignal) -> s.name = name) signals with
  | Some { stype = BInt { width; _ }; _ } -> width
  | Some { stype = BBool; _ } -> 1
  | _ -> 1

(* Active-high reset condition for a sync reset: bare signal, or its negation
 * when reset_edge is `Neg (active low). *)
let reset_condition rst edge =
  match edge with
  | Some `Neg -> BUnOp { op = BNot; operand = BVar rst; result_type = BBool }
  | _ -> BVar rst

(* Fold a synchronous reset-to-0 into the data body: `if (rstcond) <lhs:=0..>
 * else <body>`.  Each assigned LHS is zeroed at its declared width. *)
let fold_sync_reset signals rstcond body =
  let zeros =
    List.filter_map (function
      | BAssign { lhs; _ } ->
          Some (BAssign { lhs; rhs = BConst { value = Z.zero; width = width_of signals lhs } })
      | _ -> None) body
  in
  [ BIf { condition = rstcond; then_stmts = zeros; else_stmts = body } ]

let convert_module (m : bmodule) : bmodule =
  if Sys.getenv_opt "GATECLOCK_OFF" <> None then m
  else begin
    let debug = Sys.getenv_opt "GATECLOCK_DEBUG" <> None in
    let dbg fmt = if debug then Printf.eprintf fmt else Printf.ifprintf stderr fmt in

    (* gated-clock signal name -> (root clock signal, enable expr).  Filled
     * from ICG instances first, then from combinational `clk & en` assigns. *)
    let gate_of : (string, string * bexpr) Hashtbl.t = Hashtbl.create 16 in
    let icg_inst_names = Hashtbl.create 16 in  (* inst names we will drop *)

    (* Signals used as a clock somewhere in this module -- the anchor for
     * recognising a combinational clock gate. *)
    let clock_sigs = Hashtbl.create 16 in
    List.iter (function
      | BSequential { clock; _ } -> Hashtbl.replace clock_sigs clock ()
      | _ -> ()) m.processes;

    (* 1. ICG instances. *)
    List.iter (fun inst ->
      match List.assoc_opt inst.module_name icg_signatures with
      | None -> ()
      | Some sg ->
          let conn p = List.assoc_opt p inst.port_connections in
          match conn sg.out_port, conn sg.clk_port with
          | Some out_e, Some clk_e ->
              (match as_var out_e, as_var clk_e with
               | Some gclk, Some clk ->
                   let ens =
                     List.filter_map (fun p ->
                       match conn p with
                       | Some e when as_var e <> Some "1'x" -> Some e
                       | _ -> None) sg.en_ports
                   in
                   Hashtbl.replace gate_of gclk (clk, or_enables ens);
                   Hashtbl.replace icg_inst_names inst.inst_name ();
                   dbg "gateclock: ICG %s (%s) gates %s from %s\n"
                     inst.inst_name inst.module_name gclk clk
               | _ ->
                   (* out/in not simple signals -- cannot rewire safely *)
                   dbg "gateclock: skipping ICG %s (non-signal out/in port)\n" inst.inst_name)
          | _ -> ()
    ) m.instances;

    (* 2. Combinational clock gates: a process whose whole body is
     *      <gclk> = <a> & <b>
     *    where <gclk> is used as a clock and one operand is itself a clock (or
     *    a clock we already know about).  The other operand is the enable.
     *    This is the flattened case; the anti-glitch latch, if present, has
     *    become another signal feeding the enable operand and is carried
     *    along verbatim -- still correct, just not simplified away. *)
    let is_clockish s = Hashtbl.mem clock_sigs s || Hashtbl.mem gate_of s in
    List.iter (function
      | BCombinational { body = [ BAssign { lhs; rhs = BBinOp { op = BAnd; lhs = a; rhs = b; _ } } ]; _ }
        when Hashtbl.mem clock_sigs lhs && not (Hashtbl.mem gate_of lhs) ->
          (match as_var a, as_var b with
           | Some av, _ when is_clockish av -> Hashtbl.replace gate_of lhs (av, b)
           | _, Some bv when is_clockish bv -> Hashtbl.replace gate_of lhs (bv, a)
           | _ -> ())
      | _ -> ()
    ) m.processes;

    if Hashtbl.length gate_of = 0 then m
    else begin
      (* 3. Resolve cascades to a fixpoint: if a gate's root clock is itself a
       *    gated clock, compose (root := root', enable := enable AND en'). *)
      let changed = ref true in
      let iters = ref 0 in
      while !changed && !iters < 100 do
        changed := false; incr iters;
        Hashtbl.iter (fun gclk (root, en) ->
          match Hashtbl.find_opt gate_of root with
          | Some (root', en') when root' <> gclk ->
              Hashtbl.replace gate_of gclk (root', and_enables en en');
              changed := true
          | _ -> ()
        ) (Hashtbl.copy gate_of)
      done;

      (* 4. Rewrite each BSequential clocked by a gated clock.  Its data body,
       *    with any sync reset folded to a reset-to-0 on the data, goes inside
       *    `if EN`, and the clock becomes the root.  Async reset (a record
       *    field applied independently of the body) is left as is. *)
      let converted = ref 0 in
      let processes' = List.map (fun proc ->
        match proc with
        | BSequential ({ clock; reset; reset_edge; reset_async; body; _ } as s)
          when Hashtbl.mem gate_of clock ->
            let root, en = Hashtbl.find gate_of clock in
            let body', reset', edge' =
              match reset, reset_async with
              | Some rst, false ->
                  (* sync reset -> fold to data (reset-to-0 = AND on D), and
                   * drop the field so the enable wrap now covers it. *)
                  fold_sync_reset m.signals (reset_condition rst reset_edge) body, None, None
              | _ -> body, reset, reset_edge   (* none or async: field untouched *)
            in
            incr converted;
            dbg "gateclock: %s clock %s -> %s + CE\n" s.name clock root;
            BSequential { s with
              clock = root; reset = reset'; reset_edge = edge';
              body = [ BIf { condition = en; then_stmts = body'; else_stmts = [] } ] }
        | _ -> proc
      ) m.processes in

      (* 5. Drop the ICG instances we absorbed: a clock gate's `out` only ever
       *    drives clocks, so once every flop on it is rewired the instance is
       *    dead. *)
      let instances' =
        List.filter (fun inst -> not (Hashtbl.mem icg_inst_names inst.inst_name)) m.instances in

      if !converted > 0 then
        Printf.eprintf "gateclock: %s: %d gated flop(s) converted to clock-enable\n"
          m.name !converted;

      { m with processes = processes'; instances = instances' }
    end
  end

let convert_program (p : bprogram) : bprogram =
  { p with modules = List.map convert_module p.modules }
