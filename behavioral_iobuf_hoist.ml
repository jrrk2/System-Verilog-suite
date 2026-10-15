(* Inout ports: hoist behavioural tristate drives onto IOBUF black boxes.
 *
 *   inout io;  assign io = oe ? din : 1'bz;  assign dout = io;
 *
 * becomes
 *
 *   io__o  := din;          -- value driven onto the pad
 *   io__t  := ~oe;          -- IOBUF.T: 1 = tristate (Xilinx polarity)
 *   io__iobuf_b0 : IOBUF (.I(io__o), .T(io__t), .O(io__i), .IO(io))
 *   dout   := io__i;        -- every read of the pad sees IOBUF.O
 *
 * The pad is then only touched by the IOBUF, which is what the FPGA flow
 * needs (create_circuit keeps the pad through `__keep_io`), and what the Z3
 * miter needs: both the behavioural spec and the mapped netlist carry the
 * SAME black box with the same instance name, so `cut_blackboxes` ties
 * `io__iobuf_b0/O` to a shared free input and compares `io__iobuf_b0/I`
 * and `/T` -- the pad is compared as three separate signals (sense, drive,
 * direction) instead of one value that no encoding can represent.
 *
 * The frontend marks a high-impedance literal as `BCall "@z"`; the drive
 * decomposition below turns a nest of `cond ? v : 'z` into (value, enable)
 * per bit.  Any `@z` left after hoisting (a tristate on a non-inout net)
 * falls back to 0, which is what the flow did before. *)

open Behavioral_ir

let is_z = function BCall { func = "@z"; _ } -> true | _ -> false

let ones w = BConst { value = Z.sub (Z.shift_left Z.one w) Z.one; width = w }
let zeros w = BConst { value = Z.zero; width = w }

(* (value, enable) of a driver expression, enable per bit. *)
let rec drive ~w e =
  match e with
  | BCall { func = "@z"; _ } -> zeros w, zeros w
  | BCond { condition; then_val; else_val } ->
      let vt, et = drive ~w then_val and vf, ef = drive ~w else_val in
      BCond { condition; then_val = vt; else_val = vf },
      BCond { condition; then_val = et; else_val = ef }
  | e -> e, ones w

let rec map_expr f e =
  match f e with
  | Some e' -> e'
  | None ->
    (match e with
     | BVar _ | BConst _ -> e
     | BBinOp r -> BBinOp { r with lhs = map_expr f r.lhs; rhs = map_expr f r.rhs }
     | BUnOp r -> BUnOp { r with operand = map_expr f r.operand }
     | BSelect { array; index } -> BSelect { array = map_expr f array; index = map_expr f index }
     | BSlice r -> BSlice { r with signal = map_expr f r.signal }
     | BConcat l -> BConcat (List.map (map_expr f) l)
     | BReplicate r -> BReplicate { r with value = map_expr f r.value }
     | BCond r -> BCond { condition = map_expr f r.condition; then_val = map_expr f r.then_val
                        ; else_val = map_expr f r.else_val }
     | BCall r -> BCall { r with args = List.map (map_expr f) r.args })

let rec map_stmt fe fa s =
  match s with
  | BAssign { lhs; rhs } -> fa lhs (map_expr fe rhs)
  | BIf r -> [ BIf { condition = map_expr fe r.condition
                   ; then_stmts = List.concat_map (map_stmt fe fa) r.then_stmts
                   ; else_stmts = List.concat_map (map_stmt fe fa) r.else_stmts } ]
  | BCase r -> [ BCase { selector = map_expr fe r.selector
                       ; cases = List.map (fun (k, b) -> (map_expr fe k, List.concat_map (map_stmt fe fa) b)) r.cases
                       ; default = List.concat_map (map_stmt fe fa) r.default } ]
  | BWhile r -> [ BWhile { condition = map_expr fe r.condition; body = List.concat_map (map_stmt fe fa) r.body } ]
  | BFor r -> [ BFor { r with condition = map_expr fe r.condition; body = List.concat_map (map_stmt fe fa) r.body } ]
  | BBlock b -> [ BBlock (List.concat_map (map_stmt fe fa) b) ]
  | BCallStmt r -> [ BCallStmt { r with args = List.map (map_expr fe) r.args } ]
  | BReturn e -> [ BReturn (Option.map (map_expr fe) e) ]

let map_proc fe fa = function
  | BCombinational r -> BCombinational { r with body = List.concat_map (map_stmt fe fa) r.body }
  | BSequential r -> BSequential { r with body = List.concat_map (map_stmt fe fa) r.body }

let iobuf_ports : library_port list =
  [ { port_name = "I"; port_direction = `Input; port_width = 1 }
  ; { port_name = "T"; port_direction = `Input; port_width = 1 }
  ; { port_name = "O"; port_direction = `Output; port_width = 1 }
  ; { port_name = "IO"; port_direction = `Output; port_width = 1 } ]

let rec assigns_to name = function
  | BAssign { lhs; _ } -> lhs = name
  | BIf { then_stmts; else_stmts; _ } -> List.exists (assigns_to name) then_stmts || List.exists (assigns_to name) else_stmts
  | BCase { cases; default; _ } -> List.exists (fun (_, b) -> List.exists (assigns_to name) b) cases || List.exists (assigns_to name) default
  | BBlock b | BWhile { body = b; _ } | BFor { body = b; _ } -> List.exists (assigns_to name) b
  | _ -> false

let hoist_module (m : bmodule) : bmodule * int =
  let written p = List.exists (function
    | BCombinational { body; _ } | BSequential { body; _ } -> List.exists (assigns_to p) body) m.processes in
  let pads = List.filter_map (fun (s : bsignal) ->
    match s.direction, s.stype with
    | `Inout, BInt { width; _ } when written s.name -> Some (s.name, width)
    | `Inout, BBool when written s.name -> Some (s.name, 1)
    | _ -> None) m.signals in
  if pads = [] then m, 0
  else begin
    let sig_ name w dir = { name; stype = BInt { width = w; signed = Unsigned }
                          ; direction = dir; initial_value = None; attrs = [] } in
    let new_sigs = ref [] and new_insts = ref [] and new_assigns = ref [] in
    let bit e k w = if w = 1 then e else BSlice { signal = e; msb = k; lsb = k } in
    List.iter (fun (p, w) ->
      let o = p ^ "__o" and t = p ^ "__t" and i = p ^ "__i" in
      new_sigs := sig_ i w `Internal :: sig_ t w `Internal :: sig_ o w `Internal :: !new_sigs;
      let ibits = List.init w (fun k -> Printf.sprintf "%s__i_b%d" p k) in
      List.iter (fun n -> new_sigs := sig_ n 1 `Internal :: !new_sigs) ibits;
      new_assigns := BAssign { lhs = i; rhs = (match ibits with [ b ] -> BVar b | bs -> BConcat (List.rev_map (fun b -> BVar b) bs)) } :: !new_assigns;
      List.iteri (fun k ib ->
        new_insts := { inst_name = Printf.sprintf "%s__iobuf_b%d" p k; module_name = "IOBUF"
                     ; param_values = []; param_strs = []
                     ; port_connections = [ ("I", bit (BVar o) k w); ("T", bit (BVar t) k w)
                                          ; ("O", BVar ib); ("IO", bit (BVar p) k w) ] }
                     :: !new_insts) ibits) pads;
    let widths = Hashtbl.create 8 in
    List.iter (fun (p, w) -> Hashtbl.replace widths p w) pads;
    (* reads of the pad -> the sensed value *)
    let fe = function
      | BVar p when Hashtbl.mem widths p -> Some (BVar (p ^ "__i"))
      | _ -> None in
    (* writes of the pad -> (value, ~enable) *)
    let fa lhs rhs =
      match Hashtbl.find_opt widths lhs with
      | Some w ->
          let v, en = drive ~w rhs in
          [ BAssign { lhs = lhs ^ "__o"; rhs = v }
          ; BAssign { lhs = lhs ^ "__t"
                    ; rhs = BUnOp { op = BNot; operand = en
                                  ; result_type = BInt { width = w; signed = Unsigned } } } ]
      | None -> [ BAssign { lhs; rhs } ] in
    let processes = List.map (map_proc fe fa) m.processes in
    let instances = List.map (fun (inst : binstance) ->
      { inst with port_connections =
          List.map (fun (pn, e) -> (pn, if pn = "IO" || pn = "IOB" then e else map_expr fe e)) inst.port_connections })
      m.instances in
    let sense = BCombinational { name = "__iobuf_sense"; sensitivity = [ BAny ]; body = List.rev !new_assigns } in
    Printf.eprintf "[iobuf_hoist] %s: %d inout pad(s) -> %d IOBUF(s)\n%!"
      m.name (List.length pads) (List.length !new_insts);
    { m with signals = m.signals @ List.rev !new_sigs
           ; processes = processes @ [ sense ]
           ; instances = instances @ List.rev !new_insts }, List.length !new_insts
  end

(* Any `@z` still present (tristate on a non-inout net): 0, as before. *)
let strip_z_module (m : bmodule) : bmodule =
  let fe = function
    | BCall { func = "@z"; args = [ BConst { width; _ } ] } ->
        Some (BConst { value = Z.zero; width = (if width = 0 then 1 else width) })
    | BCall { func = "@z"; _ } -> Some (BConst { value = Z.zero; width = 1 })
    | _ -> None in
  let fa lhs rhs = [ BAssign { lhs; rhs } ] in
  { m with processes = List.map (map_proc fe fa) m.processes
         ; instances = List.map (fun (i : binstance) ->
             { i with port_connections = List.map (fun (p, e) -> (p, map_expr fe e)) i.port_connections }) m.instances }

let hoist_program (p : bprogram) : bprogram =
  let n = ref 0 in
  let modules = List.map (fun m -> let m', k = hoist_module m in n := !n + k; strip_z_module m') p.modules in
  let library_cells =
    if !n > 0 && not (List.mem_assoc "IOBUF" p.library_cells)
    then p.library_cells @ [ ("IOBUF", iobuf_ports) ] else p.library_cells in
  { modules; library_cells }

(* Module-level entry for the gate_map / prep_for_z3 hooks. *)
let hoist_one (m : bmodule) : bmodule = strip_z_module (fst (hoist_module m))
