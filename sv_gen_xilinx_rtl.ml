(* sv_gen_xilinx_rtl.ml
 *
 * Post-pass over the structural backend's AST that renames primitive cells
 * and their pins to Vivado RTL elaboration names (RTL_INV, RTL_AND, RTL_OR,
 * RTL_XOR, RTL_MUX, RTL_ADD, RTL_REG_SYNC__BREG_<W>, ...).
 *
 * Vivado `synth_design -rtl` emits cells from this generic library; matching
 * those names is the goal of the xilinx_rtl backend. This pass is purely a
 * relabeling — the underlying primitives were already inferred by
 * Sv_tran_struct, so no semantic transformation is needed for combinational
 * primitives. The synchronous-register fusion (collapsing dff_en + reset
 * gates into one RTL_REG_SYNC) lives in a separate pass.
 *)

open Sv_ast

(* Read an int-valued GPARAM from a primitive module's parameter list. *)
let get_param_int stmts name default =
  let rec find = function
    | [] -> default
    | Var { name = n; value = Some (Const { name = v; _ });
            is_param = true; _ } :: _ when n = name ->
        (try int_of_string v with _ -> default)
    | _ :: rest -> find rest
  in
  find stmts

(* Apply a port rename to a Pin node. *)
let rename_pin port_renames = function
  | Pin { name; expr } ->
      let new_name =
        try List.assoc name port_renames
        with Not_found -> name
      in
      Pin { name = new_name; expr }
  | other -> other

(* True iff the pin's expression is the constant `target` (e.g. "1'b0"). *)
let pin_is_const target = function
  | Pin { expr = Some (Const { name; _ }); _ } -> name = target
  | _ -> false

(* Map a structural primitive name to its Vivado RTL_* equivalent and
 * the corresponding pin renames.
 *
 *   width        — from the WIDTH GPARAM; used for the BREG_<W> suffix.
 *   is_async     — from the IS_ASYNC GPARAM (set by the structural
 *                  classifier when the source's sensitivity list has a
 *                  reset edge); chooses ASYNC vs SYNC variant.
 *   has_reset    — true iff the dff_en cell's RST pin is wired to a
 *                  non-trivial signal (not 1'b0). When false, Vivado's
 *                  matching cell is plain RTL_REG (no reset port).
 *)
let map_primitive prim_name width is_async has_reset =
  let reg_family suffix =
    if not has_reset then "RTL_REG"
    else if is_async then "RTL_REG_ASYNC" ^ suffix
    else "RTL_REG_SYNC" ^ suffix
  in
  match prim_name with
  | "bitwise_not" ->
      Some ("RTL_INV", [("in", "I0"); ("out", "O")])
  | "bitwise_and" ->
      Some ("RTL_AND", [("a", "I0"); ("b", "I1"); ("out", "O")])
  | "bitwise_or" ->
      Some ("RTL_OR",  [("a", "I0"); ("b", "I1"); ("out", "O")])
  | "bitwise_xor" ->
      Some ("RTL_XOR", [("a", "I0"); ("b", "I1"); ("out", "O")])
  | "mux2" ->
      Some ("RTL_MUX", [("sel", "S"); ("in0", "I0"); ("in1", "I1"); ("out", "O")])
  | "adder" ->
      Some ("RTL_ADD", [("a", "I0"); ("b", "I1"); ("out", "O")])
  | "subtractor" ->
      Some ("RTL_SUB", [("a", "I0"); ("b", "I1"); ("out", "O")])
  | "multiplier" ->
      Some ("RTL_MUL", [("a", "I0"); ("b", "I1"); ("out", "O")])
  | "shifter_left" ->
      Some ("RTL_LSHIFT", [("a", "I0"); ("b", "I1"); ("out", "O")])
  | "shifter_right" ->
      Some ("RTL_RSHIFT", [("a", "I0"); ("b", "I1"); ("out", "O")])
  | "comparator" | "comparator_eq" ->
      Some ("RTL_EQ",  [("a", "I0"); ("b", "I1"); ("out", "O")])
  | "comparator_neq" ->
      Some ("RTL_NEQ", [("a", "I0"); ("b", "I1"); ("out", "O")])
  | "comparator_lt"  | "comparator_lt_signed" ->
      Some ("RTL_LT",  [("a", "I0"); ("b", "I1"); ("out", "O")])
  | "comparator_lte" ->
      Some ("RTL_LEQ", [("a", "I0"); ("b", "I1"); ("out", "O")])
  | "comparator_gt" ->
      Some ("RTL_GT",  [("a", "I0"); ("b", "I1"); ("out", "O")])
  | "comparator_gte" ->
      Some ("RTL_GEQ", [("a", "I0"); ("b", "I1"); ("out", "O")])
  | "dff_en" ->
      Some (Printf.sprintf "%s__BREG_%d" (reg_family "") width,
            [("clk", "C"); ("d", "D"); ("q", "Q"); ("rst", "RST")])
  | "dffn_en" ->
      Some (Printf.sprintf "%s_N__BREG_%d" (reg_family "") width,
            [("clk", "C"); ("d", "D"); ("q", "Q"); ("rst", "RST")])
  | _ -> None

(* For RTL_REG_SYNC the dff_en pins include `en` (always tied to 1'b1 by the
 * structural backend when there's no enable). RTL_REG_SYNC has no EN port,
 * so we drop pins not in the rename list. We also drop a constant-zero RST
 * pin when present, since Vivado emits the cell without an RST in that case.
 *)
let filter_pins_for_rtl_reg port_renames pins =
  List.filter (fun pin ->
    match pin with
    | Pin { name; _ } when not (List.mem_assoc name port_renames) -> false
    | Pin { name = "rst"; _ } when pin_is_const "1'b0" pin -> false
    | _ -> true
  ) pins

(* For combinational primitives, just keep mapped pins. *)
let filter_pins_combinational port_renames pins =
  List.filter (fun pin ->
    match pin with
    | Pin { name; _ } -> List.mem_assoc name port_renames
    | _ -> true
  ) pins

(* ─── Sync-reset fusion ──────────────────────────────────────────────────
 *
 * The structural backend models `q <= rst ? 0 : d` as
 *     wire_inv = ~rst         (bitwise_not)
 *     wire_d   = wire_inv & d (bitwise_and)
 *     dff_en(.rst(1'b0), .d(wire_d), .q(q))
 * Vivado emits a single RTL_REG_SYNC with the real rst as RST and d as D.
 * We restore that fused form by recognising the (~rst & d) → dff_en chain
 * and rewriting the dff_en's RST and D pins to the original signals.
 * ─────────────────────────────────────────────────────────────────────── *)

let pin_var_name = function
  | Pin { expr = Some (VarRef { name; _ }); _ } -> Some name
  | _ -> None

let find_pin pin_name pins =
  List.find_opt
    (function Pin { name; _ } -> name = pin_name | _ -> false)
    pins

let cell_inst_name = function
  | Cell { name; _ } -> name
  | _ -> ""

let cell_prim_name = function
  | Cell { modp_addr = Some (Module { name; _ }); _ } -> Some name
  | _ -> None

(* Output-pin name for the structural primitives we know about. *)
let primitive_output_pin = function
  | "bitwise_not" | "bitwise_and" | "bitwise_or" | "bitwise_xor"
  | "adder" | "subtractor" | "mux2" -> Some "out"
  | _ -> None

let fuse_sync_reset stmts =
  (* wire_name -> producing cell *)
  let producers = Hashtbl.create 32 in
  List.iter (fun s ->
    match cell_prim_name s with
    | Some prim ->
        (match primitive_output_pin prim with
         | Some out_name ->
             let pins = match s with Cell { pins; _ } -> pins | _ -> [] in
             (match find_pin out_name pins with
              | Some p ->
                  (match pin_var_name p with
                   | Some w -> Hashtbl.replace producers w s
                   | None -> ())
              | None -> ())
         | None -> ())
    | None -> ()
  ) stmts;

  let eliminated = Hashtbl.create 16 in
  let mark c = Hashtbl.replace eliminated (cell_inst_name c) () in

  let try_fuse_dff dff_pins =
    (* Need .rst tied to 1'b0 and .d driven by bitwise_and(...) *)
    let is_rst_zero =
      match find_pin "rst" dff_pins with
      | Some p -> pin_is_const "1'b0" p
      | None -> true
    in
    if not is_rst_zero then None
    else
      match find_pin "d" dff_pins with
      | None -> None
      | Some d_pin ->
          (match pin_var_name d_pin with
           | None -> None
           | Some d_wire ->
               (match Hashtbl.find_opt producers d_wire with
                | Some (Cell { modp_addr = Some (Module { name = "bitwise_and"; _ });
                               pins = and_pins; _ } as and_cell) ->
                    let a = find_pin "a" and_pins in
                    let b = find_pin "b" and_pins in
                    (match a, b with
                     | Some pa, Some pb ->
                         let an = pin_var_name pa in
                         let bn = pin_var_name pb in
                         (* If one of an/bn is the output of a bitwise_not, the
                          * other is the real D and the inv input is the real RST. *)
                         let pick inv_w other_w =
                           match inv_w with
                           | None -> None
                           | Some w ->
                               (match Hashtbl.find_opt producers w with
                                | Some (Cell { modp_addr = Some (Module { name = "bitwise_not"; _ });
                                               pins = inv_pins; _ } as inv_cell) ->
                                    (match find_pin "in" inv_pins with
                                     | Some ip ->
                                         (match pin_var_name ip with
                                          | Some real_rst ->
                                              mark and_cell;
                                              mark inv_cell;
                                              (match other_w with
                                               | Some real_d ->
                                                   Some (real_rst, real_d)
                                               | None -> None)
                                          | None -> None)
                                     | None -> None)
                                | _ -> None)
                         in
                         (match pick an bn with
                          | Some r -> Some r
                          | None -> pick bn an)
                     | _ -> None)
                | _ -> None))
  in

  let rewrite_dff inst_name mod_node pins (real_rst, real_d) =
    let new_pins = List.map (function
      | Pin { name = "rst"; _ } ->
          Pin { name = "rst";
                expr = Some (VarRef { name = real_rst;
                                      access = "RD"; dtype_ref = None }) }
      | Pin { name = "d"; _ } ->
          Pin { name = "d";
                expr = Some (VarRef { name = real_d;
                                      access = "RD"; dtype_ref = None }) }
      | other -> other
    ) pins in
    Cell { name = inst_name; modp_addr = Some mod_node; pins = new_pins }
  in

  let fused = List.map (fun s ->
    match s with
    | Cell { name = inst; modp_addr = Some (Module { name = "dff_en"; _ } as m);
             pins } ->
        (match try_fuse_dff pins with
         | Some r -> rewrite_dff inst m pins r
         | None -> s)
    | _ -> s
  ) stmts in

  List.filter (fun s ->
    match s with
    | Cell { name; _ } -> not (Hashtbl.mem eliminated name)
    | _ -> true
  ) fused

let rewrite_cell cell =
  match cell with
  | Cell {
      name = inst_name;
      modp_addr = Some (Module { name = prim_name; stmts = mod_stmts });
      pins
    } ->
      let width = get_param_int mod_stmts "WIDTH" 1 in
      let is_async = get_param_int mod_stmts "IS_ASYNC" 0 = 1 in
      (* Has a real reset iff the dff_en's rst pin is wired to a non-trivial
       * signal (the structural backend defaults to 1'b0 when there's no
       * reset). *)
      let has_reset =
        match find_pin "rst" pins with
        | Some pin -> not (pin_is_const "1'b0" pin)
        | None -> false
      in
      (match map_primitive prim_name width is_async has_reset with
       | Some (rtl_name, port_renames) ->
           let is_reg = String.length rtl_name >= 8 &&
                        String.sub rtl_name 0 8 = "RTL_REG_" in
           let kept_pins =
             if is_reg then filter_pins_for_rtl_reg port_renames pins
             else filter_pins_combinational port_renames pins
           in
           let renamed_pins = List.map (rename_pin port_renames) kept_pins in
           (* Vivado RTL_* cells take no parameters — strip the parameter list. *)
           Cell {
             name = inst_name;
             modp_addr = Some (Module { name = rtl_name; stmts = [] });
             pins = renamed_pins;
           }
       | None ->
           cell)
  | other -> other

let rec rewrite_node = function
  | Netlist ns ->
      Netlist (List.map rewrite_node ns)
  | Module { name; stmts } ->
      (* Run sync-reset fusion before renaming so we can match on the original
       * bitwise_and / bitwise_not / dff_en names. *)
      let fused = fuse_sync_reset stmts in
      Module { name; stmts = List.map rewrite_node fused }
  | Package { name; stmts } ->
      Package { name; stmts = List.map rewrite_node stmts }
  | Interface { name; params; stmts } ->
      Interface { name; params; stmts = List.map rewrite_node stmts }
  | Cell _ as c -> rewrite_cell c
  | other -> other

let transform ast = rewrite_node ast
