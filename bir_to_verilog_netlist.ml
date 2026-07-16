(* Direct BIR → gate-level structural Verilog emitter.
 *
 * Third writer in parallel with bir_to_edif and bir_to_nextpnr_json:
 * same flattened bmodule + library_cells input, emits structural Verilog
 * that xsim/xelab can simulate against `unisims_ver` for a quick
 * functional sanity check on the SVS internal graph.
 *
 * Naming convention matches bir_to_edif: internal nets get sequential
 * IDs `n_<num>` so a textual diff between the EDIF and Verilog forms is
 * informative if the two ever drift apart.
 *)

open Behavioral_ir

(* -------------- net-id allocator (mirrors bir_to_edif) -------------- *)

type net_key = { base : string; bit : int }

type ctx = {
  ids       : (net_key, int) Hashtbl.t;
  next_id   : int ref;
  widths    : (string, int) Hashtbl.t;
}

let mk_ctx () = {
  ids     = Hashtbl.create 4096;
  next_id = ref 100;
  widths  = Hashtbl.create 256;
}

let alloc ctx (k : net_key) : int =
  match Hashtbl.find_opt ctx.ids k with
  | Some i -> i
  | None ->
      let i = !(ctx.next_id) in
      ctx.next_id := i + 1;
      Hashtbl.add ctx.ids k i;
      i

let net_name_of_id i = Printf.sprintf "n_%d" i

let const_net = function
  | "VCC" | "<const1>" -> Some "n_VCC"
  | "GND" | "<const0>" -> Some "n_GND"
  | _ -> None

let populate_widths ctx (m : bmodule) =
  List.iter (fun (s : bsignal) ->
    let w = match s.stype with
      | BInt { width; _ } -> width
      | BBool             -> 1
      | _                 -> 1 in
    Hashtbl.replace ctx.widths s.name w) m.signals

let rec nets_of_conn ctx (e : bexpr) : string list =
  match e with
  | BConst { value; width } ->
      let rec range i = if i >= width then [] else
        let b = (if Z.testbit value i then 1 else 0) in
        (if b = 1 then "n_VCC" else "n_GND") :: range (i + 1) in
      range 0
  | BConcat es ->
      List.concat_map (nets_of_conn ctx) (List.rev es)
  | BSlice { signal; msb; lsb } ->
      let base = match signal with
        | BVar nm -> nm
        | _ -> failwith ("bir_to_verilog_netlist: slice of non-BVar: "
                         ^ Behavioral_ir.string_of_bexpr signal) in
      let rec range lo hi = if lo > hi then [] else lo :: range (lo + 1) hi in
      List.map (fun i ->
        match const_net base with
        | Some n -> n
        | None   -> net_name_of_id (alloc ctx { base; bit = i }))
        (range lsb msb)
  | BVar nm ->
      (match const_net nm with
       | Some n -> [n]
       | None ->
           let w = try Hashtbl.find ctx.widths nm with Not_found -> 1 in
           List.init w (fun i -> net_name_of_id (alloc ctx { base = nm; bit = i })))
  | BSelect { array = BVar nm; index = BConst { value; _ } } ->
      (match const_net nm with
       | Some n -> [n]
       | None   -> [net_name_of_id (alloc ctx { base = nm; bit = Z.to_int value })])
  | _ ->
      failwith ("bir_to_verilog_netlist: unsupported pin expression: "
                ^ Behavioral_ir.string_of_bexpr e)

(* -------------- Verilog identifier safety -------------- *)

let v_safe_char c =
  let cc = Char.code c in
  (cc >= Char.code 'a' && cc <= Char.code 'z')
  || (cc >= Char.code 'A' && cc <= Char.code 'Z')
  || (cc >= Char.code '0' && cc <= Char.code '9')
  || c = '_'

let v_safe_id (s : string) : string =
  let n = String.length s in
  let buf = Buffer.create n in
  for i = 0 to n - 1 do
    let c = s.[i] in
    if v_safe_char c then Buffer.add_char buf c
    else Buffer.add_char buf '_'
  done;
  let r = Buffer.contents buf in
  if String.length r = 0 then "_"
  else if r.[0] >= '0' && r.[0] <= '9' then "n_" ^ r
  else r

(* Verilog parameter literal:
 *   - all-0/1 string → N'bSTR (preserve width)
 *   - already-Verilog literal (contains "'") → verbatim
 *   - otherwise → quoted string literal                                 *)
let v_param_value (v : string) : string =
  let n = String.length v in
  if n = 0 then "\"\""
  else if String.for_all (fun c -> c = '0' || c = '1') v then
    Printf.sprintf "%d'b%s" n v
  else if String.contains v '\'' then v
  else Printf.sprintf "\"%s\"" v

(* -------------- primitive port table -------------- *)

(* Verbatim from bir_to_edif so EDIF/Verilog views stay aligned. *)
let xil_primitive_ports : (string * (string * [`Input|`Output] * int) list) list = [
  "LUT1",   [ "O", `Output, 1; "I0", `Input, 1 ];
  "LUT2",   [ "O", `Output, 1; "I0", `Input, 1; "I1", `Input, 1 ];
  "LUT3",   [ "O", `Output, 1; "I0", `Input, 1; "I1", `Input, 1; "I2", `Input, 1 ];
  "LUT4",   [ "O", `Output, 1; "I0", `Input, 1; "I1", `Input, 1;
              "I2", `Input, 1; "I3", `Input, 1 ];
  "LUT5",   [ "O", `Output, 1; "I0", `Input, 1; "I1", `Input, 1;
              "I2", `Input, 1; "I3", `Input, 1; "I4", `Input, 1 ];
  "LUT6",   [ "O", `Output, 1; "I0", `Input, 1; "I1", `Input, 1;
              "I2", `Input, 1; "I3", `Input, 1; "I4", `Input, 1; "I5", `Input, 1 ];
  "FDRE",   [ "Q", `Output, 1; "C", `Input, 1; "CE", `Input, 1;
              "D", `Input, 1; "R", `Input, 1 ];
  "FDSE",   [ "Q", `Output, 1; "C", `Input, 1; "CE", `Input, 1;
              "D", `Input, 1; "S", `Input, 1 ];
  "FDCE",   [ "Q", `Output, 1; "C", `Input, 1; "CE", `Input, 1;
              "D", `Input, 1; "CLR", `Input, 1 ];
  "FDPE",   [ "Q", `Output, 1; "C", `Input, 1; "CE", `Input, 1;
              "D", `Input, 1; "PRE", `Input, 1 ];
  "CARRY4", [ "CO", `Output, 4; "O", `Output, 4;
              "CI", `Input, 1; "CYINIT", `Input, 1;
              "DI", `Input, 4; "S", `Input, 4 ];
  "BUFG",   [ "O", `Output, 1; "I", `Input, 1 ];
  "IBUF",   [ "O", `Output, 1; "I", `Input, 1 ];
  "OBUF",   [ "O", `Output, 1; "I", `Input, 1 ];
  "IBUFDS", [ "O", `Output, 1; "I", `Input, 1; "IB", `Input, 1 ];
  "OBUFDS", [ "O", `Output, 1; "OB", `Output, 1; "I", `Input, 1 ];
]

(* -------------- emission -------------- *)

let write_verilog
    ~(library_cells : (string * library_port list) list)
    ~(path : string) (m : bmodule) : unit =

  let ctx = mk_ctx () in
  populate_widths ctx m;

  let primtype_ports t =
    match List.assoc_opt t library_cells with
    | Some ports ->
        List.map (fun (p : library_port) ->
          p.port_name, p.port_direction, p.port_width) ports
    | None ->
        (match List.assoc_opt t xil_primitive_ports with
         | Some lst -> lst
         | None     -> [])
  in

  (* Pre-allocate IDs for top-level ports so the wire IDs match between
   * EDIF and Verilog views. *)
  let port_bits : (string, [`Input|`Output|`Internal] * int * int list) Hashtbl.t =
    Hashtbl.create 32 in
  List.iter (fun (s : bsignal) ->
    if s.direction <> `Internal then begin
      let w = try Hashtbl.find ctx.widths s.name with Not_found -> 1 in
      let bs = List.init w (fun i -> alloc ctx { base = s.name; bit = i }) in
      Hashtbl.add port_bits s.name (s.direction, w, bs)
    end) m.signals;

  (* First pass: resolve each instance's pins so we know every net id that
   * will appear in the body before emitting wire declarations. *)
  let is_skipped_cell = function "GND" | "VCC" -> true | _ -> false in
  let live_cells = List.filter (fun (i : binstance) ->
    not (is_skipped_cell i.module_name)) m.instances in

  let net_ids : (string, unit) Hashtbl.t = Hashtbl.create 1024 in
  let inst_pin_nets =
    List.map (fun (i : binstance) ->
      let ports = primtype_ports i.module_name in
      let pin_nets =
        List.map (fun (pin, expr) ->
          let nets = nets_of_conn ctx expr in
          let pin_w = match List.find_opt (fun (p, _, _) -> p = pin) ports with
            | Some (_, _, w) -> w
            | None           -> List.length nets in
          List.iter (fun n -> Hashtbl.replace net_ids n ()) nets;
          (pin, pin_w, nets))
          i.port_connections in
      i, pin_nets
    ) live_cells in

  (* port-net set: nets that are part of a top-level port's bit list — we
   * tie those to the port name via assign, not via wire decl. *)
  let port_nets : (string, unit) Hashtbl.t = Hashtbl.create 128 in
  Hashtbl.iter (fun _ (_, _, bits) ->
    List.iter (fun id -> Hashtbl.replace port_nets (net_name_of_id id) ())
      bits) port_bits;

  (* -------------- write the file -------------- *)
  let buf = Buffer.create (256 * 1024) in
  let pp fmt = Printf.ksprintf (Buffer.add_string buf) fmt in
  let top_name = v_safe_id m.name in

  pp "// Generated by bir_to_verilog_netlist — gate-level structural.\n";
  pp "// Sibling output of write_netlist_edif / write_nextpnr_json.\n";
  pp "// xsim sanity:  xvlog -L unisims_ver %s.v && xelab -L unisims_ver %s\n\n"
     top_name top_name;

  pp "module %s (\n" top_name;
  let port_list = Hashtbl.fold (fun nm (dir, w, _) acc -> (nm, dir, w) :: acc)
                    port_bits [] in
  let port_list = List.sort compare port_list in
  let print_port_decl (nm, dir, w) =
    let kw = match dir with
      | `Input  -> "input"
      | `Output -> "output"
      | _       -> "input"
    in
    if w = 1 then Printf.sprintf "  %s %s" kw (v_safe_id nm)
    else Printf.sprintf "  %s [%d:0] %s" kw (w - 1) (v_safe_id nm)
  in
  pp "%s\n);\n\n" (String.concat ",\n" (List.map print_port_decl port_list));

  pp "  wire n_VCC = 1'b1;\n";
  pp "  wire n_GND = 1'b0;\n";

  (* Declare wires for EVERY net id used anywhere in the body — both nets
   * referenced by instance pins (net_ids) and nets that bridge a top-
   * level port to its constituent bits (port_nets).  Skipping port_nets
   * caused xvlog to flag undeclared net symbols on the assign LHS. *)
  let all_nets : (string, unit) Hashtbl.t = Hashtbl.create 1024 in
  Hashtbl.iter (fun n () -> Hashtbl.replace all_nets n ()) net_ids;
  Hashtbl.iter (fun n () -> Hashtbl.replace all_nets n ()) port_nets;
  let wire_ids =
    Hashtbl.fold (fun n () acc ->
      if n = "n_VCC" || n = "n_GND" then acc else n :: acc)
      all_nets [] in
  let wire_ids = List.sort compare wire_ids in
  List.iter (fun w -> pp "  wire %s;\n" w) wire_ids;
  pp "\n";

  (* Tie top-level ports to their per-bit net IDs.
   * Output ports: driven by instance pins, so port = {nets MSB-first}.
   * Input ports : nets are aliases of the port bits.                  *)
  Hashtbl.iter (fun nm (dir, w, bits) ->
    let nets_msb_first = List.rev (List.map net_name_of_id bits) in
    let bus = if w = 1 then List.hd nets_msb_first
              else Printf.sprintf "{%s}" (String.concat ", " nets_msb_first) in
    let id = v_safe_id nm in
    match dir with
    | `Output -> pp "  assign %s = %s;\n" id bus
    | `Input  -> pp "  assign %s = %s;\n" bus id
    | _ -> ()
  ) port_bits;
  pp "\n";

  (* Instance bodies. *)
  List.iter (fun ((i : binstance), pin_nets) ->
    let inst_id = v_safe_id i.inst_name in
    let cell_id = v_safe_id i.module_name in
    let params =
      List.map (fun (k, v) -> Printf.sprintf ".%s(%s)" k (v_param_value v))
        i.param_strs
      @ List.map (fun (k, n) -> Printf.sprintf ".%s(%d)" k n)
        i.param_values
    in
    if params <> [] then begin
      pp "  %s #(\n      %s\n  ) %s (\n"
        cell_id (String.concat ",\n      " params) inst_id
    end else
      pp "  %s %s (\n" cell_id inst_id;
    let pin_lines =
      List.map (fun (pin, _pin_w, nets) ->
        let pin_id = v_safe_id pin in
        let conn =
          match nets with
          | [n] -> n
          | _   -> Printf.sprintf "{%s}" (String.concat ", " (List.rev nets))
        in
        Printf.sprintf "    .%s(%s)" pin_id conn)
        pin_nets in
    pp "%s\n  );\n\n" (String.concat ",\n" pin_lines)
  ) inst_pin_nets;

  pp "endmodule\n";

  let oc = open_out path in
  Fun.protect ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc (Buffer.contents buf))
