(* Direct BIR → EDIF 2.0.0 emitter.
 *
 * Mirror of bir_to_nextpnr_json for the EDIF target — same library_cells
 * input, same library-cell port-direction resolution, same constant
 * sentinel handling (BVar "VCC" → n_VCC, BVar "GND" → n_GND, BConst
 * values map bit-by-bit) — but emits EDIF that Vivado's read_edif
 * consumes directly, with INIT parameters preserved as Vivado-format
 * Verilog literals.
 *
 * Replaces the yosys-bridged JSON → EDIF path that mangled LUT INIT
 * values into wrong integers (yosys 0.64's write_edif read INIT as a
 * string and emitted incorrect integer-encoded property values; the
 * same instances came out with different INITs in EDIF than in
 * yosys's own write_verilog output).
 *)

open Behavioral_ir

(* -------------- EDIF identifier safety -------------- *)

let edif_safe_char c =
  let cc = Char.code c in
  (cc >= Char.code 'a' && cc <= Char.code 'z')
  || (cc >= Char.code 'A' && cc <= Char.code 'Z')
  || (cc >= Char.code '0' && cc <= Char.code '9')
  || c = '_'

let edif_safe_id (s : string) : string =
  let n = String.length s in
  let buf = Buffer.create n in
  for i = 0 to n - 1 do
    let c = s.[i] in
    if edif_safe_char c then Buffer.add_char buf c
    else Buffer.add_char buf '_'
  done;
  let r = Buffer.contents buf in
  if String.length r > 0
     && (let c0 = r.[0] in
         c0 >= '0' && c0 <= '9')
  then "id_" ^ r
  else r

(* -------------- net-name allocation -------------- *)

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

(* Special sentinel nets for constants — same names fpga_emit uses. *)
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

(* Resolve a port_connection bexpr to its list of net names, LSB first.
 * Constants land on n_VCC / n_GND, single-bit refs on alloc'd net ids. *)
let rec nets_of_conn ctx (e : bexpr) : string list =
  match e with
  | BConst { value; width } ->
      let rec range i = if i >= width then [] else
        let b = (value lsr i) land 1 in
        (if b = 1 then "n_VCC" else "n_GND") :: range (i + 1) in
      range 0
  | BConcat es ->
      (* BIR's BConcat is MSB-first; reverse to LSB-first. *)
      List.concat_map (nets_of_conn ctx) (List.rev es)
  | BSlice { signal; msb; lsb } ->
      let base = match signal with
        | BVar nm -> nm
        | _ -> failwith ("bir_to_edif: slice of non-BVar: "
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
       | None   -> [net_name_of_id (alloc ctx { base = nm; bit = value })])
  | _ ->
      failwith ("bir_to_edif: unsupported pin expression: "
                ^ Behavioral_ir.string_of_bexpr e)

(* -------------- INIT and parameter encoding -------------- *)

(* Convert a value that may be a raw binary string (`0000000100000000`)
 * or already a Verilog literal (`16'h3202`, `2'b10`, `"TRUE"`) to a
 * Vivado-friendly EDIF property string.  Binary strings get wrapped
 * with width:`N'bBITS`; existing Verilog literals pass through; bare
 * enums (no quotes, no digits-then-tick) pass through as strings.   *)
let edif_property_value (v : string) : string =
  let n = String.length v in
  if n = 0 then "\"\""
  else if String.for_all (fun c -> c = '0' || c = '1') v then
    Printf.sprintf "\"%d'b%s\"" n v
  else
    Printf.sprintf "\"%s\"" v

(* -------------- emission -------------- *)

let write_edif
    ~(library_cells : (string * library_port list) list)
    ~(path : string) (m : bmodule) : unit =
  let ctx = mk_ctx () in
  populate_widths ctx m;

  (* Pre-allocate net ids for top-level ports. *)
  let port_bits : (string, [`Input|`Output|`Internal] * int * int list) Hashtbl.t =
    Hashtbl.create 32 in
  List.iter (fun (s : bsignal) ->
    if s.direction <> `Internal then begin
      let w = try Hashtbl.find ctx.widths s.name with Not_found -> 1 in
      let bs = List.init w (fun i -> alloc ctx { base = s.name; bit = i }) in
      Hashtbl.add port_bits s.name (s.direction, w, bs)
    end) m.signals;

  (* Collect every primitive cell-type the design uses.  Library
   * declaration covers each one's port interface for Vivado's
   * read_edif. *)
  let used_types : (string, unit) Hashtbl.t = Hashtbl.create 32 in
  List.iter (fun (i : binstance) ->
    Hashtbl.replace used_types i.module_name ()) m.instances;

  (* Hardcoded per-type port lists for primitives we don't otherwise
   * have library_cells for (so the EDIF library declaration is
   * complete enough for Vivado to link).  Source: openXC7 primitives
   * list, mirrors bir_to_nextpnr_json.xil_primitive_ports. *)
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
  ] in

  let primtype_ports t : (string * [`Input|`Output] * int) list =
    match List.assoc_opt t library_cells with
    | Some ports ->
        List.map (fun (p : library_port) ->
          p.port_name, p.port_direction, p.port_width) ports
    | None ->
        match List.assoc_opt t xil_primitive_ports with
        | Some lst -> lst
        | None -> []
  in

  let buf = Buffer.create (256 * 1024) in
  let pp fmt = Printf.ksprintf (Buffer.add_string buf) fmt in
  let top_name = m.name in
  let now = Unix.gmtime (Unix.time ()) in

  pp "(edif %s\n" (edif_safe_id top_name);
  pp "  (edifversion 2 0 0)\n";
  pp "  (edifLevel 0)\n";
  pp "  (keywordmap (keywordlevel 0))\n";
  pp "  (status (written (timeStamp %d %d %d %d %d %d)\n      (program \"bir_to_edif\")))\n"
    (now.tm_year + 1900) (now.tm_mon + 1) now.tm_mday
    now.tm_hour now.tm_min now.tm_sec;

  (* --- hdi_primitives library --- *)
  pp "  (Library hdi_primitives\n";
  pp "    (edifLevel 0)\n";
  pp "    (technology (numberDefinition))\n";
  pp "    (cell GND (celltype GENERIC) (view netlist (viewtype NETLIST) (interface (port G (direction OUTPUT)))))\n";
  pp "    (cell VCC (celltype GENERIC) (view netlist (viewtype NETLIST) (interface (port P (direction OUTPUT)))))\n";
  Hashtbl.iter (fun ty () ->
    if ty = "GND" || ty = "VCC" then () else begin
      let ports = primtype_ports ty in
      pp "    (cell %s (celltype GENERIC)\n" (edif_safe_id ty);
      pp "      (view netlist (viewtype NETLIST)\n";
      pp "        (interface\n";
      List.iter (fun (pname, dir, w) ->
        let dir_s = match dir with `Input -> "INPUT" | `Output -> "OUTPUT" in
        if w = 1 then
          pp "          (port %s (direction %s))\n" (edif_safe_id pname) dir_s
        else
          pp "          (port (array (rename %s \"%s[%d:0]\") %d) (direction %s))\n"
            (edif_safe_id pname) pname (w - 1) w dir_s
      ) ports;
      pp "        )))\n"
    end) used_types;
  pp "  )\n";

  (* --- work library: top cell --- *)
  pp "  (Library work\n";
  pp "    (edifLevel 0)\n";
  pp "    (technology (numberDefinition))\n";
  pp "    (cell %s (celltype GENERIC)\n" (edif_safe_id top_name);
  pp "      (view netlist (viewtype NETLIST)\n";
  pp "        (interface\n";
  Hashtbl.iter (fun nm (dir, w, _bits) ->
    if dir <> `Internal then begin
      let dir_s = match dir with `Input -> "INPUT" | `Output -> "OUTPUT" | _ -> "INPUT" in
      if w = 1 then
        pp "          (port %s (direction %s))\n" (edif_safe_id nm) dir_s
      else
        pp "          (port (array (rename %s \"%s[%d:0]\") %d) (direction %s))\n"
          (edif_safe_id nm) nm (w - 1) w dir_s
    end) port_bits;
  pp "        )\n";

  pp "        (contents\n";
  pp "          (instance n_GND_inst (viewref netlist (cellref GND (libraryref hdi_primitives))))\n";
  pp "          (instance n_VCC_inst (viewref netlist (cellref VCC (libraryref hdi_primitives))))\n";

  (* Emit each cell instance with its INIT/property parameters. *)
  let is_skipped_cell = function "GND" | "VCC" -> true | _ -> false in
  let live_cells = List.filter (fun (i : binstance) ->
    not (is_skipped_cell i.module_name)) m.instances in
  List.iter (fun (i : binstance) ->
    pp "          (instance %s (viewref netlist (cellref %s (libraryref hdi_primitives)))"
      (edif_safe_id i.inst_name) (edif_safe_id i.module_name);
    List.iter (fun (k, v) ->
      pp "\n            (property %s (string %s))" k (edif_property_value v)
    ) i.param_strs;
    List.iter (fun (k, n) ->
      pp "\n            (property %s (integer %d))" k n
    ) i.param_values;
    pp "\n          )\n"
  ) live_cells;

  (* Collect every port reference per net.  Vivado/Xilinx EDIF
   * convention: for an array port renamed as "led[7:0]" (descending),
   * `(member led 0)` is led[7] (MSB).  Our bits arrays are LSB-first,
   * so we reverse the member index when emitting. *)
  let net_uses : (string, string list) Hashtbl.t = Hashtbl.create 1024 in
  let add_use net pref =
    let cur = try Hashtbl.find net_uses net with Not_found -> [] in
    Hashtbl.replace net_uses net (pref :: cur)
  in
  let mem_idx ~w i = w - 1 - i in

  (* Per-instance pin references. *)
  List.iter (fun (i : binstance) ->
    let inst_id = edif_safe_id i.inst_name in
    let ports = primtype_ports i.module_name in
    List.iter (fun (pin, expr) ->
      let nets = nets_of_conn ctx expr in
      let pin_w = match List.find_opt (fun (p, _, _) -> p = pin) ports with
        | Some (_, _, w) -> w
        | None -> 1 in
      List.iteri (fun bit_i net ->
        let pref =
          if pin_w = 1 then
            Printf.sprintf "(portref %s (instanceref %s))"
              (edif_safe_id pin) inst_id
          else
            Printf.sprintf "(portref (member %s %d) (instanceref %s))"
              (edif_safe_id pin) (mem_idx ~w:pin_w bit_i) inst_id
        in
        add_use net pref) nets
    ) i.port_connections) live_cells;

  (* Constant tie drivers. *)
  add_use "n_GND" "(portref G (instanceref n_GND_inst))";
  add_use "n_VCC" "(portref P (instanceref n_VCC_inst))";

  (* Top-level port references. *)
  Hashtbl.iter (fun nm (_dir, w, bits) ->
    List.iteri (fun bit_i id ->
      let net = net_name_of_id id in
      let pref =
        if w = 1 then Printf.sprintf "(portref %s)" (edif_safe_id nm)
        else Printf.sprintf "(portref (member %s %d))"
               (edif_safe_id nm) (mem_idx ~w bit_i)
      in
      add_use net pref) bits) port_bits;

  Hashtbl.iter (fun net uses ->
    pp "          (net %s\n            (joined" net;
    List.iter (fun u -> pp "\n              %s" u) uses;
    pp ")\n          )\n") net_uses;

  pp "        )\n";   (* end contents *)
  pp "      )\n";     (* end view *)
  pp "    )\n";       (* end cell *)
  pp "  )\n";         (* end Library work *)
  pp "  (Design %s (cellref %s (libraryref work)))\n"
    (edif_safe_id top_name) (edif_safe_id top_name);
  pp ")\n";

  let oc = open_out path in
  Fun.protect ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc (Buffer.contents buf))
