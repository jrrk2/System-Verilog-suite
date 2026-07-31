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

(* Authoritative Xilinx primitive port interfaces, dumped from Vivado
   (get_lib_pins) as JSON: { "CELLTYPE": [ {"name","dir","width"}, ... ] }.
   Used as the top-priority source for primitive cell interfaces so hard
   primitives (GTXE2_CHANNEL etc.) declare their EXACT bus widths/directions
   and link as real architecture primitives instead of black boxes.  Path via
   env XIL_PRIM_PORTS_JSON, default xilinx_lef/xil_primitive_ports.json. *)
let xil_json_ports : (string, (string * [`Input|`Output] * int) list) Hashtbl.t Lazy.t =
  lazy (
    let tbl = Hashtbl.create 64 in
    (* The default USED to be a hardcoded $HOME/System-Verilog-suite path.  That
       directory does not exist when the suite is vendored as a submodule
       (deps/System-Verilog-suite), so the oracle silently loaded NOTHING and
       every Xilinx hard primitive lost its port list -- which degrades quietly
       rather than failing: box_ports returns [], the canonical ufo__ cut is
       skipped, and miter boundaries around MMCM/GT/BRAM stop pairing across
       flows.  Resolve relative to the running executable first, then the old
       path, and honour XIL_PRIM_PORTS_JSON above both. *)
    let candidates =
      let exe_dir = Filename.dirname Sys.executable_name in
      let rel = Filename.concat "xilinx_lef" "xil_primitive_ports.json" in
      [ (* _build/default/<exe> -> repo root is ../.. *)
        Filename.concat (Filename.concat exe_dir "../..") rel;
        Filename.concat exe_dir rel;
        Filename.concat (Sys.getcwd ()) rel;
        "/home/jonathan/System-Verilog-suite/xilinx_lef/xil_primitive_ports.json" ] in
    let path =
      try Sys.getenv "XIL_PRIM_PORTS_JSON"
      with Not_found ->
        (try List.find Sys.file_exists candidates
         with Not_found -> List.hd candidates) in
    (* BOMB rather than degrade.  A missing oracle is invisible at the call
       sites -- box_ports just returns [], so no Xilinx primitive gets a
       canonical key, none gets the ufo__/ufi__ boundary cut, and every miter
       around an MMCM/GT/BRAM silently compares unpaired free variables and
       manufactures counterexamples.  That cost a whole day of chasing five
       "divergences" that were all this one dead path.  Set
       XIL_PRIM_PORTS_OPTIONAL=1 for a flow that genuinely has no Xilinx
       primitives. *)
    if not (Sys.file_exists path) then begin
      if Sys.getenv_opt "XIL_PRIM_PORTS_OPTIONAL" = None then
        failwith (Printf.sprintf
          "xil_primitive_ports.json NOT FOUND (tried: %s).  Without it every \
           Xilinx hard primitive (MMCME2_ADV, GTXE2_*, RAMB36E1, ...) loses \
           its port list, so miter boundaries around them stop pairing and \
           equivalence results become meaningless.  Set XIL_PRIM_PORTS_JSON \
           to the file, or XIL_PRIM_PORTS_OPTIONAL=1 if this design has no \
           Xilinx primitives."
          (String.concat ", " candidates))
      else
        Printf.eprintf
          "WARNING: xil_primitive_ports.json not found (%s); Xilinx primitive \
           port lists unavailable (XIL_PRIM_PORTS_OPTIONAL=1)\n%!" path
    end;
    (try
       (match Yojson.Safe.from_file path with
        | `Assoc cells ->
            List.iter (fun (cn, ports) ->
              match ports with
              | `List ps ->
                  let plist = List.filter_map (fun p -> match p with
                    | `Assoc f ->
                        let gs k = match List.assoc_opt k f with Some (`String s) -> s | _ -> "" in
                        let gi k = match List.assoc_opt k f with Some (`Int i) -> i | _ -> 1 in
                        let name = gs "name" in
                        if name = "" then None
                        else Some (name, (if gs "dir" = "output" then `Output else `Input), gi "width")
                    | _ -> None) ps in
                  Hashtbl.replace tbl cn plist
              | _ -> ()) cells
        | _ -> ())
     with e ->
       (* Also bomb on a malformed/unreadable oracle: a parse failure here used
          to be swallowed, leaving the same silently-empty table. *)
       if Sys.getenv_opt "XIL_PRIM_PORTS_OPTIONAL" = None then
         failwith (Printf.sprintf
           "xil_primitive_ports.json at %s failed to parse: %s"
           path (Printexc.to_string e)));
    if Hashtbl.length tbl = 0 && Sys.getenv_opt "XIL_PRIM_PORTS_OPTIONAL" = None
    then failwith (Printf.sprintf
      "xil_primitive_ports.json at %s parsed to ZERO cells -- the oracle is \
       empty, which silently disables every primitive port lookup." path);
    tbl)

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
  widths    : (string, int) Hashtbl.t;   (* declared ∪ slice-inferred: BVar expansion *)
  declw     : (string, int) Hashtbl.t;   (* declared only: bitbus_ref strip gate *)
}

let mk_ctx () = {
  ids     = Hashtbl.create 4096;
  next_id = ref 100;
  widths  = Hashtbl.create 256;
  declw   = Hashtbl.create 256;
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
    (* Use the canonical width so an array/struct-typed signal keeps its full
       width — a `_ -> 1` fallback here silently collapsed buses to one net and
       dropped the upper bits' pin connections. *)
    let w = width_of_btype s.stype in
    Hashtbl.replace ctx.widths s.name w;
    Hashtbl.replace ctx.declw s.name w) m.signals

(* Resolve a port_connection bexpr to its list of net names, LSB first.
 * Constants land on n_VCC / n_GND, single-bit refs on alloc'd net ids. *)
(* `<base>__<i>` (of_circuit multi-bit-input memo naming) -> bit i of the
   vector `<base>` (a declared multi-bit signal), so per-bit input references
   connect to the port bit rather than an orphan scalar net. *)
let bitbus_ref ctx nm =
  (* a name that is ITSELF a declared signal (Vivado's next-state twin
     `wr_addr__0`, a distinct wire, EVEN when width 1) is a real net, not a
     `<base>__<i>` bit memo — stripping it aliases it onto <base>'s bit ids
     (multiply-driven).  Only UNDECLARED synthetic `__<i>` names get stripped;
     declared names expand by their own width downstream. *)
  if Hashtbl.mem ctx.widths nm then None else
  let n = String.length nm in
  let rec find i =
    if i < 1 then None
    else if nm.[i] = '_' && nm.[i - 1] = '_' then Some i else find (i - 1) in
  match find (n - 1) with
  | Some j when j + 1 < n ->
      let suf = String.sub nm (j + 1) (n - j - 1) in
      let base = String.sub nm 0 (j - 1) in
      (* gate on DECLARED width only: an inference-bumped base must not
         start hijacking distinct signals that merely share its prefix
         (Vivado's next-state `wr_addr__0` vs bus `wr_addr`) *)
      if suf <> "" && String.for_all (fun c -> c >= '0' && c <= '9') suf
         && (match Hashtbl.find_opt ctx.declw base with Some w -> w > 1 | None -> false)
      then Some (base, int_of_string suf) else None
  | _ -> None

let rec nets_of_conn ctx (e : bexpr) : string list =
  match e with
  | BConst { value; width } ->
      let rec range i = if i >= width then [] else
        let b = (if Z.testbit value i then 1 else 0) in
        (if b = 1 then "n_VCC" else "n_GND") :: range (i + 1) in
      range 0
  | BConcat es ->
      (* BIR's BConcat is MSB-first; reverse to LSB-first. *)
      List.concat_map (nets_of_conn ctx) (List.rev es)
  | BSlice { signal = BVar base; msb; lsb } ->
      let rec range lo hi = if lo > hi then [] else lo :: range (lo + 1) hi in
      List.map (fun i ->
        match const_net base with
        | Some n -> n
        | None   -> net_name_of_id (alloc ctx { base; bit = i }))
        (range lsb msb)
  | BSlice { signal; msb; lsb } ->
      (* Slice of a composite (flatten_struct emits a bus as a BConcat of nets,
         then slices it): expand the inner signal to its LSB-first net list and
         take [lsb..msb]; out-of-range bits read x -> tie GND (yosys parity). *)
      let full = Array.of_list (nets_of_conn ctx signal) in
      let n = Array.length full in
      let rec range lo hi = if lo > hi then [] else lo :: range (lo + 1) hi in
      List.map (fun i -> if i >= 0 && i < n then full.(i) else "n_GND")
        (range lsb msb)
  | BVar nm ->
      (match const_net nm with
       | Some n -> [n]
       | None ->
           (match bitbus_ref ctx nm with
            | Some (base, bit) ->
                (* of_circuit's `<base>__<i>` multi-bit-input naming -> bit i of
                   the vector `<base>`, else the reader orphans from the port
                   bit (driverless net). *)
                [net_name_of_id (alloc ctx { base; bit })]
            | None ->
                let w = try Hashtbl.find ctx.widths nm with Not_found -> 1 in
                List.init w (fun i -> net_name_of_id (alloc ctx { base = nm; bit = i }))))
  | BSelect { array = BVar nm; index = BConst { value; _ } } ->
      (match const_net nm with
       | Some n -> [n]
       | None   -> [net_name_of_id (alloc ctx { base = nm; bit = Z.to_int value })])
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
  (* string params now flow quote-normalised ("LVDS") from the elaborate
     fix — strip the quotes here or they'd double up in the EDIF *)
  let v =
    let n = String.length v in
    if n >= 2 && v.[0] = '"' && v.[n - 1] = '"'
    then String.sub v 1 (n - 2) else v in
  let n = String.length v in
  (* Defensive: a based literal that lost its digits ("1'b", "8'h", …)
     would otherwise be emitted verbatim and Vivado would silently read
     the INIT as 0 — the class of bug that killed PCS one-hot FSM init.
     Refuse to emit a valueless literal; fail loudly at synthesis time. *)
  (match String.index_opt v '\'' with
   | Some i when i + 2 = n
                 && (match Char.lowercase_ascii v.[i + 1] with
                     | 'b' | 'h' | 'o' | 'd' -> true | _ -> false) ->
       failwith (Printf.sprintf
         "edif_property_value: malformed based literal %S (base marker with \
          no digits) — INIT/parameter truncated upstream" v)
   | _ -> ());
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
  (* Width inference from slice evidence: a bus signal whose DECLARATION was
     lost through gate_map/flatten still appears with explicit BSlice/BSelect
     bounds at reader pins.  A whole-bus BVar at another pin (e.g. a blackbox
     GT output) would otherwise expand to width 1, silently dropping the
     remaining bits' connections.  Bump widths to cover every sliced bit. *)
  let port_names : (string, unit) Hashtbl.t = Hashtbl.create 32 in
  List.iter (fun (s : bsignal) ->
    if s.direction <> `Internal then Hashtbl.replace port_names s.name ()) m.signals;
  let bump base w =
    if not (Hashtbl.mem port_names base) then begin
      let cur = try Hashtbl.find ctx.widths base with Not_found -> 1 in
      if w > cur then Hashtbl.replace ctx.widths base w
    end in
  let rec infer = function
    | BSlice { signal = BVar base; msb; lsb } ->
        bump base (max msb lsb + 1)
    | BSlice { signal; _ } -> infer signal
    | BSelect { array = BVar base; index = BConst { value; _ } } ->
        bump base (Z.to_int value + 1)
    | BConcat es -> List.iter infer es
    | _ -> () in
  List.iter (fun (i : binstance) ->
    List.iter (fun (_, e) -> infer e) i.port_connections) m.instances;

  (* Pre-allocate net ids for top-level ports. *)
  let port_bits : (string, [`Input|`Output|`Internal] * int * int list) Hashtbl.t =
    Hashtbl.create 32 in
  (* `__keep_<net>` outputs are synthetic FF-clock retention handles from
     of_circuit; they must NOT surface as top-level IO ports (Vivado NSTD-1/
     UCIO-1 on an unconstrained pad).  Same strip as bir_to_nextpnr_json. *)
  let is_keep_port nm =
    String.length nm >= 7 && String.sub nm 0 7 = "__keep_" in
  List.iter (fun (s : bsignal) ->
    if s.direction <> `Internal && not (is_keep_port s.name) then begin
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

  (* Ports actually connected by instances of each primitive type — the EDIF
     interface MUST declare every one or Vivado's read_edif errors ("Cannot
     find port X on instance of cell Y"). *)
  let connected_ports : (string, (string, int) Hashtbl.t) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun (i : binstance) ->
    let h = match Hashtbl.find_opt connected_ports i.module_name with
      | Some h -> h | None -> let h = Hashtbl.create 16 in Hashtbl.add connected_ports i.module_name h; h in
    List.iter (fun (pn, e) ->
      let w = List.length (nets_of_conn ctx e) in
      let prev = try Hashtbl.find h pn with Not_found -> 0 in
      Hashtbl.replace h pn (max prev (max 1 w))) i.port_connections) m.instances;

  let primtype_ports t : (string * [`Input|`Output] * int) list =
    (* authoritative direction/width source, best first *)
    let from_lib =
      match List.assoc_opt t library_cells with
      | Some ports when ports <> [] ->
          Some (List.map (fun (p : library_port) ->
            p.port_name, p.port_direction, p.port_width) ports)
      | _ -> None in
    let from_hard = List.assoc_opt t xil_primitive_ports in
    let from_vhdl () =
      match (try Vhdl_to_behavioral.lookup_xil_primitive_ports [t] with _ -> []) with
      | (_, ports) :: _ ->
          List.map (fun (p : library_port) ->
            p.port_name, p.port_direction, p.port_width) ports
      | [] -> [] in
    (* Authoritative declared interface: the Vivado xil_primitive_ports JSON,
       else the primitive's VHD entity (via library_cells).  Both give the EXACT
       bus widths -- use them without bumping to the connected width: a 32-bit
       const bound to a narrow GT pin (C_GT_LOOPBACK -> LOOPBACK[2:0]) must NOT
       widen the pin to LOOPBACK[31:0] (Vivado then rejects the invalid pins and
       black-boxes the whole GTXE2_CHANNEL -> undriven CPLL_RESET LUTs).  The
       downstream per-pin clamp truncates the over-wide connection instead. *)
    let authoritative =
      match Hashtbl.find_opt (Lazy.force xil_json_ports) t with
      | Some l when l <> [] -> Some l
      | _ -> from_lib in
    let union_extra known =
      match Hashtbl.find_opt connected_ports t with
      | None -> []
      | Some h ->
          Hashtbl.fold (fun pn w acc ->
            if List.mem pn known then acc else (pn, `Input, w) :: acc) h [] in
    match authoritative with
    | Some l -> l @ union_extra (List.map (fun (n, _, _) -> n) l)
    | None ->
        (* Truly unknown primitive: hardcoded table / VHDL-direct, bumped to the
           connected width and unioned with any connected port they omit, so the
           interface is complete enough to link. *)
        let base0 = match from_hard with Some l -> l | None -> from_vhdl () in
        let conn_w pn =
          match Hashtbl.find_opt connected_ports t with
          | Some h -> (try Hashtbl.find h pn with Not_found -> 0)
          | None -> 0 in
        let base = List.map (fun (n, d, w) -> (n, d, max w (conn_w n))) base0 in
        base @ union_extra (List.map (fun (n, _, _) -> n) base)
  in

  (* ─── ELECTRICAL RULE CHECK ───────────────────────────────────────────
     Every net read by a cell INPUT pin must be DRIVEN by a cell OUTPUT pin,
     a top-level INPUT port, or a constant.  A floating input means a driver
     was silently dropped (a gate_map connectivity bug or a lost cell) — the
     kind of fault Vivado only catches at opt_design, or worse silently trims
     into a dead board.  Fail here naming the net + its readers.  SVS_ERC=0
     disables (bit-level buses are approximated at net granularity for now).
     ERC_AUTOFIX=1 instead ties each undriven input net to n_GND (logged) so a
     debug bitstream still builds past Vivado opt_design — the floaters become
     deterministic 0s rather than opt_design errors, letting you test the rest
     of the design on silicon while the real dropped-driver bug is chased. *)
  let autofix_gnd : (string, unit) Hashtbl.t = Hashtbl.create 64 in
  if Sys.getenv_opt "SVS_ERC" <> Some "0" then begin
    (* BIT-LEVEL: resolve every pin connection through the SAME per-bit net-id
       allocator the emission uses (nets_of_conn), so a bus whose width
       declaration was lost (pin expands to 1 net while sinks read orphan
       `__k` singletons) is caught here instead of at Vivado opt_design.
       ctx.ids is shared with the emission below, so ids agree. *)
    let driven : (string, string list) Hashtbl.t = Hashtbl.create 4096 in
    let readers : (string, string list) Hashtbl.t = Hashtbl.create 4096 in
    let add_driver net who =
      let prev = try Hashtbl.find driven net with Not_found -> [] in
      Hashtbl.replace driven net (who :: prev) in
    List.iter (fun c -> add_driver c "tie") ["n_GND"; "n_VCC"];
    Hashtbl.iter (fun nm (dir, _, bits) ->
      if dir = `Input || is_keep_port nm then
        List.iter (fun id -> add_driver (net_name_of_id id) ("port:" ^ nm)) bits)
      port_bits;
    let conn_nets (i : binstance) (pin, e) =
      (* mirror the emission's clamp of the connection to the declared width *)
      let ports = primtype_ports i.module_name in
      let pin_w = match List.find_opt (fun (p, _, _) -> p = pin) ports with
        | Some (_, _, w) -> w | None -> 1 in
      let all = nets_of_conn ctx e in
      (if List.length all > pin_w then List.filteri (fun k _ -> k < pin_w) all
       else all),
      (match List.find_opt (fun (p, _, _) -> p = pin) ports with
       | Some (_, d, _) -> d | None -> `Input) in
    let all_conns : (string, string list) Hashtbl.t = Hashtbl.create 4096 in
    let diag_on = Sys.getenv_opt "ERC_DIAG" <> None in
    List.iter (fun (i : binstance) ->
      List.iter (fun (pin, e) ->
        let ns, dir = conn_nets i (pin, e) in
        (if diag_on then
           let d = match dir with `Output -> "O" | `Input -> "I" in
           List.iter (fun n ->
             let tag = Printf.sprintf "%s.%s[%s:%s]" i.module_name pin d
                 (Behavioral_ir.string_of_bexpr e) in
             Hashtbl.replace all_conns n (tag :: (try Hashtbl.find all_conns n with Not_found -> []))) ns);
        match dir with
        | `Output ->
            (* mirror the emission's skip of const-folded outputs *)
            List.iter (fun n ->
              if n <> "n_GND" && n <> "n_VCC" then
                add_driver n (i.inst_name ^ "." ^ pin)) ns
        | `Input -> List.iter (fun n ->
            let prev = try Hashtbl.find readers n with Not_found -> [] in
            Hashtbl.replace readers n ((i.inst_name ^ "." ^ pin) :: prev)) ns
      ) i.port_connections) m.instances;
    (* top-level OUTPUT port bits are readers too: an undriven one reaches
       silicon as a floating pad / constant-tied status bit *)
    Hashtbl.iter (fun nm (dir, _, bits) ->
      if dir = `Output && not (is_keep_port nm) then
        List.iteri (fun k id ->
          let n = net_name_of_id id in
          let prev = try Hashtbl.find readers n with Not_found -> [] in
          Hashtbl.replace readers n (Printf.sprintf "port:%s[%d]" nm k :: prev))
          bits) port_bits;
    (* reverse map for diagnosability: net id -> symbolic base[bit] *)
    let sym : (string, string) Hashtbl.t = Hashtbl.create 4096 in
    Hashtbl.iter (fun (k : net_key) id ->
      Hashtbl.replace sym (net_name_of_id id)
        (Printf.sprintf "%s[%d]" k.base k.bit)) ctx.ids;
    let undriven =
      Hashtbl.fold (fun n rs acc ->
        if Hashtbl.mem driven n then acc
        else
          let label = match Hashtbl.find_opt sym n with
            | Some s -> s | None -> n in
          let base = match String.index_opt label '[' with
            | Some i -> String.sub label 0 i | None -> label in
          if is_keep_port base then acc else (label, rs) :: acc)
        readers [] in
    let multi =
      Hashtbl.fold (fun n ds acc ->
        match ds with
        | _ :: _ :: _ ->
            let label = match Hashtbl.find_opt sym n with
              | Some s -> s | None -> n in
            (label, ds) :: acc
        | _ -> acc) driven [] in
    if multi <> [] then begin
      let total = List.length multi in
      let sample = List.filteri (fun i _ -> i < 8) multi in
      let body = String.concat "\n  " (List.map (fun (net, ds) ->
        Printf.sprintf "%S driven by [%s]" net
          (String.concat "," (List.filteri (fun i _ -> i < 4) ds))) sample) in
      failwith (Printf.sprintf
        "bir_to_edif ERC: %d net(s) have MULTIPLE drivers (aliased net ids \
         / a bitbus collision):\n  %s%s"
        total body
        (if total > 8 then Printf.sprintf "\n  ... and %d more" (total - 8) else ""))
    end;
    if undriven <> [] then begin
      (* DIAGNOSTIC: for each undriven net's base, show the driven/read status of
         every allocated bit of that base — reveals whether the floater is a
         bit-aliasing (base driven at other bits) or a base-naming split. *)
      (match Sys.getenv_opt "ERC_DIAG" with
       | None -> ()
       | Some path ->
         let oc = open_out path in
         (* reverse net-name -> id map for reporting neighbour bits by base *)
         let name_of : (string, net_key) Hashtbl.t = Hashtbl.create 4096 in
         Hashtbl.iter (fun (k : net_key) id -> Hashtbl.replace name_of (net_name_of_id id) k) ctx.ids;
         Hashtbl.iter (fun n rs ->
           if not (Hashtbl.mem driven n) then begin
             let label = match Hashtbl.find_opt sym n with Some s -> s | None -> n in
             Printf.fprintf oc "UNDRIVEN %s (id %s)\n" label n;
             Printf.fprintf oc "  readers: %s\n"
               (String.concat ", " (List.filteri (fun i _ -> i < 4) rs));
             let conns = try Hashtbl.find all_conns n with Not_found -> [] in
             Printf.fprintf oc "  ALL conns on this id: %s\n"
               (String.concat " | " conns);
             ignore name_of
           end) readers;
         close_out oc;
         Printf.eprintf "[ERC_DIAG] wrote %s\n" path);
      let total = List.length undriven in
      let sample = List.filteri (fun i _ -> i < 12) undriven in
      let body = String.concat "\n  " (List.map (fun (net, rs) ->
        Printf.sprintf "%S read by [%s] — NO DRIVER" net
          (String.concat "," (List.filteri (fun i _ -> i < 3) rs))) sample) in
      if Sys.getenv_opt "ERC_AUTOFIX" <> None then begin
        (* record the raw per-bit net ids so emission can alias them to n_GND *)
        Hashtbl.iter (fun n _rs ->
          if not (Hashtbl.mem driven n) then begin
            let label = match Hashtbl.find_opt sym n with Some s -> s | None -> n in
            let base = match String.index_opt label '[' with
              | Some i -> String.sub label 0 i | None -> label in
            if not (is_keep_port base) then Hashtbl.replace autofix_gnd n ()
          end) readers;
        Printf.eprintf
          "[ERC_AUTOFIX] %d undriven input net(s) tied to n_GND (debug build):\n  %s%s\n"
          (Hashtbl.length autofix_gnd) body
          (if total > 12 then Printf.sprintf "\n  ... and %d more" (total - 12) else "")
      end else
      failwith (Printf.sprintf
        "bir_to_edif ERC: %d net(s) feed cell INPUT pins with no driver \
         (floating — a dropped driver / gate_map bug):\n  %s%s"
        total body
        (if total > 12 then Printf.sprintf "\n  ... and %d more" (total - 12) else ""))
    end
  end;

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

  (* write_edif emits UNBUFFERED outputs.  of_circuit inserts a per-output-port
     identity LUT6 buffer (INIT=64'hFFFFFFFF00000000, all six inputs tied) as a
     nextpnr-xilinx OUTMUX workaround; for Vivado that is pure redundancy (it
     disconnects an explicit OBUF from its pad and forces Vivado to re-insert
     IO buffers).  Bypass each such buffer by aliasing its INPUT net to its
     OUTPUT net (so the real driver connects straight through) and dropping it.
     write_nextpnr_json keeps the buffers. *)
  let key_of_conn = function
    | BVar nm ->
        (match const_net nm with Some _ -> None | None ->
         (match bitbus_ref ctx nm with
          | Some (b, i) -> Some { base = b; bit = i }
          | None -> Some { base = nm; bit = 0 }))
    | BSlice { signal = BVar b; msb; lsb } when msb = lsb && const_net b = None ->
        Some { base = b; bit = lsb }
    | BSelect { array = BVar b; index = BConst { value; _ } } when const_net b = None ->
        Some { base = b; bit = Z.to_int value }
    | _ -> None in
  let is_ident_obuf (i : binstance) =
    String.equal i.module_name "LUT6"
    && List.mem ("INIT", "64'hFFFFFFFF00000000") i.param_strs
    && (match List.assoc_opt "I0" i.port_connections with
        | Some i0 ->
            List.for_all (fun p ->
              match List.assoc_opt p i.port_connections with Some e -> e = i0 | None -> false)
              ["I1"; "I2"; "I3"; "I4"; "I5"]
        | None -> false) in
  (* only bypass a buffer whose OUTPUT is a top-level OUTPUT PORT (the one that
     sits between an explicit OBUF and the pad); intermediate buffers on
     internal nets stay (bypassing them disconnects the fabric). *)
  let out_ports : (string, unit) Hashtbl.t = Hashtbl.create 32 in
  List.iter (fun (s : bsignal) ->
    if s.direction = `Output then Hashtbl.replace out_ports s.name ()) m.signals;
  let ident_names : (string, unit) Hashtbl.t = Hashtbl.create 64 in
  (* NET-NAME aliasing (applied at add_use time): dropping a buffer must JOIN
     its output net onto its input net — the old ids-table redirect MOVED the
     input's driver onto the port net instead, leaving the input net (which may
     itself be a port or have other readers: rx_clk≡eth_clk) driverless. *)
  let net_alias : (string, string) Hashtbl.t = Hashtbl.create 64 in
  let rec resolve_alias ?(d = 0) n =
    if d > 32 then n else
    match Hashtbl.find_opt net_alias n with
    | Some n' when n' <> n -> resolve_alias ~d:(d + 1) n'
    | _ -> n in
  List.iter (fun (i : binstance) ->
    if is_ident_obuf i then
      match List.assoc_opt "O" i.port_connections, List.assoc_opt "I0" i.port_connections with
      | Some o, Some i0 ->
          (match key_of_conn o, key_of_conn i0 with
           | Some ok, Some ik when ok <> ik && Hashtbl.mem out_ports ok.base ->
               Hashtbl.replace ident_names i.inst_name ();
               Hashtbl.replace net_alias
                 (net_name_of_id (alloc ctx ok)) (net_name_of_id (alloc ctx ik))
           | _ -> ())
      | _ -> ()) m.instances;
  (* Emit each cell instance with its INIT/property parameters. *)
  let is_skipped_cell = function "GND" | "VCC" -> true | _ -> false in
  (* GND/VCC CELL instances are skipped (constants ride the shared tie nets) —
     but their output nets must then BECOME the tie nets, or every reader of a
     named const net (reset-sync CE/R pins!) is left floating. *)
  List.iter (fun (i : binstance) ->
    match i.module_name with
    | ("GND" | "VCC") as mn ->
        let tie = if mn = "GND" then "n_GND" else "n_VCC" in
        let pin = if mn = "GND" then "G" else "P" in
        (match List.assoc_opt pin i.port_connections with
         | Some e ->
             List.iter (fun n -> if n <> tie then Hashtbl.replace net_alias n tie)
               (nets_of_conn ctx e)
         | None -> ())
    | _ -> ()) m.instances;
  (* ERC_AUTOFIX: redirect every undriven input net onto the grounded tie net.
     Applied AFTER the GND/VCC and ident-obuf aliases so a real driver alias
     always wins; a purely-floating net has none, so it lands on n_GND. *)
  Hashtbl.iter (fun n () ->
    if not (Hashtbl.mem net_alias n) then Hashtbl.replace net_alias n "n_GND")
    autofix_gnd;
  let live_cells = List.filter (fun (i : binstance) ->
    not (is_skipped_cell i.module_name)
    && not (Hashtbl.mem ident_names i.inst_name)) m.instances in
  List.iter (fun (i : binstance) ->
    pp "          (instance %s (viewref netlist (cellref %s (libraryref hdi_primitives)))"
      (edif_safe_id i.inst_name) (edif_safe_id i.module_name);
    let seen_prop : (string, unit) Hashtbl.t = Hashtbl.create 8 in
    List.iter (fun (k, v) ->
      if not (Hashtbl.mem seen_prop k) then begin
        Hashtbl.add seen_prop k ();
        pp "\n            (property %s (string %s))" k (edif_property_value v)
      end
    ) i.param_strs;
    List.iter (fun (k, n) ->
      if not (Hashtbl.mem seen_prop k) then begin
        Hashtbl.add seen_prop k ();
        pp "\n            (property %s (integer %d))" k n
      end
    ) i.param_values;
    pp "\n          )\n"
  ) live_cells;

  (* Collect every port reference per net.  Vivado/Xilinx EDIF
   * convention: for an array port renamed as "led[7:0]" (descending),
   * `(member led 0)` is led[7] (MSB).  Our bits arrays are LSB-first,
   * so we reverse the member index when emitting. *)
  let net_uses : (string, string list) Hashtbl.t = Hashtbl.create 1024 in
  (* post-bypass connectivity audit: the ident-obuf bypass and const-output
     skip DROP portrefs after the ERC ran — verify at the FINAL net list that
     every net read still has a driver (catches a bypass that lost the
     driver-to-port redirect, e.g. undriven pcspma_status bits). *)
  let net_has_driver : (string, unit) Hashtbl.t = Hashtbl.create 1024 in
  let net_readers : (string, string list) Hashtbl.t = Hashtbl.create 1024 in
  let add_use ?(driver = false) ?(who = "") net pref =
    let net = resolve_alias net in
    let cur = try Hashtbl.find net_uses net with Not_found -> [] in
    Hashtbl.replace net_uses net (pref :: cur);
    if driver then Hashtbl.replace net_has_driver net ()
    else begin
      let r = try Hashtbl.find net_readers net with Not_found -> [] in
      Hashtbl.replace net_readers net (who :: r)
    end
  in
  let mem_idx ~w i = w - 1 - i in

  (* Per-instance pin references. *)
  List.iter (fun (i : binstance) ->
    let inst_id = edif_safe_id i.inst_name in
    let ports = primtype_ports i.module_name in
    List.iter (fun (pin, expr) ->
      let pin_w = match List.find_opt (fun (p, _, _) -> p = pin) ports with
        | Some (_, _, w) -> w
        | None -> 1 in
      (* Clamp the connection to the pin's declared width: a scalar pin (LUT
         I0, etc.) must connect to exactly ONE net.  A malformed wider
         connection (a gate-map edge case producing a bus on a 1-bit LUT input)
         would otherwise emit that pin into every net's member list, which
         Vivado rejects as a multiply-connected pin. *)
      let nets =
        let all = nets_of_conn ctx expr in
        if List.length all > pin_w then
          List.filteri (fun i _ -> i < pin_w) all
        else all in
      let pin_dir = match List.find_opt (fun (p, _, _) -> p = pin) ports with
        | Some (_, d, _) -> d | None -> `Input in
      List.iteri (fun bit_i net ->
        (* A cell OUTPUT folded to a constant (net n_GND/n_VCC) must not drive
           the tie net — that is a multiply-driven-net DRC.  Skip it (the
           output is left dangling; consumers already read the tie directly). *)
        if pin_dir = `Output && (net = "n_GND" || net = "n_VCC") then ()
        else begin
          let pref =
            if pin_w = 1 then
              Printf.sprintf "(portref %s (instanceref %s))"
                (edif_safe_id pin) inst_id
            else
              Printf.sprintf "(portref (member %s %d) (instanceref %s))"
                (edif_safe_id pin) (mem_idx ~w:pin_w bit_i) inst_id
          in
          add_use ~driver:(pin_dir = `Output)
            ~who:(i.inst_name ^ "." ^ pin) net pref
        end) nets
    ) i.port_connections) live_cells;

  (* Constant tie drivers. *)
  add_use ~driver:true "n_GND" "(portref G (instanceref n_GND_inst))";
  add_use ~driver:true "n_VCC" "(portref P (instanceref n_VCC_inst))";

  (* Top-level port references. *)
  Hashtbl.iter (fun nm (dir, w, bits) ->
    List.iteri (fun bit_i id ->
      let net = net_name_of_id id in
      let pref =
        if w = 1 then Printf.sprintf "(portref %s)" (edif_safe_id nm)
        else Printf.sprintf "(portref (member %s %d))"
               (edif_safe_id nm) (mem_idx ~w bit_i)
      in
      add_use ~driver:(dir = `Input)
        ~who:(Printf.sprintf "port:%s[%d]" nm bit_i) net pref) bits) port_bits;
  if Sys.getenv_opt "SVS_ERC" <> Some "0" then begin
    let lost =
      Hashtbl.fold (fun net rs acc ->
        if Hashtbl.mem net_has_driver net then acc else (net, rs) :: acc)
        net_readers [] in
    if lost <> [] then begin
      let sym : (string, string) Hashtbl.t = Hashtbl.create 4096 in
      Hashtbl.iter (fun (k : net_key) id ->
        Hashtbl.replace sym (net_name_of_id id)
          (Printf.sprintf "%s[%d]" k.base k.bit)) ctx.ids;
      let body = String.concat "\n  " (List.map (fun (net, rs) ->
        Printf.sprintf "%S (%s) read by [%s] — DRIVER LOST POST-ERC"
          (match Hashtbl.find_opt sym net with Some s -> s | None -> net) net
          (String.concat "," (List.filteri (fun i _ -> i < 3) rs)))
        (List.filteri (fun i _ -> i < 12) lost)) in
      failwith (Printf.sprintf
        "bir_to_edif POST-BYPASS: %d net(s) lost their driver between ERC and \
         emission (ident-obuf bypass / const-output skip bug):\n  %s"
        (List.length lost) body)
    end
  end;

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

(* ─── Hierarchical EDIF: emit one `work` cell per module, submodule instances
 * as `work` cellrefs, primitives as `hdi_primitives` — NO flatten_struct.
 * Each module's net connectivity comes straight from gate_map (miter-proven),
 * so this avoids the flatten artifacts (bit-bus aliasing / dropped drivers). *)
let write_edif_hier
    ~(library_cells : (string * library_port list) list)
    ~(path : string) (p : bprogram) ~(top : string) : unit =
  let user_modules : (string, bmodule) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun (m : bmodule) -> Hashtbl.replace user_modules m.name m) p.modules;
  let sig_width (s : bsignal) = match s.stype with
    | BInt { width; _ } -> width | BBool -> 1
    | BArray { element = BInt { width; _ }; size } -> width * size
    | _ -> 1 in
  let user_ports : (string, (string * [`Input|`Output] * int) list) Hashtbl.t =
    Hashtbl.create 64 in
  List.iter (fun (m : bmodule) ->
    Hashtbl.replace user_ports m.name
      (List.filter_map (fun (s : bsignal) -> match s.direction with
         | `Input  -> Some (s.name, `Input, sig_width s)
         | `Output -> Some (s.name, `Output, sig_width s)
         | _ -> None) m.signals)) p.modules;
  let xil_primitive_ports : (string * (string * [`Input|`Output] * int) list) list = [
    "LUT1",[ "O",`Output,1;"I0",`Input,1 ];
    "LUT2",[ "O",`Output,1;"I0",`Input,1;"I1",`Input,1 ];
    "LUT3",[ "O",`Output,1;"I0",`Input,1;"I1",`Input,1;"I2",`Input,1 ];
    "LUT4",[ "O",`Output,1;"I0",`Input,1;"I1",`Input,1;"I2",`Input,1;"I3",`Input,1 ];
    "LUT5",[ "O",`Output,1;"I0",`Input,1;"I1",`Input,1;"I2",`Input,1;"I3",`Input,1;"I4",`Input,1 ];
    "LUT6",[ "O",`Output,1;"I0",`Input,1;"I1",`Input,1;"I2",`Input,1;"I3",`Input,1;"I4",`Input,1;"I5",`Input,1 ];
    "FDRE",[ "Q",`Output,1;"C",`Input,1;"CE",`Input,1;"D",`Input,1;"R",`Input,1 ];
    "FDSE",[ "Q",`Output,1;"C",`Input,1;"CE",`Input,1;"D",`Input,1;"S",`Input,1 ];
    "FDCE",[ "Q",`Output,1;"C",`Input,1;"CE",`Input,1;"D",`Input,1;"CLR",`Input,1 ];
    "FDPE",[ "Q",`Output,1;"C",`Input,1;"CE",`Input,1;"D",`Input,1;"PRE",`Input,1 ];
    "CARRY4",[ "CO",`Output,4;"O",`Output,4;"CI",`Input,1;"CYINIT",`Input,1;"DI",`Input,4;"S",`Input,4 ];
    "BUFG",[ "O",`Output,1;"I",`Input,1 ]; "IBUF",[ "O",`Output,1;"I",`Input,1 ];
    "OBUF",[ "O",`Output,1;"I",`Input,1 ]; "INV",[ "O",`Output,1;"I",`Input,1 ];
    "IBUFDS",[ "O",`Output,1;"I",`Input,1;"IB",`Input,1 ];
    "MUXF7",[ "O",`Output,1;"I0",`Input,1;"I1",`Input,1;"S",`Input,1 ];
    "MUXF8",[ "O",`Output,1;"I0",`Input,1;"I1",`Input,1;"S",`Input,1 ];
  ] in
  let is_user t = Hashtbl.mem user_modules t in
  let is_skipped_cell = function "GND" | "VCC" -> true | _ -> false in
  let is_keep_port nm = String.length nm >= 7 && String.sub nm 0 7 = "__keep_" in
  (* declared port interface of any cell type (user module or primitive). *)
  let prim_ports t : (string * [`Input|`Output] * int) list =
    if is_user t then (try Hashtbl.find user_ports t with Not_found -> [])
    else
      let auth =
        match Hashtbl.find_opt (Lazy.force xil_json_ports) t with
        | Some l when l <> [] -> Some l
        | _ ->
          (match List.assoc_opt t library_cells with
           | Some ports when ports <> [] ->
               Some (List.map (fun (pp : library_port) ->
                 pp.port_name, pp.port_direction, pp.port_width) ports)
           | _ -> None) in
      (match auth with
       | Some l -> l
       | None ->
         (match List.assoc_opt t xil_primitive_ports with
          | Some l -> l
          | None ->
            (match (try Vhdl_to_behavioral.lookup_xil_primitive_ports [t] with _ -> []) with
             | (_, ports) :: _ ->
                 List.map (fun (pp : library_port) ->
                   pp.port_name, pp.port_direction, pp.port_width) ports
             | [] -> []))) in
  (* Union of ports (and max connection width) each user cell type is
     instantiated with across ALL parents — a cell must declare every port any
     parent connects (gate_map may drop a child input the parent still wires,
     e.g. rgmii_lfsr.state_in). *)
  let type_conn : (string, (string, int) Hashtbl.t) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun (m : bmodule) ->
    let ctx = mk_ctx () in populate_widths ctx m;
    let bump base w = let cur = try Hashtbl.find ctx.widths base with Not_found -> 1 in
      if w > cur then Hashtbl.replace ctx.widths base w in
    let rec infer = function
      | BSlice { signal = BVar base; msb; lsb } -> bump base (max msb lsb + 1)
      | BSlice { signal; _ } -> infer signal
      | BSelect { array = BVar base; index = BConst { value; _ } } -> bump base (Z.to_int value + 1)
      | BConcat es -> List.iter infer es | _ -> () in
    List.iter (fun (i : binstance) -> List.iter (fun (_, e) -> infer e) i.port_connections) m.instances;
    List.iter (fun (i : binstance) ->
      if is_user i.module_name then begin
        let h = match Hashtbl.find_opt type_conn i.module_name with
          | Some h -> h | None -> let h = Hashtbl.create 16 in Hashtbl.add type_conn i.module_name h; h in
        List.iter (fun (pin, e) ->
          let w = max 1 (List.length (nets_of_conn ctx e)) in
          Hashtbl.replace h pin (max (try Hashtbl.find h pin with Not_found -> 0) w)) i.port_connections
      end) m.instances) p.modules;
  (* Base signal names a module DRIVES internally: any process-assigned LHS,
     any @mem_write/@slice_write target, and any net wired to a child instance's
     OUTPUT pin.  Used to infer the direction of connected-but-undeclared ports
     (below): if the lower-level module is proven to drive the port, it is an
     OUTPUT (a driver), not a defaulted input — otherwise a mis-/under-declared
     driven port reads as a floating net in the parent ERC. *)
  let driven_of (m : bmodule) : (string, unit) Hashtbl.t =
    let d = Hashtbl.create 64 in
    let mark n = Hashtbl.replace d n () in
    let rec base = function
      | BVar n -> Some n
      | BSelect { array; _ } -> base array
      | BSlice { signal; _ } -> base signal
      | _ -> None in
    let rec st = function
      | BAssign { lhs; _ } -> mark lhs
      | BCallStmt { func = ("@mem_write" | "@slice_write" | "@part_sel_write_up");
                    args = (BVar a) :: _ } -> mark a
      | BIf r -> List.iter st r.then_stmts; List.iter st r.else_stmts
      | BCase r -> List.iter (fun (_, b) -> List.iter st b) r.cases; List.iter st r.default
      | BBlock b -> List.iter st b
      | BWhile r -> List.iter st r.body
      | BFor r -> st r.init; st r.update; List.iter st r.body
      | _ -> () in
    List.iter (function
      | BCombinational c -> List.iter st c.body
      | BSequential s -> List.iter st s.body) m.processes;
    List.iter (fun (i : binstance) ->
      let ports = prim_ports i.module_name in
      List.iter (fun (pin, e) ->
        match List.find_opt (fun (p, _, _) -> p = pin) ports with
        | Some (_, `Output, _) -> (match base e with Some n -> mark n | None -> ())
        | _ -> ()) i.port_connections) m.instances;
    d in
  (* Fold the connected-port union into user_ports so BOTH the child's interface
     and the parent's connection emission size each pin identically (array member
     refs, not scalar).  Declared I/O keep their signal widths; an undeclared
     connected pin becomes an OUTPUT if the module drives it internally, else an
     input, at the connection width. *)
  List.iter (fun (m : bmodule) ->
    let existing = try Hashtbl.find user_ports m.name with Not_found -> [] in
    let names = List.map (fun (n, _, _) -> n) existing in
    let extra = match Hashtbl.find_opt type_conn m.name with
      | Some h ->
          let drv = driven_of m in
          Hashtbl.fold (fun pin w acc ->
            if List.mem pin names then acc
            else (pin, (if Hashtbl.mem drv pin then `Output else `Input), w) :: acc) h []
      | None -> [] in
    Hashtbl.replace user_ports m.name (existing @ extra)) p.modules;
  let buf = Buffer.create (1024 * 1024) in
  let pp fmt = Printf.ksprintf (Buffer.add_string buf) fmt in
  let now = Unix.gmtime (Unix.time ()) in
  pp "(edif %s\n" (edif_safe_id top);
  pp "  (edifversion 2 0 0)\n  (edifLevel 0)\n  (keywordmap (keywordlevel 0))\n";
  pp "  (status (written (timeStamp %d %d %d %d %d %d)\n      (program \"bir_to_edif_hier\")))\n"
    (now.tm_year + 1900) (now.tm_mon + 1) now.tm_mday now.tm_hour now.tm_min now.tm_sec;

  (* all PRIMITIVE cell types used anywhere (instances whose module is not a user cell). *)
  let used_prim : (string, unit) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun (m : bmodule) ->
    List.iter (fun (i : binstance) ->
      if not (is_user i.module_name) then Hashtbl.replace used_prim i.module_name ())
      m.instances) p.modules;
  pp "  (Library hdi_primitives\n    (edifLevel 0)\n    (technology (numberDefinition))\n";
  pp "    (cell GND (celltype GENERIC) (view netlist (viewtype NETLIST) (interface (port G (direction OUTPUT)))))\n";
  pp "    (cell VCC (celltype GENERIC) (view netlist (viewtype NETLIST) (interface (port P (direction OUTPUT)))))\n";
  Hashtbl.iter (fun ty () ->
    if is_skipped_cell ty then () else begin
      pp "    (cell %s (celltype GENERIC)\n      (view netlist (viewtype NETLIST)\n        (interface\n" (edif_safe_id ty);
      List.iter (fun (pname, dir, w) ->
        let d = match dir with `Input -> "INPUT" | `Output -> "OUTPUT" in
        if w = 1 then pp "          (port %s (direction %s))\n" (edif_safe_id pname) d
        else pp "          (port (array (rename %s \"%s[%d:0]\") %d) (direction %s))\n"
               (edif_safe_id pname) pname (w - 1) w d) (prim_ports ty);
      pp "        )))\n"
    end) used_prim;
  pp "  )\n";

  (* ── one work cell per module ── *)
  let emit_cell (m : bmodule) =
    let ctx = mk_ctx () in
    populate_widths ctx m;
    let port_names : (string, unit) Hashtbl.t = Hashtbl.create 32 in
    List.iter (fun (s : bsignal) ->
      if s.direction <> `Internal then Hashtbl.replace port_names s.name ()) m.signals;
    let bump base w =
      if not (Hashtbl.mem port_names base) then begin
        let cur = try Hashtbl.find ctx.widths base with Not_found -> 1 in
        if w > cur then Hashtbl.replace ctx.widths base w end in
    let rec infer = function
      | BSlice { signal = BVar base; msb; lsb } -> bump base (max msb lsb + 1)
      | BSlice { signal; _ } -> infer signal
      | BSelect { array = BVar base; index = BConst { value; _ } } -> bump base (Z.to_int value + 1)
      | BConcat es -> List.iter infer es | _ -> () in
    List.iter (fun (i : binstance) -> List.iter (fun (_, e) -> infer e) i.port_connections) m.instances;
    (* Interface = the reconciled user_ports (declared outputs + connected-port
       union), so the child's ports and the parent's connections use IDENTICAL
       widths (member refs on both sides). *)
    let port_bits : (string, [`Input|`Output|`Internal] * int * int list) Hashtbl.t = Hashtbl.create 32 in
    List.iter (fun (pn, dir, w) ->
      if not (is_keep_port pn) && not (Hashtbl.mem port_bits pn) then
        Hashtbl.add port_bits pn ((dir :> [`Input|`Output|`Internal]), w,
          List.init w (fun i -> alloc ctx { base = pn; bit = i })))
      (try Hashtbl.find user_ports m.name with Not_found -> []);
    (* interface *)
    pp "    (cell %s (celltype GENERIC)\n      (view netlist (viewtype NETLIST)\n        (interface\n" (edif_safe_id m.name);
    Hashtbl.iter (fun nm (dir, w, _) ->
      let d = match dir with `Input -> "INPUT" | `Output -> "OUTPUT" | _ -> "INPUT" in
      if w = 1 then pp "          (port %s (direction %s))\n" (edif_safe_id nm) d
      else pp "          (port (array (rename %s \"%s[%d:0]\") %d) (direction %s))\n"
             (edif_safe_id nm) nm (w - 1) w d) port_bits;
    pp "        )\n        (contents\n";
    pp "          (instance n_GND_inst (viewref netlist (cellref GND (libraryref hdi_primitives))))\n";
    pp "          (instance n_VCC_inst (viewref netlist (cellref VCC (libraryref hdi_primitives))))\n";
    let live_cells = List.filter (fun (i : binstance) -> not (is_skipped_cell i.module_name)) m.instances in
    List.iter (fun (i : binstance) ->
      let lib = if is_user i.module_name then "work" else "hdi_primitives" in
      pp "          (instance %s (viewref netlist (cellref %s (libraryref %s)))"
        (edif_safe_id i.inst_name) (edif_safe_id i.module_name) lib;
      let seen : (string, unit) Hashtbl.t = Hashtbl.create 8 in
      List.iter (fun (k, v) -> if not (Hashtbl.mem seen k) then begin
        Hashtbl.add seen k (); pp "\n            (property %s (string %s))" k (edif_property_value v) end) i.param_strs;
      List.iter (fun (k, n) -> if not (Hashtbl.mem seen k) then begin
        Hashtbl.add seen k (); pp "\n            (property %s (integer %d))" k n end) i.param_values;
      pp "\n          )\n") live_cells;
    (* nets *)
    let net_uses : (string, string list) Hashtbl.t = Hashtbl.create 1024 in
    let net_has_driver : (string, unit) Hashtbl.t = Hashtbl.create 1024 in
    let net_readers : (string, string list) Hashtbl.t = Hashtbl.create 1024 in
    let add_use ?(driver = false) ?(who = "") net pref =
      Hashtbl.replace net_uses net (pref :: (try Hashtbl.find net_uses net with Not_found -> []));
      if driver then Hashtbl.replace net_has_driver net ()
      else Hashtbl.replace net_readers net (who :: (try Hashtbl.find net_readers net with Not_found -> [])) in
    let mem_idx ~w i = w - 1 - i in
    List.iter (fun (i : binstance) ->
      let inst_id = edif_safe_id i.inst_name in
      let ports = prim_ports i.module_name in
      List.iter (fun (pin, expr) ->
        let pin_w = match List.find_opt (fun (pn, _, _) -> pn = pin) ports with Some (_, _, w) -> w | None -> 1 in
        let pin_dir = match List.find_opt (fun (pn, _, _) -> pn = pin) ports with Some (_, d, _) -> d | None -> `Input in
        let nets = let all = nets_of_conn ctx expr in
          if List.length all > pin_w then List.filteri (fun k _ -> k < pin_w) all else all in
        List.iteri (fun bit_i net ->
          if pin_dir = `Output && (net = "n_GND" || net = "n_VCC") then ()
          else begin
            let pref = if pin_w = 1 then Printf.sprintf "(portref %s (instanceref %s))" (edif_safe_id pin) inst_id
                       else Printf.sprintf "(portref (member %s %d) (instanceref %s))" (edif_safe_id pin) (mem_idx ~w:pin_w bit_i) inst_id in
            add_use ~driver:(pin_dir = `Output) ~who:(i.inst_name ^ "." ^ pin) net pref
          end) nets) i.port_connections) live_cells;
    add_use ~driver:true "n_GND" "(portref G (instanceref n_GND_inst))";
    add_use ~driver:true "n_VCC" "(portref P (instanceref n_VCC_inst))";
    Hashtbl.iter (fun nm (dir, w, bits) ->
      List.iteri (fun bit_i id ->
        let net = net_name_of_id id in
        let pref = if w = 1 then Printf.sprintf "(portref %s)" (edif_safe_id nm)
                   else Printf.sprintf "(portref (member %s %d))" (edif_safe_id nm) (mem_idx ~w bit_i) in
        (* a module INPUT drives internal nets; an OUTPUT reads them *)
        add_use ~driver:(dir = `Input) ~who:(Printf.sprintf "port:%s[%d]" nm bit_i) net pref) bits) port_bits;
    if Sys.getenv_opt "SVS_ERC" <> Some "0" then begin
      let lost = Hashtbl.fold (fun net rs acc -> if Hashtbl.mem net_has_driver net then acc else (net, rs) :: acc) net_readers [] in
      if lost <> [] then
        Printf.eprintf "[edif_hier] %s: %d net(s) read with no driver (e.g. %s)\n" m.name
          (List.length lost)
          (match lost with (n, _) :: _ -> n | [] -> "");
      (* HIER_ERC_DIAG=<module>: dump every undriven net's symbolic name +
         readers so we can see if the emitter dropped a real driver or the
         name is a port/const the crude ERC misclassifies. *)
      (match Sys.getenv_opt "HIER_ERC_DIAG" with
       | Some target when target = m.name ->
         let sym : (string, string) Hashtbl.t = Hashtbl.create 4096 in
         Hashtbl.iter (fun (k : net_key) id ->
           Hashtbl.replace sym (net_name_of_id id) (Printf.sprintf "%s[%d]" k.base k.bit)) ctx.ids;
         List.iter (fun (net, rs) ->
           Printf.eprintf "[HDIAG] %s (%s) <- read by %s\n"
             (match Hashtbl.find_opt sym net with Some s -> s | None -> net) net
             (String.concat ", " (List.filteri (fun i _ -> i < 3) rs))) lost
       | _ -> ())
    end;
    Hashtbl.iter (fun net uses ->
      pp "          (net %s\n            (joined" net;
      List.iter (fun u -> pp "\n              %s" u) uses;
      pp ")\n          )\n") net_uses;
    pp "        )\n      )\n    )\n"
  in
  pp "  (Library work\n    (edifLevel 0)\n    (technology (numberDefinition))\n";
  (* Vivado read_edif is single-pass (definition-before-use): a cell must be
     emitted before any cell that instantiates it.  Post-order DFS = leaves
     first, each module after its user-submodules. *)
  let ordered =
    let visited = Hashtbl.create 128 and order = ref [] in
    let rec dfs n =
      if not (Hashtbl.mem visited n) then begin
        Hashtbl.add visited n ();
        (match Hashtbl.find_opt user_modules n with
         | Some (m : bmodule) ->
             List.iter (fun (i : binstance) ->
               if is_user i.module_name then dfs i.module_name) m.instances;
             order := m :: !order
         | None -> ())
      end in
    List.iter (fun (m : bmodule) -> dfs m.name) p.modules;
    List.rev !order in
  List.iter emit_cell ordered;
  pp "  )\n";
  pp "  (Design %s (cellref %s (libraryref work)))\n" (edif_safe_id top) (edif_safe_id top);
  pp ")\n";
  let oc = open_out path in
  Fun.protect ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc (Buffer.contents buf))
