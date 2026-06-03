(* EDIF -> Behavioral_ir, structural, via the menhir-based edif2 parser.
 *
 * Direct walker on the raw Edif2.token tree — we don't go through
 * Emain.rw because its pattern matches expect a slightly different
 * grouping than Vivado-2020.1 emits.  Vivado puts the top-level edif as
 *   ITEM(Edif, TLIST [ID top; ITEM(Edifversion, ...); ...; ITEM(Library, ...)])
 * while Emain.rw expects ITEM2(Edif, TLIST [ID top], TLIST [...]).
 *
 * EDIF conventions we honour:
 *   - case-insensitive keyword matching (handled by elexer.mll/eord.ml)
 *   - `(member P k)` indexes by declaration order: for a port renamed
 *     "P[hi:lo]" (Vivado convention), k=0 = Verilog P[hi] = MSB.
 *     Sorting incoming portrefs by k ascending therefore gives an
 *     MSB-first list — which is exactly what BConcat consumes.  No
 *     List.rev required (the regex parser's reverse was the source of
 *     the CARRY4 chain-bit-swap bug on counter25 BUFG hardware).
 *)

open Behavioral_ir
open Edif2

(* ── small helpers ───────────────────────────────────────────────────── *)

let parse_signal_name (name : string) : string * int option =
  try
    let bracket_pos = String.rindex name '[' in
    let close_pos = String.rindex name ']' in
    if close_pos = String.length name - 1 then
      let base = String.sub name 0 bracket_pos in
      let idx = int_of_string (String.sub name (bracket_pos + 1)
                                 (close_pos - bracket_pos - 1)) in
      (base, Some idx)
    else (name, None)
  with _ -> (name, None)

let net_to_expr (net_name : string) : bexpr =
  let (base, idx_opt) = parse_signal_name net_name in
  match idx_opt with
  | Some idx -> BSelect { array = BVar base; index = BConst { value = idx; width = 32 } }
  | None     -> BVar base

let btype_for_width w = BInt { width = w; signed = Unsigned }

(* Direction extractor for a port's direction marker.  *)
let dir_of_tok = function
  | Input  -> `Input
  | Output -> `Output
  | Inout  -> `Input   (* nextpnr-xilinx doesn't carry inouts in our flow *)
  | _      -> `Internal

(* ── token-tree navigation primitives ───────────────────────────────── *)

(* Children of any TLIST / ITEM / ITEM2 wrapper, flattening one level.   *)
let flatten1 (t : token) : token list =
  match t with
  | TLIST xs            -> xs
  | ITEM (_, TLIST xs)  -> xs
  | ITEM (_, x)         -> [x]
  | ITEM2 (_, b1, b2)   -> [b1; b2]
  | other               -> [other]

(* List of children of an item with the named tag, or [] if not found.   *)
let children_of_tag (tag : token) (items : token list) : token list =
  let matches = List.filter (function
    | ITEM (k, _) -> k = tag
    | ITEM2 (k, _, _) -> k = tag
    | _ -> false) items in
  List.concat_map flatten1 matches

(* Find the first ID inside `items`. *)
let first_id (items : token list) : string option =
  List.find_map (function ID s -> Some s | _ -> None) items

let first_int (items : token list) : int option =
  List.find_map (function INT n -> Some n | _ -> None) items

let first_string (items : token list) : string option =
  List.find_map (function STRING s -> Some s | _ -> None) items

(* Parse "name[hi:lo]" -> ("name", Some (hi-lo+1)) etc.  *)
let parse_array_rename (s : string) : string * int option =
  try
    let lb = String.index s '[' in
    let colon = String.index s ':' in
    let rb = String.index s ']' in
    if rb = String.length s - 1 then
      let base = String.sub s 0 lb in
      let hi = int_of_string (String.sub s (lb+1) (colon - lb - 1)) in
      let lo = int_of_string (String.sub s (colon+1) (rb - colon - 1)) in
      let w = abs (hi - lo) + 1 in
      (base, Some w)
    else (s, None)
  with _ -> (s, None)

(* ── port / portref / instance / property parsing ───────────────────── *)

type pinfo = {
  pname : string;
  prename : string;
  pwidth : int;
  pdir : [`Input | `Output | `Internal];
}

(* Parse an EDIF (port ...) item.
   Three forms we care about:
     1. ITEM2 (Port, TLIST [ID name], TLIST [ITEM (Direction, TLIST [dir])])
     2. ITEM2 (Port, TLIST [], TLIST [ITEM (Rename, TLIST [ID nm; STRING str]);
                                       ITEM (Direction, TLIST [dir])])
     3. ITEM2 (Port, TLIST [],
               TLIST [ITEM2 (Array,
                             ITEM (Rename, TLIST [ID nm; STRING str]),
                             INT wid);
                      ITEM (Direction, TLIST [dir])])
*)
let port_of_token (t : token) : pinfo option =
  let port_body = match t with
    | ITEM2 (Port, _, body)         -> Some (flatten1 body)
    | ITEM  (Port, TLIST items)     -> Some items
    | ITEM  (Port, single)          -> Some [single]
    | _ -> None
  in
  match port_body with
  | None -> None
  | Some items ->
    (* Original code expected items already flattened. *)
    let _ = items in
    let items = items in
    let dir =
      match List.find_map (function
        | ITEM (Direction, TLIST [d]) -> Some (dir_of_tok d)
        | _ -> None) items with
      | Some d -> d
      | None -> `Input
    in
    (* Array form first: (array (rename id "name[hi:lo]") wid) *)
    let array_form =
      List.find_map (function
        | ITEM2 (Array,
                 ITEM (Rename, TLIST [ID nm; STRING rn]),
                 INT wid) ->
          Some (nm, rn, wid)
        | _ -> None) items
    in
    (match array_form with
     | Some (nm, rn, wid) ->
       Some { pname = nm; prename = rn; pwidth = wid; pdir = dir }
     | None ->
       (* Rename-only form. *)
       let rn =
         List.find_map (function
           | ITEM (Rename, TLIST [ID nm; STRING rn]) -> Some (nm, rn)
           | _ -> None) items
       in
       (match rn with
        | Some (nm, rn) ->
          let (_, w_opt) = parse_array_rename rn in
          let w = (match w_opt with Some w -> w | None -> 1) in
          Some { pname = nm; prename = rn; pwidth = w; pdir = dir }
        | None ->
          (* Bare (port ID (direction ...)) form. *)
          (match first_id items with
           | Some nm -> Some { pname = nm; prename = nm; pwidth = 1; pdir = dir }
           | None -> None)))

type portref = {
  prpin   : string;
  pridx   : int option;   (* member k for bus pins *)
  prinst  : string option (* None = top-level port reference *)
}

(* Parse a (portref ...).  Vivado 2020.1 EDIF emits every node as the
   ITEM(Tag, TLIST [...]) form rather than ITEM2 — confirmed by the full
   token-tree dump in /home/jonathan/edif2/TerminalSavedOutput.txt.  We
   treat the TLIST as a heterogeneous bag of children and pick out the
   ones we care about:
     - ID name           — scalar pin reference
     - ITEM (Member, TLIST [ID name; INT k])
                         — array-member pin reference (member k)
     - ITEM (Instanceref, TLIST [ID instname])
                         — instance the pin belongs to.  If absent, the
                           portref refers to a top-level port. *)
let portref_of_token (t : token) : portref option =
  let items = match t with
    | ITEM  (Portref, TLIST xs) -> xs
    | ITEM  (Portref, single)   -> [single]
    | ITEM2 (Portref, h, b)     -> flatten1 h @ flatten1 b
    | _ -> []
  in
  if items = [] then None
  else
    let mem =
      List.find_map (function
        | ITEM (Member, TLIST [ID nm; INT k]) -> Some (nm, k)
        | _ -> None) items
    in
    let pin_id = first_id items in
    let inst =
      List.find_map (function
        | ITEM (Instanceref, TLIST [ID inm]) -> Some inm
        | ITEM (Instanceref, TLIST items) -> first_id items
        | _ -> None) items
    in
    (match mem, pin_id with
     | Some (nm, k), _ -> Some { prpin = nm; pridx = Some k; prinst = inst }
     | None, Some nm  -> Some { prpin = nm; pridx = None;   prinst = inst }
     | None, None     -> None)

(* (property NAME (string "val")) | (integer N) | (boolean (true|false))
   Vivado 2020.1 emits everything as ITEM (Property, TLIST [ID name;
   ITEM (TypeTag, TLIST [...])]).  We strip the outer quotes from
   STRING values because Vivado quotes them in EDIF. *)
let strip_quotes s =
  let n = String.length s in
  if n >= 2 && s.[0] = '"' && s.[n-1] = '"' then String.sub s 1 (n-2) else s

let prop_of_token (t : token) : (string * string) option =
  let items = match t with
    | ITEM  (Property, TLIST xs) -> xs
    | ITEM2 (Property, h, b)     -> flatten1 h @ flatten1 b
    | _ -> []
  in
  if items = [] then None
  else
    let pname = first_id items in
    let pval =
      List.find_map (function
        | ITEM (String, TLIST [STRING s]) -> Some (strip_quotes s)
        | ITEM (Integer, TLIST [INT n])   -> Some (string_of_int n)
        | ITEM (Boolean, TLIST [ITEM (True, _)])  -> Some "1"
        | ITEM (Boolean, TLIST [ITEM (False, _)]) -> Some "0"
        | _ -> None) items
    in
    match pname, pval with
    | Some p, Some v -> Some (p, v)
    | _ -> None

(* Instance: (instance name (viewref view (cellref cell (libraryref lib)))
              <property...>)
   or (instance (rename name "real_name") (viewref ...)) *)
type inst_t = {
  iname : string;
  icell : string;
  iprops : (string * string) list;
}

let inst_of_token (t : token) : inst_t option =
  let (head_items, body_items) = match t with
    | ITEM2 (Instance, head, tail) -> (flatten1 head, flatten1 tail)
    | ITEM (Instance, TLIST items) -> ([], items)
    | _ -> ([], [])
  in
  if head_items = [] && body_items = [] then None
  else
    let iname =
      match first_id head_items with
      | Some s -> Some s
      | None ->
        (match first_id body_items with
         | Some s -> Some s
         | None ->
           List.find_map (function
             | ITEM (Rename, TLIST [ID nm; STRING _]) -> Some nm
             | _ -> None) body_items)
    in
    (* Viewref/Cellref each appear as ITEM or ITEM2 depending on Vivado's
       grouping; descend with a permissive helper rather than a single
       hand-rolled pattern. *)
    let viewref_kids = List.concat_map (function
      | ITEM (Viewref, TLIST xs) -> xs
      | ITEM2 (Viewref, _, TLIST xs) -> xs
      | _ -> []) body_items in
    let icell = List.find_map (function
      | ITEM (Cellref, TLIST items) -> first_id items
      | ITEM2 (Cellref, head, _) -> first_id (flatten1 head)
      | _ -> None) viewref_kids in
    let iprops = List.filter_map prop_of_token body_items in
    (match iname, icell with
     | Some n, Some c -> Some { iname = n; icell = c; iprops }
     | _ -> None)

(* (net validated_name (joined <portrefs...>)) *)
type net_t = {
  nname : string;
  nports : portref list;
}

let net_of_token (t : token) : net_t option =
  let (head_items, body_items) = match t with
    | ITEM2 (Net, head, tail) -> (flatten1 head, flatten1 tail)
    | ITEM (Net, TLIST items) -> ([], items)
    | _ -> ([], [])
  in
  if head_items = [] && body_items = [] then None
  else
    let nname =
      match first_id head_items with
      | Some s -> Some s
      | None ->
        (match first_id body_items with
         | Some s -> Some s
         | None ->
           let search_items = head_items @ body_items in
           List.find_map (function
             | ITEM (Rename, TLIST [ID nm; STRING _]) -> Some nm
             | _ -> None) search_items)
    in
    let portrefs =
      (* Vivado 2020.1 emits ITEM(Joined, TLIST [...portrefs...]) per the
         token-tree dump; older / other EDIFs may use ITEM2(Joined, _,
         TLIST [...]).  Accept both. *)
      let joined_items =
        List.find_map (function
          | ITEM  (Joined, TLIST xs)       -> Some xs
          | ITEM2 (Joined, _, TLIST xs)    -> Some xs
          | _ -> None) (head_items @ body_items) in
      match joined_items with
      | Some xs -> List.filter_map portref_of_token xs
      | None -> []
    in
    (match nname with
     | Some n -> Some { nname = n; nports = portrefs }
     | None -> None)

(* ── cell / library / top traversal ─────────────────────────────────── *)

(* Cell shape: ITEM2(Cell, TLIST [ID cellid],
                          TLIST [ITEM(Celltype, TLIST [ID "GENERIC"]);
                                 ITEM2(View, TLIST [ID viewid],
                                       TLIST [ITEM(Viewtype, ...);
                                              ITEM2(Interface, ..., TLIST <ports>);
                                              ITEM2(Contents, ..., TLIST <insts+nets>);  (* maybe absent *)
                                              <properties>])]) *)

type cell_t = {
  cname : string;
  cports : pinfo list;
  cinsts : inst_t list;
  cnets : net_t list;
  cprops : (string * string) list;
}

let parse_cell (t : token) : cell_t option =
  let (cname, body) = match t with
    | ITEM2 (Cell, head, tail) -> (first_id (flatten1 head), flatten1 tail)
    | ITEM  (Cell, TLIST items) ->
      (* Single-ITEM form Vivado 2020.1 emits:
         ITEM(Cell, TLIST [ID name; Celltype; View]).  Strip leading IDs
         so downstream finds the View/Celltype/properties cleanly. *)
      (first_id items,
       List.filter (function ID _ -> false | _ -> true) items)
    | _ -> (None, [])
  in
  match cname with
  | None -> None
  | Some _ ->
    let _ = body in
    let body = body in
    (* Drill into the view -> interface / contents.                      *)
    (* Helpers that recognise either ITEM(Tag, ...) or ITEM2(Tag, ...). *)
    let children_with_tag tag items =
      List.concat_map (function
        | ITEM (k, TLIST xs) when k = tag -> xs
        | ITEM (k, x)        when k = tag -> [x]
        | ITEM2 (k, _, TLIST xs) when k = tag -> xs
        | ITEM2 (k, _, x) when k = tag -> [x]
        | _ -> []) items
    in
    let view_children = children_with_tag View body in
    let interface_items = children_with_tag Interface view_children in
    let contents_items  = children_with_tag Contents view_children in
    let cports = List.filter_map port_of_token interface_items in
    let cinsts = List.filter_map inst_of_token contents_items in
    let cnets  = List.filter_map net_of_token  contents_items in
    let cprops = List.filter_map prop_of_token view_children in
    Some { cname = (match cname with Some n -> n | None -> "?");
           cports; cinsts; cnets; cprops }

(* Convert a parsed cell into a bmodule (the user-design path) or into
   a library_cells entry (the primitive path).  Primitives have no
   instances + no nets — only port declarations. *)
let cell_is_primitive (c : cell_t) : bool =
  c.cinsts = [] && c.cnets = []

let convert_cell (c : cell_t) : bmodule =
  (* Identify GND/VCC primitive instances so we can collapse the EDIF
     "<const0>" / "<const1>" broadcast nets into the BIR's GND/VCC
     sentinels.  Without this, CARRY4 chain CI/CYINIT/DI ties end up as
     undriven internal wires after bir_to_edif emits the round-tripped
     EDIF (the emitter skips the GND/VCC user-instances, breaking the
     consumer-side driver).                                              *)
  let const_insts : (string, [`Gnd | `Vcc]) Hashtbl.t = Hashtbl.create 8 in
  List.iter (fun (i : inst_t) ->
    match i.icell with
    | "GND" -> Hashtbl.replace const_insts i.iname `Gnd
    | "VCC" -> Hashtbl.replace const_insts i.iname `Vcc
    | _ -> ()) c.cinsts;

  (* If a net has any portref pointing at a GND.G or VCC.P primitive
     output, the entire net is the constant — rewrite the net name to
     "GND"/"VCC" so consumer instances bind BVar "GND"/"VCC", which
     bir_to_edif.const_net then maps onto n_GND / n_VCC.                 *)
  let net_const_name (n : net_t) : string option =
    List.find_map (fun (pr : portref) ->
      match pr.prinst with
      | Some inst ->
        (match Hashtbl.find_opt const_insts inst with
         | Some `Gnd when pr.prpin = "G" -> Some "GND"
         | Some `Vcc when pr.prpin = "P" -> Some "VCC"
         | _ -> None)
      | None -> None) n.nports
  in
  let const_net_names : (string, string) Hashtbl.t = Hashtbl.create 8 in
  List.iter (fun (n : net_t) ->
    match net_const_name n with
    | Some k -> Hashtbl.replace const_net_names n.nname k
    | None -> ()) c.cnets;

  (* Top-level port-alias detection: if a net joins a top-level
     (portref (member portname k)) or (portref portname), the net IS
     that port's bit.  Rewrite consumer instances to drive/listen on
     the port directly so bir_to_nextpnr_json's port_bits and instance
     connections share net IDs — otherwise the OBUF output sits on an
     internal net while the top-level `led` port lives on a different
     one, and the IO packer can't find the OBUF driving each pad. *)
  let port_width : (string, int) Hashtbl.t = Hashtbl.create 16 in
  List.iter (fun (p : pinfo) ->
    Hashtbl.replace port_width p.pname p.pwidth) c.cports;
  let is_port_name nm = Hashtbl.mem port_width nm in
  (* net_name -> (port_name, bir_bit_index).  EDIF (member P k) indexes
     by declaration order, k=0 = MSB for "P[hi:lo]"; BIR bit i = LSB+i,
     so bir_bit = port_width - 1 - k. *)
  let port_alias : (string, string * int) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun (n : net_t) ->
    List.iter (fun (pr : portref) ->
      match pr.prinst with
      | None when is_port_name pr.prpin ->
        let w = Hashtbl.find port_width pr.prpin in
        let bir_bit = match pr.pridx with
          | Some k -> w - 1 - k
          | None   -> 0     (* scalar port *)
        in
        Hashtbl.replace port_alias n.nname (pr.prpin, bir_bit)
      | _ -> ()) n.nports
  ) c.cnets;

  (* Pin lookup: (inst, pin) -> [(member_index option, net_name); ...]    *)
  let pin_entries : (string * string, (int option * string) list) Hashtbl.t =
    Hashtbl.create 1024
  in
  List.iter (fun (n : net_t) ->
    let effective_name =
      match Hashtbl.find_opt const_net_names n.nname with
      | Some k -> k
      | None   -> n.nname
    in
    List.iter (fun (pr : portref) -> match pr.prinst with
      | Some inst when Hashtbl.mem const_insts inst ->
        (* Skip the GND/VCC primitive's own portrefs — we're collapsing
           those instances into BIR constants. *)
        ()
      | Some inst ->
        let k = (inst, pr.prpin) in
        let cur = try Hashtbl.find pin_entries k with Not_found -> [] in
        Hashtbl.replace pin_entries k ((pr.pridx, effective_name) :: cur)
      | None -> ()) n.nports
  ) c.cnets;

  let port_signals = List.map (fun p ->
    { name = p.pname;
      stype = btype_for_width p.pwidth;
      direction = p.pdir;
      initial_value = None;
      attrs = [] }) c.cports
  in
  let port_names = List.map (fun p -> p.pname) c.cports in
  let is_port nm = List.mem nm port_names in
  let net_names = ref [] in
  let seen = Hashtbl.create 4096 in
  List.iter (fun n ->
    (* Skip const nets (collapse onto GND/VCC) and port-aliased nets
       (collapse onto a top-level port bit) — these mustn't appear as
       standalone internal signals or they'd shadow the real port. *)
    if not (is_port n.nname)
       && not (Hashtbl.mem seen n.nname)
       && not (Hashtbl.mem const_net_names n.nname)
       && not (Hashtbl.mem port_alias n.nname)
    then begin
      Hashtbl.add seen n.nname ();
      net_names := n.nname :: !net_names
    end) c.cnets;
  let net_signals = List.rev_map (fun nm ->
    { name = nm;
      stype = BInt { width = 1; signed = Unsigned };
      direction = `Internal;
      initial_value = None;
      attrs = [] }) !net_names
  in
  let signals = port_signals @ List.rev net_signals in

  (* Filter out the GND/VCC primitive instances — their function is now
     encoded as BIR constants on consumer pins.  Keeping them would
     leak through as cells named "GND"/"VCC" in the nextpnr JSON. *)
  let user_insts =
    List.filter (fun (i : inst_t) ->
      not (Hashtbl.mem const_insts i.iname)) c.cinsts in
  (* Map a net name to its BIR expression, honouring port aliases:
     a net that joins a top-level port reference becomes BSelect{port, bit}
     so the JSON emitter unifies the OBUF.O net with the port's bit.    *)
  let net_to_expr_aliased nm =
    match Hashtbl.find_opt port_alias nm with
    | Some (pnm, bit) ->
      BSelect { array = BVar pnm;
                index = BConst { value = bit; width = 32 } }
    | None -> net_to_expr nm
  in
  let instances = List.map (fun (i : inst_t) ->
    let pcs = ref [] in
    Hashtbl.iter (fun (i_name, pin) entries ->
      if i_name = i.iname then begin
        let has_idx = List.exists (fun (idx, _) -> idx <> None) entries in
        if not has_idx then begin
          match entries with
          | [(_, nm)] -> pcs := (pin, net_to_expr_aliased nm) :: !pcs
          | _ ->
            let nm = match List.hd entries with (_, n) -> n in
            pcs := (pin, net_to_expr_aliased nm) :: !pcs
        end else begin
          (* Vivado-EDIF convention: (member P k) indexes by declaration
             order, so for "P[hi:lo]" k=0 = Verilog MSB.  Sort ascending
             k = MSB-first list; BConcat consumes its list MSB-first
             directly, so we do NOT reverse here. *)
          let sorted = List.sort (fun (a, _) (b, _) ->
            compare (match a with Some x -> x | None -> -1)
                    (match b with Some x -> x | None -> -1)) entries in
          let bits = List.map (fun (_, nm) -> net_to_expr_aliased nm) sorted in
          pcs := (pin, BConcat bits) :: !pcs
        end
      end) pin_entries;
    let pcs = List.sort (fun (a,_) (b,_) -> String.compare a b) !pcs in
    { inst_name        = i.iname;
      module_name      = i.icell;
      param_values     = [];
      param_strs       = i.iprops;
      port_connections = pcs }
  ) user_insts in

  { name = c.cname;
    params = [];
    signals;
    processes = [];
    instances;
    funcs = [];
    mems  = [];
    attrs = [] }

(* ── top-level entry ────────────────────────────────────────────────── *)

let convert (filename : string) : bprogram =
  let raw_tok = Eparse.eparse filename in
  let top_children =
    match raw_tok with
    | ITEM (Edif, TLIST xs) -> xs
    | TLIST xs              -> xs
    | _                     -> []
  in
  let library_items =
    List.concat_map (function
      | ITEM (Library, TLIST xs) -> [xs]
      | ITEM2 (Library, _, TLIST xs) -> [xs]
      | _ -> []) top_children
  in
  let modules       = ref [] in
  let library_cells = ref [] in
  List.iter (fun lib_items ->
    List.iter (fun tok ->
      match parse_cell tok with
      | None ->
        Printf.printf "  cell None\n%!"
      | Some c when cell_is_primitive c ->
        Printf.printf "  prim: %s (%d ports)\n%!" c.cname (List.length c.cports);
        let lps = List.map (fun p ->
          { port_name      = p.pname;
            port_direction = (match p.pdir with
                              | `Output -> `Output
                              | _       -> `Input);
            port_width     = p.pwidth }) c.cports in
        library_cells := (c.cname, lps) :: !library_cells
      | Some c ->
        Printf.printf "  user: %s (%d ports, %d insts, %d nets)\n%!"
          c.cname (List.length c.cports) (List.length c.cinsts) (List.length c.cnets);
        modules := convert_cell c :: !modules
    ) lib_items
  ) library_items;
  { modules = List.rev !modules;
    library_cells = List.rev !library_cells }
