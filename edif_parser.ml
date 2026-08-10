(* EDIF Parser for Behavioral Verilog Conversion
 * Parses EDIF netlists and extracts module structure
 *)

type port_direction = Input | Output | Inout

type port_info = {
  name: string;
  direction: port_direction;
  width: int;
}

type instance_info = {
  name: string;
  vivado_name: string;  (* The quoted original from (instance (rename SAFE_ID
                           "orig/name[3]") …) -- i.e. the name Vivado's own
                           physical database uses, complete with '/' and '[]'.
                           Equal to `name` when there is no rename.

                           WHY BOTH.  `name` stays the EDIF-safe identifier
                           because the Verilog/BIR consumers emit it as an
                           identifier, where '/' and '[' are illegal.  But
                           anything joining against Vivado's physical side (the
                           opendcp XML, a placement file) needs the original:
                           keyed on `name` an XML cell join matched 712 of 5812
                           cells and 5093 cells silently lost their placement.
                           parse_nets already draws this distinction for nets
                           (net_info.name is the original, original_name the safe
                           id); instances were asymmetric. *)
  cell_type: string;
  library: string;
  init: string option;  (* (property INIT (string "...")) — LUT truth table,
                           FF reset state, etc.  Raw verilog literal form
                           ("64'hABCDEF...", "1'b0", etc.); decoded by the
                           consumer.  None when no INIT property present. *)
  properties: (string * string) list;
                        (* All other (property NAME (kind VAL)) entries on
                           the instance — DIFF_TERM, IBUF_LOW_PWR,
                           CLKCM_CFG, etc.  Each (name, val_text); the
                           wrapping (string "...") / (boolean (true|false))
                           is stripped so consumers see just the literal
                           value text ("TRUE", "FALSE", "11", …).  Empty
                           list when no other properties present. *)
}

type net_pin = {
  inst: string option;  (* None = top-level port *)
  pin: string;
  index: int option;
}

type net_info = {
  name: string;
  original_name: string;
  connections: net_pin list;
}

type edif_data = {
  module_name: string;
  ports: port_info list;
  instances: instance_info list;
  nets: net_info list;
  library_cells: (string, port_info list) Hashtbl.t;
}

(* Read entire EDIF file *)

(* Safe slice that clamps to string bounds rather than raising
   Invalid_argument. Returns "" if start exceeds length. Used by the
   find_close-based section extractors below; a malformed (or simply
   foreign-dialect) EDIF whose paren depth doesn't balance would
   otherwise crash via String.sub. *)
let safe_sub content start length =
  let cap = String.length content in
  if start >= cap then ""
  else
    let length = min length (cap - start) in
    if length <= 0 then "" else String.sub content start length

let read_file filename =
  let ic = open_in filename in
  let buf = Buffer.create (1024 * 1024) in
  try
    while true do
      let line = input_line ic in
      Buffer.add_string buf line;
      Buffer.add_char buf '\n'
    done;
    ""
  with End_of_file ->
    close_in ic;
    Buffer.contents buf

(* Extract array size and name from EDIF rename: "NAME[H:L]" or "NAME[N]" *)
let parse_array_name str =
  try
    let open_bracket = String.index str '[' in
    let close_bracket = String.index str ']' in
    let base = String.sub str 0 open_bracket in

    (* Check if there's a colon (range) or just a single index *)
    try
      let colon = String.index_from str open_bracket ':' in
      if colon < close_bracket then
        (* Range format: NAME[H:L] *)
        let high = int_of_string (String.sub str (open_bracket + 1) (colon - open_bracket - 1)) in
        let low = int_of_string (String.sub str (colon + 1) (close_bracket - colon - 1)) in
        (base, high - low + 1)
      else
        (* No colon found in range, treat as single bit *)
        (base, 1)
    with Not_found ->
      (* Single bit format: NAME[N] *)
      (base, 1)
  with Not_found ->
    (* No brackets at all *)
    (str, 1)

(* Simple regex-like pattern matching *)
let extract_between start_pat end_pat text =
  try
    let start_idx = String.length start_pat +
      (try Str.search_forward (Str.regexp_string start_pat) text 0 with Not_found -> -1) in
    if start_idx < String.length start_pat then None
    else
      let end_idx = Str.search_forward (Str.regexp_string end_pat) text start_idx in
      Some (String.sub text start_idx (end_idx - start_idx))
  with _ -> None

(* Parse ports from interface section *)
let parse_ports content =
  let ports = ref [] in
  let rec find_port pos =
    try
      let port_start = Str.search_forward (Str.regexp_case_fold "(port ") content pos in
      (* Find matching closing paren *)
      let rec find_close p depth =
        if p >= String.length content then p
        else match content.[p] with
          | '(' -> find_close (p + 1) (depth + 1)
          | ')' -> if depth = 1 then p else find_close (p + 1) (depth - 1)
          | _ -> find_close (p + 1) depth
      in
      let port_end = find_close (port_start + 1) 1 in
      let port_text = safe_sub content port_start (port_end - port_start + 1) in

      (* Parse port - three cases:
         1. Array with rename: (port (array (rename NAME "NAME[H:L]") SIZE) (direction DIR))
         2. Simple rename: (port (rename NAME "NAME[N]") (direction DIR))
         3. Simple port: (port NAME (direction DIR))
      *)
      let is_array = String.contains port_text '(' &&
                     Str.string_match (Str.regexp_case_fold ".*array.*rename") port_text 0 in
      let is_renamed = String.contains port_text '(' &&
                       Str.string_match (Str.regexp_case_fold ".*port[ \t\n\r]+(rename") port_text 0 in

      if is_array then begin
        (* Array port: (port (array (rename NAME "NAME[H:L]") SIZE) (direction DIR)) *)
        try
          let _ = Str.search_forward (Str.regexp_case_fold "rename[ \t\n\r]+\\([^ ]+\\)[ \t\n\r]+\"\\([^\"]+\\)\"") port_text 0 in
          let name = Str.matched_group 1 port_text in
          let array_str = Str.matched_group 2 port_text in
          let (base, width) = parse_array_name array_str in
          let _ = Str.search_forward (Str.regexp_case_fold "direction[ \t\n\r]+\\(INPUT\\|OUTPUT\\|INOUT\\)") port_text 0 in
          let dir_str = Str.matched_group 1 port_text in
          let direction = match dir_str with
            | "INPUT" -> Input
            | "OUTPUT" -> Output
            | _ -> Inout
          in
          ports := { name = base; direction; width } :: !ports
        with _ -> ()
      end else if is_renamed then begin
        (* Simple renamed port: (port (rename NAME "NAME[N]") (direction DIR)) *)
        try
          let _ = Str.search_forward (Str.regexp_case_fold "rename[ \t\n\r]+\\([^ ]+\\)[ \t\n\r]+\"\\([^\"]+\\)\"") port_text 0 in
          let orig_name = Str.matched_group 1 port_text in
          let renamed_str = Str.matched_group 2 port_text in
          (* Extract base name from renamed string (e.g., "I0[0]" -> "I0") *)
          let (base, _) = parse_array_name renamed_str in
          let _ = Str.search_forward (Str.regexp_case_fold "direction[ \t\n\r]+\\(INPUT\\|OUTPUT\\|INOUT\\)") port_text 0 in
          let dir_str = Str.matched_group 1 port_text in
          let direction = match dir_str with
            | "INPUT" -> Input
            | "OUTPUT" -> Output
            | _ -> Inout
          in
          (* For bit-select renamed ports like "I0[0]", use base name and width=1 *)
          ports := { name = base; direction; width = 1 } :: !ports
        with _ -> ()
      end else begin
        (* Simple port: (port NAME (direction DIR)) *)
        try
          let _ = Str.search_forward (Str.regexp_case_fold "port[ \t\n\r]+\\([^ (]+\\)") port_text 0 in
          let name = Str.matched_group 1 port_text in
          let _ = Str.search_forward (Str.regexp_case_fold "direction[ \t\n\r]+\\(INPUT\\|OUTPUT\\|INOUT\\)") port_text 0 in
          let dir_str = Str.matched_group 1 port_text in
          let direction = match dir_str with
            | "INPUT" -> Input
            | "OUTPUT" -> Output
            | _ -> Inout
          in
          ports := { name; direction; width = 1 } :: !ports
        with _ -> ()
      end;

      find_port (port_end + 1)
    with Not_found -> ()
  in
  find_port 0;
  List.rev !ports

(* Parse instances from contents section *)
let parse_instances content =
  let instances = ref [] in
  let rec find_instance pos =
    try
      let inst_start = Str.search_forward (Str.regexp_case_fold "(instance ") content pos in

      (* Find cellref and libraryref *)
      let rec find_close p depth =
        if p >= String.length content then p
        else match content.[p] with
          | '(' -> find_close (p + 1) (depth + 1)
          | ')' -> if depth = 1 then p else find_close (p + 1) (depth - 1)
          | _ -> find_close (p + 1) depth
      in
      let inst_end = find_close (inst_start + 1) 1 in
      let inst_text = safe_sub content inst_start (inst_end - inst_start + 1) in

      (* Extract instance name - handle both simple and rename formats.
         In (rename SAFE_ID "orig/name[3]") group 1 is the EDIF-safe identifier
         and group 2 the original; keep BOTH, since the Verilog consumers need
         the former and any join against Vivado's physical database needs the
         latter. *)
      let (inst_name, inst_vivado_name) =
        try
          let _ = Str.search_forward (Str.regexp_case_fold "rename[ \t\n\r]+\\([^ ]+\\)[ \t\n\r]+\"\\([^\"]+\\)\"") inst_text 0 in
          (Str.matched_group 1 inst_text, Str.matched_group 2 inst_text)
        with Not_found ->
          try
            (* Try simple format: (instance name ...) *)
            let _ = Str.search_forward (Str.regexp_case_fold "instance[ \t\n\r]+\\([^ (]+\\)") inst_text 0 in
            let n = Str.matched_group 1 inst_text in (n, n)
          with _ -> ("", "")
      in

      (* Extract cellref and libraryref *)
      if inst_name <> "" then
        (try
          let _ = Str.search_forward (Str.regexp_case_fold "cellref[ \t\n\r]+\\([^ )]+\\)") inst_text 0 in
          let cell_type = Str.matched_group 1 inst_text in
          let _ = Str.search_forward (Str.regexp_case_fold "libraryref[ \t\n\r]+\\([^ )]+\\)") inst_text 0 in
          let library = Str.matched_group 1 inst_text in
          (* Optional (property INIT (string "...")) — truth table for LUTs,
             reset state for FFs.  Vivado puts at most one INIT per inst. *)
          let init =
            try
              let _ = Str.search_forward
                (Str.regexp
                   "property +INIT +(string +\"\\([^\"]*\\)\"")
                inst_text 0 in
              Some (Str.matched_group 1 inst_text)
            with Not_found -> None
          in
          (* Sweep every (property NAME (kind RAW)) on the instance.
             Vivado emits one per cell attribute — DIFF_TERM, IOSTANDARD,
             IBUF_LOW_PWR, CLKCM_CFG, …  Match the name and the kind+raw
             value blob, then post-process to strip wrapping. *)
          let properties =
            let re =
              Str.regexp
                "property +\\([A-Za-z_][A-Za-z0-9_]*\\) +(\\([a-z]+\\) +\\(.*\\))" in
            let strip_value kind raw =
              (* raw still has trailing close-parens from the property's
                 own closing wrap; cut at the first ')' for boolean/integer
                 forms which use (boolean (false)) syntax.                *)
              match kind with
              | "string" ->
                (* raw looks like:  "LVDS")     after the final )
                   strip the leading quote, find the closing quote.        *)
                (try
                   let q1 = String.index raw '"' in
                   let q2 = String.index_from raw (q1 + 1) '"' in
                   String.sub raw (q1 + 1) (q2 - q1 - 1)
                 with Not_found -> raw)
              | "boolean" ->
                (* raw looks like:  (false))    or (true)).                *)
                (try
                   let lp = String.index raw '(' in
                   let rp = String.index_from raw lp ')' in
                   String.sub raw (lp + 1) (rp - lp - 1)
                 with Not_found -> raw)
              | "integer" ->
                (try
                   let rp = String.index raw ')' in
                   String.trim (String.sub raw 0 rp)
                 with Not_found -> raw)
              | _ -> raw
            in
            let rec scan acc pos =
              try
                let _ = Str.search_forward re inst_text pos in
                let nm   = Str.matched_group 1 inst_text in
                let kind = Str.matched_group 2 inst_text in
                let raw  = Str.matched_group 3 inst_text in
                let v    = strip_value kind raw in
                let acc =
                  if nm = "INIT" then acc
                  else (nm, String.uppercase_ascii v) :: acc in
                scan acc (Str.match_end ())
              with Not_found -> List.rev acc
            in
            scan [] 0
          in
          instances := { name = inst_name; vivado_name = inst_vivado_name;
                         cell_type; library; init;
                         properties } :: !instances
        with _ -> ());

      find_instance (inst_end + 1)
    with Not_found -> ()
  in
  find_instance 0;
  List.rev !instances

(* Parse nets from contents section *)
let parse_nets content =
  let nets = ref [] in
  let rec find_net pos =
    try
      let net_start = Str.search_forward (Str.regexp_case_fold "(net ") content pos in
      let rec find_close p depth =
        if p >= String.length content then p
        else match content.[p] with
          | '(' -> find_close (p + 1) (depth + 1)
          | ')' -> if depth = 1 then p else find_close (p + 1) (depth - 1)
          | _ -> find_close (p + 1) depth
      in
      let net_end = find_close (net_start + 1) 1 in
      let net_text = safe_sub content net_start (net_end - net_start + 1) in

      (* Extract net name (may have rename) *)
      let (name, original) =
        try
          let _ = Str.search_forward (Str.regexp_case_fold "rename[ \t\n\r]+\\([^ ]+\\)[ \t\n\r]+\"\\([^\"]+\\)\"") net_text 0 in
          let orig = Str.matched_group 1 net_text in
          let renamed = Str.matched_group 2 net_text in
          (renamed, orig)
        with _ ->
          try
            let _ = Str.search_forward (Str.regexp_case_fold "net[ \t\n\r]+\\([^ )]+\\)") net_text 0 in
            let n = Str.matched_group 1 net_text in
            (n, n)
          with _ -> ("", "")
      in

      if name <> "" then begin
        (* Parse portref connections *)
        let connections = ref [] in
        let rec find_portref p =
          try
            let pr_start = Str.search_forward (Str.regexp_case_fold "(portref ") net_text p in
            (* `(portref (member S 0) (instanceref X))` has a nested paren
               group, so we can't just take the first `)`.  Walk with a
               depth counter to find the matching close. *)
            let rec find_close pos depth =
              if pos >= String.length net_text then pos
              else match net_text.[pos] with
                | '(' -> find_close (pos + 1) (depth + 1)
                | ')' -> if depth = 1 then pos else find_close (pos + 1) (depth - 1)
                | _   -> find_close (pos + 1) depth
            in
            let pr_end = find_close (pr_start + 1) 1 in
            let pr_text = safe_sub net_text pr_start (pr_end - pr_start + 1) in

            (* Check for member (array index) *)
            let (pin, idx) =
              try
                let _ = Str.search_forward (Str.regexp_case_fold "member[ \t\n\r]+\\([^ )]+\\)[ \t\n\r]+\\([0-9]+\\)") pr_text 0 in
                let p = Str.matched_group 1 pr_text in
                let i = int_of_string (Str.matched_group 2 pr_text) in
                (p, Some i)
              with _ ->
                try
                  let _ = Str.search_forward (Str.regexp_case_fold "portref[ \t\n\r]+\\([^ )]+\\)") pr_text 0 in
                  (Str.matched_group 1 pr_text, None)
                with _ -> ("", None)
            in

            (* Check for instanceref *)
            let inst =
              try
                let _ = Str.search_forward (Str.regexp_case_fold "instanceref[ \t\n\r]+\\([^ )]+\\)") pr_text 0 in
                Some (Str.matched_group 1 pr_text)
              with _ -> None
            in

            if pin <> "" then
              connections := { inst; pin; index = idx } :: !connections;

            find_portref (pr_end + 1)
          with Not_found -> ()
        in
        find_portref 0;
        nets := { name; original_name = original; connections = List.rev !connections } :: !nets
      end;

      find_net (net_end + 1)
    with Not_found -> ()
  in
  find_net 0;
  List.rev !nets

(* Main parser *)
let parse_library_cells content =
  let cells = Hashtbl.create 256 in
  let rec find_cell pos =
    try
      let cell_start = Str.search_forward (Str.regexp_case_fold "(cell[ \t\n\r]+\\([^ ]+\\)[ \t\n\r]+(celltype[ \t\n\r]+GENERIC)") content pos in
      let cell_name = Str.matched_group 1 content in

      (* Find matching closing paren *)
      let rec find_close p depth =
        if p >= String.length content then p
        else match content.[p] with
          | '(' -> find_close (p + 1) (depth + 1)
          | ')' -> if depth = 1 then p else find_close (p + 1) (depth - 1)
          | _ -> find_close (p + 1) depth
      in
      let cell_end = find_close (cell_start + 1) 1 in
      let cell_text = safe_sub content cell_start (cell_end - cell_start + 1) in

      (* Check if this is a primitive cell (no contents) or netlist cell (has contents) *)
      let has_contents = try
        let _ = Str.search_forward (Str.regexp_case_fold "(contents") cell_text 0 in
        true
      with Not_found -> false in

      (* Record the INTERFACE of every cell, primitive or not.
         A cell with (contents …) is a macro -- RAM256X1S, RAM32M, RAM64M are
         defined here as RAMS64E/RAMD32 plus F7/F8 muxes.  Skipping those left
         consumers with no interface for the macro at all, so a design that
         instantiates one got a cell with NO connections: 362 ports across 16
         RAM256X1S, 12 RAM32M and 10 RAM64M silently came out empty.
         library_cells is only ever consumed as a port direction/width table,
         never as an "is this a primitive" test, so recording macros too is
         additive.  Parse the interface block alone -- the contents hold
         (portref …) entries that must not be mistaken for declarations. *)
      let iface_text =
        if not has_contents then cell_text
        else
          try
            let i = Str.search_forward (Str.regexp_case_fold "(interface") cell_text 0 in
            let rec fc p depth =
              if p >= String.length cell_text then p
              else match cell_text.[p] with
                | '(' -> fc (p + 1) (depth + 1)
                | ')' -> if depth = 1 then p else fc (p + 1) (depth - 1)
                | _ -> fc (p + 1) depth in
            let e = fc (i + 1) 1 in
            safe_sub cell_text i (e - i + 1)
          with Not_found -> "" in
      if iface_text <> "" then begin
        let ports = parse_ports iface_text in
        if List.length ports > 0 then
          Hashtbl.replace cells cell_name ports
      end;

      find_cell (cell_end + 1)
    with Not_found -> ()
  in
  find_cell 0;
  cells

(* Parse a single cell given its content *)
let parse_cell cell_name cell_content =
  let ports = parse_ports cell_content in
  let instances = parse_instances cell_content in
  let nets = parse_nets cell_content in
  {
    module_name = cell_name;
    ports;
    instances;
    nets;
    library_cells = Hashtbl.create 0;  (* Will be populated separately *)
  }

(* Parse all netlist cells (those with contents sections) *)
let parse_all_netlist_cells content =
  let cells = ref [] in
  let rec find_cell pos =
    try
      let cell_start = Str.search_forward (Str.regexp_case_fold "(cell[ \t\n\r]+\\([^ ]+\\)[ \t\n\r]+(celltype[ \t\n\r]+GENERIC)") content pos in
      let cell_name = Str.matched_group 1 content in

      (* Find matching closing paren *)
      let rec find_close p depth =
        if p >= String.length content then p
        else match content.[p] with
          | '(' -> find_close (p + 1) (depth + 1)
          | ')' -> if depth = 1 then p else find_close (p + 1) (depth - 1)
          | _ -> find_close (p + 1) depth
      in
      let cell_end = find_close (cell_start + 1) 1 in
      (* Clamp to string bounds in case find_close walked off the end
         (malformed input or a regex match that wasn't really a cell). *)
      let cell_end = min cell_end (String.length content - 1) in
      if cell_end <= cell_start then
        failwith (Printf.sprintf
          "edif_parser: find_close walked off end of '(cell %s …)' \
           starting at %d (no matching close paren)" cell_name cell_start)
      else begin
        let cell_text = safe_sub content cell_start (cell_end - cell_start + 1) in

        (* Check if this cell has a contents section (netlist cell) *)
        let has_contents = try
          let _ = Str.search_forward (Str.regexp_case_fold "(contents") cell_text 0 in
          true
        with Not_found -> false in

        if has_contents then begin
          let cell_data = parse_cell cell_name cell_text in
          cells := cell_data :: !cells
        end;

        find_cell (cell_end + 1)
      end
    with Not_found -> ()
  in
  find_cell 0;
  List.rev !cells

let parse_schematic filename =
  let content = read_file filename in

  (* Find main module name from top-level edif declaration *)
  let module_name =
    try
      let _ = Str.search_forward (Str.regexp_case_fold "(edif[ \t\n\r]+\\([^ \n]+\\)") content 0 in
      Str.matched_group 1 content
    with _ -> "top"
  in

  (* Parse library cell definitions from entire file *)
  let library_cells = parse_library_cells content in

  (* Find the main cell definition (cell with module_name in work library) *)
  let cell_start =
    try
      Str.search_forward (Str.regexp_case_fold (Printf.sprintf "(cell +%s +(celltype +GENERIC)" module_name)) content 0
    with Not_found -> 0
  in

  (* Find the end of this cell *)
  let rec find_cell_end pos depth =
    if pos >= String.length content then pos
    else match content.[pos] with
      | '(' -> find_cell_end (pos + 1) (depth + 1)
      | ')' -> if depth = 1 then pos else find_cell_end (pos + 1) (depth - 1)
      | _ -> find_cell_end (pos + 1) depth
  in
  let cell_end = find_cell_end cell_start 0 in
  let cell_content = safe_sub content cell_start (cell_end - cell_start + 1) in

  (* Extract interface section from main cell only *)
  let ports = parse_ports cell_content in

  (* Extract contents section from main cell only *)
  let instances = parse_instances cell_content in
  let nets = parse_nets cell_content in

  {
    module_name;
    ports;
    instances;
    nets;
    library_cells;
  }

(* Parse library cell definitions *)

(* Statistics *)
let print_stats edif =
  Printf.printf "Module: %s\n" edif.module_name;
  Printf.printf "  Ports: %d\n" (List.length edif.ports);
  Printf.printf "  Instances: %d\n" (List.length edif.instances);
  Printf.printf "  Nets: %d\n" (List.length edif.nets)
