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
  cell_type: string;
  library: string;
  init: string option;  (* (property INIT (string "...")) — LUT truth table,
                           FF reset state, etc.  Raw verilog literal form
                           ("64'hABCDEF...", "1'b0", etc.); decoded by the
                           consumer.  None when no INIT property present. *)
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
      let port_start = Str.search_forward (Str.regexp "(port ") content pos in
      (* Find matching closing paren *)
      let rec find_close p depth =
        if p >= String.length content then p
        else match content.[p] with
          | '(' -> find_close (p + 1) (depth + 1)
          | ')' -> if depth = 1 then p else find_close (p + 1) (depth - 1)
          | _ -> find_close (p + 1) depth
      in
      let port_end = find_close (port_start + 1) 1 in
      let port_text = String.sub content port_start (port_end - port_start + 1) in

      (* Parse port - three cases:
         1. Array with rename: (port (array (rename NAME "NAME[H:L]") SIZE) (direction DIR))
         2. Simple rename: (port (rename NAME "NAME[N]") (direction DIR))
         3. Simple port: (port NAME (direction DIR))
      *)
      let is_array = String.contains port_text '(' &&
                     Str.string_match (Str.regexp ".*array.*rename") port_text 0 in
      let is_renamed = String.contains port_text '(' &&
                       Str.string_match (Str.regexp ".*port +(rename") port_text 0 in

      if is_array then begin
        (* Array port: (port (array (rename NAME "NAME[H:L]") SIZE) (direction DIR)) *)
        try
          let _ = Str.search_forward (Str.regexp "rename +\\([^ ]+\\) +\"\\([^\"]+\\)\"") port_text 0 in
          let name = Str.matched_group 1 port_text in
          let array_str = Str.matched_group 2 port_text in
          let (base, width) = parse_array_name array_str in
          let _ = Str.search_forward (Str.regexp "direction +\\(INPUT\\|OUTPUT\\|INOUT\\)") port_text 0 in
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
          let _ = Str.search_forward (Str.regexp "rename +\\([^ ]+\\) +\"\\([^\"]+\\)\"") port_text 0 in
          let orig_name = Str.matched_group 1 port_text in
          let renamed_str = Str.matched_group 2 port_text in
          (* Extract base name from renamed string (e.g., "I0[0]" -> "I0") *)
          let (base, _) = parse_array_name renamed_str in
          let _ = Str.search_forward (Str.regexp "direction +\\(INPUT\\|OUTPUT\\|INOUT\\)") port_text 0 in
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
          let _ = Str.search_forward (Str.regexp "port +\\([^ (]+\\)") port_text 0 in
          let name = Str.matched_group 1 port_text in
          let _ = Str.search_forward (Str.regexp "direction +\\(INPUT\\|OUTPUT\\|INOUT\\)") port_text 0 in
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
      let inst_start = Str.search_forward (Str.regexp "(instance ") content pos in

      (* Find cellref and libraryref *)
      let rec find_close p depth =
        if p >= String.length content then p
        else match content.[p] with
          | '(' -> find_close (p + 1) (depth + 1)
          | ')' -> if depth = 1 then p else find_close (p + 1) (depth - 1)
          | _ -> find_close (p + 1) depth
      in
      let inst_end = find_close (inst_start + 1) 1 in
      let inst_text = String.sub content inst_start (inst_end - inst_start + 1) in

      (* Extract instance name - handle both simple and rename formats *)
      let inst_name =
        try
          (* Try renamed format: (instance (rename old "new") ...) *)
          let _ = Str.search_forward (Str.regexp "rename +\\([^ ]+\\) +\"\\([^\"]+\\)\"") inst_text 0 in
          Str.matched_group 1 inst_text  (* Use original name *)
        with Not_found ->
          try
            (* Try simple format: (instance name ...) *)
            let _ = Str.search_forward (Str.regexp "instance +\\([^ (]+\\)") inst_text 0 in
            Str.matched_group 1 inst_text
          with _ -> ""
      in

      (* Extract cellref and libraryref *)
      if inst_name <> "" then
        (try
          let _ = Str.search_forward (Str.regexp "cellref +\\([^ )]+\\)") inst_text 0 in
          let cell_type = Str.matched_group 1 inst_text in
          let _ = Str.search_forward (Str.regexp "libraryref +\\([^ )]+\\)") inst_text 0 in
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
          instances := { name = inst_name; cell_type; library; init } :: !instances
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
      let net_start = Str.search_forward (Str.regexp "(net ") content pos in
      let rec find_close p depth =
        if p >= String.length content then p
        else match content.[p] with
          | '(' -> find_close (p + 1) (depth + 1)
          | ')' -> if depth = 1 then p else find_close (p + 1) (depth - 1)
          | _ -> find_close (p + 1) depth
      in
      let net_end = find_close (net_start + 1) 1 in
      let net_text = String.sub content net_start (net_end - net_start + 1) in

      (* Extract net name (may have rename) *)
      let (name, original) =
        try
          let _ = Str.search_forward (Str.regexp "rename +\\([^ ]+\\) +\"\\([^\"]+\\)\"") net_text 0 in
          let orig = Str.matched_group 1 net_text in
          let renamed = Str.matched_group 2 net_text in
          (renamed, orig)
        with _ ->
          try
            let _ = Str.search_forward (Str.regexp "net +\\([^ )]+\\)") net_text 0 in
            let n = Str.matched_group 1 net_text in
            (n, n)
          with _ -> ("", "")
      in

      if name <> "" then begin
        (* Parse portref connections *)
        let connections = ref [] in
        let rec find_portref p =
          try
            let pr_start = Str.search_forward (Str.regexp "(portref ") net_text p in
            let pr_end = String.index_from net_text pr_start ')' in
            let pr_text = String.sub net_text pr_start (pr_end - pr_start + 1) in

            (* Check for member (array index) *)
            let (pin, idx) =
              try
                let _ = Str.search_forward (Str.regexp "member +\\([^ )]+\\) +\\([0-9]+\\)") pr_text 0 in
                let p = Str.matched_group 1 pr_text in
                let i = int_of_string (Str.matched_group 2 pr_text) in
                (p, Some i)
              with _ ->
                try
                  let _ = Str.search_forward (Str.regexp "portref +\\([^ )]+\\)") pr_text 0 in
                  (Str.matched_group 1 pr_text, None)
                with _ -> ("", None)
            in

            (* Check for instanceref *)
            let inst =
              try
                let _ = Str.search_forward (Str.regexp "instanceref +\\([^ )]+\\)") pr_text 0 in
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
      let cell_start = Str.search_forward (Str.regexp "(cell +\\([^ ]+\\) +(celltype +GENERIC)") content pos in
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
      let cell_text = String.sub content cell_start (cell_end - cell_start + 1) in

      (* Check if this is a primitive cell (no contents) or netlist cell (has contents) *)
      let has_contents = try
        let _ = Str.search_forward (Str.regexp "(contents") cell_text 0 in
        true
      with Not_found -> false in

      (* Only parse ports for primitive cells (no contents section) *)
      if not has_contents then begin
        let ports = parse_ports cell_text in
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
      let cell_start = Str.search_forward (Str.regexp "(cell +\\([^ ]+\\) +(celltype +GENERIC)") content pos in
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
      let cell_text = String.sub content cell_start (cell_end - cell_start + 1) in

      (* Check if this cell has a contents section (netlist cell) *)
      let has_contents = try
        let _ = Str.search_forward (Str.regexp "(contents") cell_text 0 in
        true
      with Not_found -> false in

      if has_contents then begin
        let cell_data = parse_cell cell_name cell_text in
        cells := cell_data :: !cells
      end;

      find_cell (cell_end + 1)
    with Not_found -> ()
  in
  find_cell 0;
  List.rev !cells

let parse_schematic filename =
  let content = read_file filename in

  (* Find main module name from top-level edif declaration *)
  let module_name =
    try
      let _ = Str.search_forward (Str.regexp "(edif +\\([^ \n]+\\)") content 0 in
      Str.matched_group 1 content
    with _ -> "top"
  in

  (* Parse library cell definitions from entire file *)
  let library_cells = parse_library_cells content in

  (* Find the main cell definition (cell with module_name in work library) *)
  let cell_start =
    try
      Str.search_forward (Str.regexp (Printf.sprintf "(cell +%s +(celltype +GENERIC)" module_name)) content 0
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
  let cell_content = String.sub content cell_start (cell_end - cell_start + 1) in

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
