(* Minimal gate-level Verilog parser for the structural-only
   subset emitted by Synth_mac (and, by virtue of how Yosys writes
   its post-techmap netlists, the same shape OpenROAD writes back
   after ECO).

   Grammar:
     file       ::= module*
     module     ::= 'module' ID '(' port_list ')' ';' decls cells 'endmodule'
     port_list  ::= ID (',' ID)*
     decls      ::= ('input' | 'output' | 'wire') id_list ';'  *
     id_list    ::= ID (',' ID)*
     cells      ::= cell_inst*
     cell_inst  ::= ID ID '(' conn_list ')' ';'
     conn_list  ::= ('.' ID '(' net ')' (',' '.' ID '(' net ')')* )?
     net        ::= ID | NUMBER 'b' [01zxZX01] | NUMBER ''b' [01]
                  ;
   Comments: // line, /* block */.

   No expressions, no procedural blocks, no parameters — that's
   exactly what the OpenROAD-flow yosys output gives us, so we
   don't need anything more. *)

type port_kind = Input | Output | Wire

type port = { name : string; kind : port_kind }

type cell_inst = {
  cell_type : string;
  inst_name : string;
  conns     : (string * string) list;   (* (pin, net_or_const) *)
}

type vmodule = {
  name    : string;
  ports   : port list;
  cells   : cell_inst list;
}

(* ── Tokenizer ─────────────────────────────────────────────── *)

type token =
  | T_id of string
  | T_num of string
  | T_lparen | T_rparen
  | T_comma | T_dot | T_semi
  | T_eof

let is_id_start c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
                     || c = '_' || c = '\\'
let is_id_cont c = is_id_start c || (c >= '0' && c <= '9')
                    || c = '[' || c = ']' || c = '\''

let tokenize src =
  let n = String.length src in
  let i = ref 0 in
  let tok = ref [] in
  let push t = tok := t :: !tok in
  while !i < n do
    let c = src.[!i] in
    if c = ' ' || c = '\t' || c = '\r' || c = '\n' then incr i
    else if c = '/' && !i + 1 < n && src.[!i+1] = '/' then begin
      while !i < n && src.[!i] <> '\n' do incr i done
    end
    else if c = '/' && !i + 1 < n && src.[!i+1] = '*' then begin
      i := !i + 2;
      while !i + 1 < n
            && not (src.[!i] = '*' && src.[!i+1] = '/') do incr i done;
      if !i + 1 < n then i := !i + 2
    end
    else if c = '(' then (push T_lparen; incr i)
    else if c = ')' then (push T_rparen; incr i)
    else if c = ',' then (push T_comma;  incr i)
    else if c = '.' then (push T_dot;    incr i)
    else if c = ';' then (push T_semi;   incr i)
    else if (c >= '0' && c <= '9')
         || (c = '\'' && !i + 1 < n
             && (src.[!i+1] = 'b' || src.[!i+1] = 'h' || src.[!i+1] = 'd')) then begin
      let start = !i in
      while !i < n
            && (let d = src.[!i] in
                (d >= '0' && d <= '9') || d = '\''
                || d = 'b' || d = 'h' || d = 'd'
                || d = 'x' || d = 'z' || d = 'X' || d = 'Z'
                || (d >= 'a' && d <= 'f') || (d >= 'A' && d <= 'F'))
      do incr i done;
      push (T_num (String.sub src start (!i - start)))
    end
    else if is_id_start c then begin
      let start = !i in
      (* escaped Verilog name: \... up to whitespace *)
      if c = '\\' then begin
        incr i;
        while !i < n && src.[!i] <> ' ' && src.[!i] <> '\t'
              && src.[!i] <> '\n' && src.[!i] <> '\r' do incr i done
      end else
        while !i < n && is_id_cont src.[!i] do incr i done;
      push (T_id (String.sub src start (!i - start)))
    end
    else
      (* unknown character: skip (the structural subset is
         tolerant of stray punctuation in some Verilog dialects) *)
      incr i
  done;
  push T_eof;
  Array.of_list (List.rev !tok)

(* ── Parser ─────────────────────────────────────────────────── *)

exception Parse_err of string * int

let pp_tok = function
  | T_id s    -> Printf.sprintf "id:%s" s
  | T_num s   -> Printf.sprintf "num:%s" s
  | T_lparen  -> "("    | T_rparen  -> ")"
  | T_comma   -> ","    | T_dot     -> "."
  | T_semi    -> ";"
  | T_eof     -> "<eof>"

let parse tokens =
  let pos = ref 0 in
  let n = Array.length tokens in
  let peek () = if !pos < n then tokens.(!pos) else T_eof in
  let advance () = incr pos in
  let expect_id () =
    match peek () with
    | T_id s -> advance (); s
    | t -> raise (Parse_err
                    (Printf.sprintf "expected id, got %s" (pp_tok t), !pos))
  in
  let expect t =
    if peek () = t then advance ()
    else raise (Parse_err
                  (Printf.sprintf "expected %s, got %s"
                     (pp_tok t) (pp_tok (peek ())), !pos))
  in
  let opt t = if peek () = t then (advance (); true) else false in
  let parse_id_list_until terminator =
    let acc = ref [] in
    let rec loop () =
      if peek () = terminator then ()
      else begin
        (match peek () with
         | T_id s -> acc := s :: !acc; advance ()
         | _ -> advance ()); (* tolerate odd tokens in port lists *)
        if peek () = T_comma then (advance (); loop ())
        else if peek () = terminator then ()
        else loop ()
      end
    in
    loop ();
    List.rev !acc
  in
  let parse_module () =
    expect (T_id "module");
    let mname = expect_id () in
    expect T_lparen;
    let port_names = parse_id_list_until T_rparen in
    expect T_rparen;
    expect T_semi;
    let port_kinds = Hashtbl.create (List.length port_names) in
    List.iter (fun nm -> Hashtbl.replace port_kinds nm Wire) port_names;
    let cells = ref [] in
    let rec loop () =
      match peek () with
      | T_id "endmodule" -> advance ()
      | T_id "input"  ->
          advance ();
          let names = parse_id_list_until T_semi in
          List.iter (fun n -> Hashtbl.replace port_kinds n Input) names;
          expect T_semi; loop ()
      | T_id "output" ->
          advance ();
          let names = parse_id_list_until T_semi in
          List.iter (fun n -> Hashtbl.replace port_kinds n Output) names;
          expect T_semi; loop ()
      | T_id "wire"   ->
          advance ();
          let _ = parse_id_list_until T_semi in
          expect T_semi; loop ()
      | T_id cell_type ->
          advance ();
          let inst = expect_id () in
          expect T_lparen;
          let conns = ref [] in
          let parsing = ref true in
          while !parsing do
            match peek () with
            | T_rparen -> parsing := false
            | T_dot ->
                advance ();
                let pin = expect_id () in
                expect T_lparen;
                let net = match peek () with
                  | T_id s  -> advance (); s
                  | T_num s -> advance (); s
                  | _ -> "" in
                expect T_rparen;
                conns := (pin, net) :: !conns;
                let _ = opt T_comma in ()
            | T_eof -> parsing := false
            | _ -> advance ()
          done;
          expect T_rparen;
          expect T_semi;
          cells := { cell_type; inst_name = inst;
                     conns = List.rev !conns } :: !cells;
          loop ()
      | T_eof -> ()
      | t -> raise (Parse_err
                      (Printf.sprintf "unexpected %s in module body"
                         (pp_tok t), !pos))
    in
    loop ();
    let ports =
      List.map (fun nm ->
        { name = nm;
          kind = (try Hashtbl.find port_kinds nm with Not_found -> Wire) })
        port_names in
    { name = mname; ports; cells = List.rev !cells }
  in
  let rec all_modules acc =
    match peek () with
    | T_eof -> List.rev acc
    | _ -> all_modules (parse_module () :: acc)
  in
  all_modules []

let parse_string src =
  parse (tokenize src)

let parse_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let buf = Bytes.create n in
  really_input ic buf 0 n;
  close_in ic;
  parse_string (Bytes.unsafe_to_string buf)

(* ── Convert to fanout edges + placement-style records ───────── *)

(* For round-tripping with placement_timing, callers need a
   driver-to-loads net map.  We compute it from the cell
   instance pin lists, treating any pin whose name matches a
   declared output of the cell type's Liberty as a driver.
   Without Liberty we infer driver/load from a small static
   table for the cells we synthesise (AND2_X1, XOR2_X1, etc).
   Pass [~pin_dir] to override with a real LEF table. *)

type pin_dir = Pin_in | Pin_out

(* Pin-direction table type kept abstract to the caller — fill it
   from the Liberty (Cell_delay.pin_dir_table) for any real flow.
   The empty table treats every pin as input, which is the correct
   degenerate behaviour: a cell with all-input pins drives no nets,
   so [build_nets] returns the netlist with nobody on the driver
   side and the caller can detect the missing Liberty. *)
let empty_pin_dir : (string * string, pin_dir) Hashtbl.t =
  Hashtbl.create 1

let build_nets ~pin_dir (m : vmodule) =
  let net_pins = Hashtbl.create 256 in
  List.iter (fun c ->
    List.iter (fun (pin, net) ->
      let dir =
        try Hashtbl.find pin_dir (c.cell_type, pin)
        with Not_found -> Pin_in in
      let cur = try Hashtbl.find net_pins net with Not_found -> ([], []) in
      let (drivers, loads) = cur in
      let entry = (c.inst_name, pin) in
      Hashtbl.replace net_pins net
        (if dir = Pin_out then (entry :: drivers, loads)
         else (drivers, entry :: loads))) c.conns) m.cells;
  net_pins
