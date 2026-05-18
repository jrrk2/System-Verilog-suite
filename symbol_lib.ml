(* symbol_lib.ml — Synopsys-style symbol library (.slib) reader, plus
   an auto-generated fallback that synthesises a rectangular symbol
   from a Liberty cell's pin list.

   The grammar is the source form (text .slib, not the compiled .sdb):

     symbol_library (NAME) {
       attr ( arg, ... ) ;
       symbol (CELL) {
         bbox  ( x1, y1, x2, y2 ) ;
         line  ( x1, y1, x2, y2 ) ;
         arc   ( cx, cy, sx, sy, ex, ey ) ;
         circle( cx, cy, r ) ;
         pin (NAME) {
           direction (input|output|inout) ;
           line ( x1, y1, x2, y2 ) ;
           connect_point ( x, y ) ;
         }
       }
     }

   Comments are C-style. Statement-terminating ';' is optional. *)

type point = float * float

type prim =
  | PLine   of point * point
  | PArc    of point * point * point  (* centre, start, end *)
  | PCircle of point * float

type pin_dir = PinIn | PinOut | PinInOut

type pin = {
  pin_name    : string;
  pin_dir     : pin_dir;
  pin_prims   : prim list;
  pin_connect : point;     (* the single connection point *)
}

type symbol = {
  sym_name : string;
  sym_bbox : float * float * float * float;   (* x1, y1, x2, y2 *)
  sym_prims : prim list;
  sym_pins  : pin list;
}

type library = (string, symbol) Hashtbl.t

(* ---------------- tokeniser ---------------- *)

type tok =
  | LParen | RParen | LBrace | RBrace
  | Comma | Semi
  | Ident  of string
  | Number of float
  | Str    of string
  | EOF

let strip_comments s =
  (* Replace each /* ... */ with equivalent spaces so positions stay
     comparable. Also strip // line-comments. *)
  let b = Buffer.create (String.length s) in
  let i = ref 0 and n = String.length s in
  while !i < n do
    if !i + 1 < n && s.[!i] = '/' && s.[!i + 1] = '*' then begin
      Buffer.add_char b ' '; Buffer.add_char b ' ';
      i := !i + 2;
      (try
         while !i + 1 < n &&
               not (s.[!i] = '*' && s.[!i + 1] = '/') do
           Buffer.add_char b (if s.[!i] = '\n' then '\n' else ' ');
           incr i
         done;
         if !i + 1 < n then (Buffer.add_char b ' '; Buffer.add_char b ' '; i := !i + 2)
       with _ -> ())
    end else if !i + 1 < n && s.[!i] = '/' && s.[!i + 1] = '/' then begin
      while !i < n && s.[!i] <> '\n' do
        Buffer.add_char b ' '; incr i
      done
    end else begin
      Buffer.add_char b s.[!i]; incr i
    end
  done;
  Buffer.contents b

let tokenise s : tok list =
  let s = strip_comments s in
  let n = String.length s in
  let i = ref 0 in
  let out = ref [] in
  let is_ident0 c =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
  in
  let is_identn c =
    is_ident0 c || (c >= '0' && c <= '9') || c = '.' || c = '$'
  in
  let is_numeric0 c =
    (c >= '0' && c <= '9') || c = '-' || c = '+' || c = '.'
  in
  while !i < n do
    let c = s.[!i] in
    if c = ' ' || c = '\t' || c = '\n' || c = '\r' then incr i
    else if c = '(' then (out := LParen :: !out; incr i)
    else if c = ')' then (out := RParen :: !out; incr i)
    else if c = '{' then (out := LBrace :: !out; incr i)
    else if c = '}' then (out := RBrace :: !out; incr i)
    else if c = ',' then (out := Comma  :: !out; incr i)
    else if c = ';' then (out := Semi   :: !out; incr i)
    else if c = '"' then begin
      let j = ref (!i + 1) in
      while !j < n && s.[!j] <> '"' do incr j done;
      out := Str (String.sub s (!i + 1) (!j - !i - 1)) :: !out;
      i := !j + 1
    end else if is_ident0 c then begin
      let j = ref !i in
      while !j < n && is_identn s.[!j] do incr j done;
      out := Ident (String.sub s !i (!j - !i)) :: !out;
      i := !j
    end else if is_numeric0 c then begin
      let j = ref !i in
      (* Allow leading sign then digits/'.'/e. *)
      if s.[!j] = '+' || s.[!j] = '-' then incr j;
      while !j < n &&
            (let c = s.[!j] in
             (c >= '0' && c <= '9') || c = '.' || c = 'e' || c = 'E' || c = '+' || c = '-')
      do incr j done;
      let lex = String.sub s !i (!j - !i) in
      (try out := Number (float_of_string lex) :: !out
       with _ -> out := Ident lex :: !out);
      i := !j
    end else
      (* Unknown char — skip rather than fail. *)
      incr i
  done;
  List.rev (EOF :: !out)

(* ---------------- recursive-descent parser ---------------- *)

exception Parse_error of string

let parse_lib (toks0 : tok list) : library =
  let lib : library = Hashtbl.create 64 in
  let toks = ref toks0 in
  let peek () = match !toks with [] -> EOF | t :: _ -> t in
  let advance () = match !toks with [] -> () | _ :: r -> toks := r in
  let eat t =
    if peek () = t then advance ()
    else raise (Parse_error
                  (Printf.sprintf "expected token, got %s"
                     (match peek () with
                      | LParen -> "(" | RParen -> ")"
                      | LBrace -> "{" | RBrace -> "}"
                      | Comma -> "," | Semi -> ";"
                      | Ident s -> "ident " ^ s
                      | Number f -> "num " ^ string_of_float f
                      | Str s -> "\"" ^ s ^ "\""
                      | EOF -> "<eof>")))
  in
  let expect_ident () = match peek () with
    | Ident s -> advance (); s
    | _ -> raise (Parse_error "expected identifier")
  in
  let expect_number () = match peek () with
    | Number f -> advance (); f
    | Ident s ->
        (* tolerate bare integers that lexed as identifiers *)
        (try let f = float_of_string s in advance (); f
         with _ -> raise (Parse_error ("expected number, got ident " ^ s)))
    | _ -> raise (Parse_error "expected number")
  in
  (* Read a comma-separated list of numbers inside the parens already
     consumed.  Stops at the matching ')'. *)
  let read_num_paren () =
    eat LParen;
    let xs = ref [] in
    let rec loop () =
      match peek () with
      | RParen -> ()
      | Comma  -> advance (); loop ()
      | _ -> xs := expect_number () :: !xs; loop ()
    in loop ();
    eat RParen;
    List.rev !xs
  in
  (* Same but the first token inside parens is an identifier
     (the "head" — e.g. the cell name in `symbol (AND2)`). *)
  let read_ident_paren () =
    eat LParen;
    let s = expect_ident () in
    eat RParen;
    s
  in
  let skip_paren_args () =
    (* Generic attribute-args skip — accept anything until the matching ')' *)
    eat LParen;
    let depth = ref 1 in
    while !depth > 0 do
      match peek () with
      | LParen -> incr depth; advance ()
      | RParen -> decr depth; advance ()
      | EOF -> raise (Parse_error "unterminated (")
      | _ -> advance ()
    done
  in
  let opt_semi () = if peek () = Semi then advance () in
  let opt_block () =
    (* Skip a {…} block if present; used for ignoring unknown attributes
       like `technology { ... }`. *)
    if peek () = LBrace then begin
      eat LBrace;
      let depth = ref 1 in
      while !depth > 0 do
        match peek () with
        | LBrace -> incr depth; advance ()
        | RBrace -> decr depth; advance ()
        | EOF -> raise (Parse_error "unterminated {")
        | _ -> advance ()
      done
    end
  in
  let parse_prim kw : prim option =
    match kw with
    | "line" ->
        (match read_num_paren () with
         | [x1; y1; x2; y2] -> Some (PLine ((x1, y1), (x2, y2)))
         | _ -> None)
    | "arc" ->
        (match read_num_paren () with
         | [cx; cy; sx; sy; ex; ey] ->
             Some (PArc ((cx, cy), (sx, sy), (ex, ey)))
         | _ -> None)
    | "circle" ->
        (match read_num_paren () with
         | [cx; cy; r] -> Some (PCircle ((cx, cy), r))
         | _ -> None)
    | _ -> None
  in
  let parse_pin () =
    let name = read_ident_paren () in
    let dir = ref PinIn in
    let prims = ref [] in
    let connect = ref (0.0, 0.0) in
    eat LBrace;
    let rec loop () =
      match peek () with
      | RBrace -> ()
      | Ident "direction" ->
          advance ();
          eat LParen;
          let s = expect_ident () in
          eat RParen; opt_semi ();
          dir := (match String.lowercase_ascii s with
                  | "input" | "in" -> PinIn
                  | "output" | "out" -> PinOut
                  | "inout" | "bidir" | "bidirectional" -> PinInOut
                  | _ -> PinIn);
          loop ()
      | Ident "connect_point" ->
          advance ();
          (match read_num_paren () with
           | [x; y] -> connect := (x, y)
           | _ -> ());
          opt_semi (); loop ()
      | Ident kw when kw = "line" || kw = "arc" || kw = "circle" ->
          advance ();
          (match parse_prim kw with
           | Some p -> prims := p :: !prims
           | None -> ());
          opt_semi (); loop ()
      | Ident _ ->
          advance ();
          if peek () = LParen then skip_paren_args ();
          opt_block ();
          opt_semi (); loop ()
      | Semi -> advance (); loop ()
      | _ -> raise (Parse_error "unexpected token in pin body")
    in loop ();
    eat RBrace;
    { pin_name = name; pin_dir = !dir;
      pin_prims = List.rev !prims; pin_connect = !connect }
  in
  let parse_symbol () =
    let cell = read_ident_paren () in
    let bbox = ref (0.0, 0.0, 40.0, 40.0) in
    let prims = ref [] in
    let pins = ref [] in
    eat LBrace;
    let rec loop () =
      match peek () with
      | RBrace -> ()
      | Ident "bbox" ->
          advance ();
          (match read_num_paren () with
           | [x1; y1; x2; y2] -> bbox := (x1, y1, x2, y2)
           | _ -> ());
          opt_semi (); loop ()
      | Ident kw when kw = "line" || kw = "arc" || kw = "circle" ->
          advance ();
          (match parse_prim kw with
           | Some p -> prims := p :: !prims
           | None -> ());
          opt_semi (); loop ()
      | Ident "pin" ->
          advance ();
          pins := parse_pin () :: !pins;
          opt_semi (); loop ()
      | Ident _ ->
          advance ();
          if peek () = LParen then skip_paren_args ();
          opt_block ();
          opt_semi (); loop ()
      | Semi -> advance (); loop ()
      | _ -> raise (Parse_error "unexpected token in symbol body")
    in loop ();
    eat RBrace;
    Hashtbl.replace lib cell
      { sym_name = cell; sym_bbox = !bbox;
        sym_prims = List.rev !prims;
        sym_pins  = List.rev !pins }
  in
  (* Top: `symbol_library ( name ) { … }` — but tolerate slibs that
     are just a flat list of symbols. *)
  (match peek () with
   | Ident "symbol_library" ->
       advance ();
       let _ = read_ident_paren () in
       eat LBrace;
       let rec loop () =
         match peek () with
         | RBrace -> ()
         | Ident "symbol" -> advance (); parse_symbol (); loop ()
         | Ident _ ->
             advance ();
             if peek () = LParen then skip_paren_args ();
             opt_block ();
             opt_semi (); loop ()
         | Semi -> advance (); loop ()
         | _ -> raise (Parse_error "unexpected token at library level")
       in loop ();
       eat RBrace
   | _ ->
       let rec loop () =
         match peek () with
         | EOF -> ()
         | Ident "symbol" -> advance (); parse_symbol (); loop ()
         | Ident _ ->
             advance ();
             if peek () = LParen then skip_paren_args ();
             opt_block ();
             opt_semi (); loop ()
         | Semi -> advance (); loop ()
         | _ -> advance (); loop ()
       in loop ());
  lib

let parse_file (path : string) : library =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  parse_lib (tokenise s)

let empty () : library = Hashtbl.create 16

let merge ~into other =
  Hashtbl.iter (fun k v -> Hashtbl.replace into k v) other

let find_opt (lib : library) name : symbol option =
  Hashtbl.find_opt lib name

(* ---------------- auto-generation from Liberty pins ----------------

   When the user hasn't supplied a .slib we still need a symbol to
   draw.  We classify the cell name (vendor prefix and drive-strength
   suffix stripped) into a small set of gate kinds and emit IEEE-91
   distinctive-shape symbols: D for AND, shield for OR, triangle for
   buffers/inverters, trapezoid for MUX.  Anything we can't classify
   (flip-flops, complex cells, hierarchical instances) falls back to a
   rectangle.                                                          *)

let _pin_stub_len = 10.0

type gate_kind =
  | GK_And | GK_Nand | GK_Or | GK_Nor | GK_Xor | GK_Xnor
  | GK_Inv | GK_Buf | GK_Mux | GK_FF | GK_Generic

(* Drop everything up to the last "__" if present (sky130_fd_sc_hd__... ). *)
let drop_vendor_prefix s =
  let len = String.length s in
  let rec find i =
    if i + 1 >= len then 0
    else if s.[i] = '_' && s.[i+1] = '_' then i + 2
    else find (i+1) in
  let i = find 0 in
  if i >= len then s else String.sub s i (len - i)

(* Strip trailing drive-strength suffix.  Two recognised patterns:
     _X<digits>  / _x<digits>
     _<digits>                                                          *)
let strip_drive_suffix s =
  let len = String.length s in
  let i = ref len in
  while !i > 0 && (let c = s.[!i - 1] in c >= '0' && c <= '9') do decr i done;
  if !i = len then s
  else
    let after_digits = !i in
    let i' =
      if !i > 0 && (let c = s.[!i - 1] in c = 'x' || c = 'X') then !i - 1
      else !i in
    if i' > 0 && s.[i' - 1] = '_' then String.sub s 0 (i' - 1)
    else if after_digits = !i && i' = !i && !i > 0 && s.[!i - 1] = '_'
    then String.sub s 0 (!i - 1)
    else s

let try_classify n : gate_kind option =
  let has pfx =
    let lp = String.length pfx in
    String.length n >= lp && String.sub n 0 lp = pfx in
  if has "xnor" then Some GK_Xnor
  else if has "nand" then Some GK_Nand
  else if has "xor"  then Some GK_Xor
  else if has "nor"  then Some GK_Nor
  else if has "and"  then Some GK_And
  else if has "or"   then Some GK_Or
  else if has "inv" || has "not" then Some GK_Inv
  else if has "clkbuf" || has "buf" then Some GK_Buf
  else if has "mux" || has "mx" then Some GK_Mux
  else if has "dffrs" || has "sdffsr" || has "sdff"
       || has "dffs"  || has "dffr"   || has "dff"
       || has "fdre"  || has "fdce"
       || has "latch" || has "dlatch" then Some GK_FF
  else None

let classify_cell name : gate_kind =
  let n0 = drop_vendor_prefix (String.lowercase_ascii name) in
  match try_classify n0 with
  | Some k -> k
  | None ->
      let n1 = strip_drive_suffix n0 in
      if n1 = n0 then GK_Generic
      else (match try_classify n1 with
            | Some k -> k
            | None -> GK_Generic)

(* Helpers used by every shape generator.  Each input pin gets a
   slightly different stub length so its connect-point sits at a
   unique x — that way the vertical jog from a row channel to the pin
   doesn't share an x with any sibling input's jog, avoiding the
   "two-wires-on-top-of-each-other" crossing at multi-input gates.   *)
let _pin_stub_step = 6.0
let stub_len_of i = _pin_stub_len +. float_of_int i *. _pin_stub_step

let stub_in ?(i = 0) name (xb, y) =
  let len = stub_len_of i in
  { pin_name = name; pin_dir = PinIn;
    pin_prims = [ PLine ((xb -. len, y), (xb, y)) ];
    pin_connect = (xb -. len, y) }

let stub_out name (xb, y) =
  { pin_name = name; pin_dir = PinOut;
    pin_prims = [ PLine ((xb, y), (xb +. _pin_stub_len, y)) ];
    pin_connect = (xb +. _pin_stub_len, y) }

(* Distribute [n] inputs vertically inside a body of height [h]; the
   topmost pin is at index 0 (highest y) — matching the natural reading
   order. *)
let pin_y h n i =
  if n <= 1 then h /. 2.0
  else h *. (1.0 -. (float_of_int i +. 0.5) /. float_of_int n)

(* Approximate the arc passing through (x1,y1)→(x2,y2)→(x3,y3) as
   [segments] short PLines.  Using polylines instead of PArc means the
   renderer never has to decide arc direction or radius — every gate
   outline is composed only of strictly straight segments, so any
   non-axis-aligned visible line is by construction part of a gate
   body, not a wire. *)
let arc_polyline ?(segments = 14) (x1, y1) (x2, y2) (x3, y3) : prim list =
  let ax = x2 -. x1 and ay = y2 -. y1 in
  let bx = x3 -. x1 and by = y3 -. y1 in
  let d  = 2.0 *. (ax *. by -. ay *. bx) in
  if abs_float d < 1e-9 then
    [ PLine ((x1, y1), (x3, y3)) ]
  else
    let a2 = ax *. ax +. ay *. ay in
    let b2 = bx *. bx +. by *. by in
    let cx = x1 +. (by *. a2 -. ay *. b2) /. d in
    let cy = y1 +. (ax *. b2 -. bx *. a2) /. d in
    let r  = sqrt ((x1 -. cx) ** 2.0 +. (y1 -. cy) ** 2.0) in
    let a1' = atan2 (y1 -. cy) (x1 -. cx) in
    let am' = atan2 (y2 -. cy) (x2 -. cx) in
    let ae' = atan2 (y3 -. cy) (x3 -. cx) in
    let pi2 = 2.0 *. 3.14159265358979 in
    let norm a = let a = mod_float a pi2 in if a < 0.0 then a +. pi2 else a in
    let n1 = norm a1' and nm = norm am' and ne = norm ae' in
    let dnm = if nm >= n1 then nm -. n1 else nm -. n1 +. pi2 in
    let dne = if ne >= n1 then ne -. n1 else ne -. n1 +. pi2 in
    let sweep =
      if dnm < dne then dne          (* CCW *)
      else dne -. pi2                (* CW (negative) *) in
    let pt k =
      let t = float_of_int k /. float_of_int segments in
      let a = a1' +. sweep *. t in
      (cx +. r *. cos a, cy +. r *. sin a) in
    let rec emit k acc =
      if k >= segments then List.rev acc
      else
        let p1 = pt k and p2 = pt (k + 1) in
        emit (k + 1) (PLine (p1, p2) :: acc) in
    emit 0 []

(* Approximate a full circle by an octagon — used for inversion
   bubbles.  Removes any arc rendering from the schematic entirely. *)
let circle_polyline (cx, cy) r : prim list =
  let segments = 12 in
  let pi2 = 2.0 *. 3.14159265358979 in
  let pt k =
    let a = pi2 *. float_of_int k /. float_of_int segments in
    (cx +. r *. cos a, cy +. r *. sin a) in
  let rec emit k acc =
    if k >= segments then List.rev acc
    else emit (k + 1) (PLine (pt k, pt (k + 1)) :: acc) in
  emit 0 []

(* ---- Shape generators.  Each returns (prims, bbox, in_xy_list,
       out_xy, bubble_at_output?, name).                                *)

let shape_and ~n_in =
  let h = max 40.0 (float_of_int n_in *. 18.0) in
  let body_w = h *. 0.6 in       (* rectangle portion *)
  let bulge  = h /. 2.0 in       (* semicircle radius *)
  let semicircle =
    arc_polyline (body_w, h) (body_w +. bulge, h /. 2.0) (body_w, 0.0) in
  let prims =
    PLine ((0.0, 0.0), (0.0, h))
    :: PLine ((0.0, h),  (body_w, h))
    :: PLine ((0.0, 0.0),(body_w, 0.0))
    :: semicircle in
  let ins = List.init n_in (fun i -> (0.0, pin_y h n_in i)) in
  let out = (body_w +. bulge, h /. 2.0) in
  prims, (0.0, 0.0, body_w +. bulge, h), ins, out

let shape_or ~n_in =
  let h = max 40.0 (float_of_int n_in *. 18.0) in
  let body_w = h *. 1.25 in
  let back_bow = h *. 0.25 in
  let back = arc_polyline (0.0, h) (back_bow, h /. 2.0) (0.0, 0.0) in
  let top  = arc_polyline (0.0, h) (body_w *. 0.6, h *. 0.95) (body_w, h /. 2.0) in
  let bot  = arc_polyline (0.0, 0.0) (body_w *. 0.6, h *. 0.05) (body_w, h /. 2.0) in
  let prims = back @ top @ bot in
  let ins = List.init n_in (fun i ->
    let y = pin_y h n_in i in
    (back_bow *. 0.4, y)) in
  let out = (body_w, h /. 2.0) in
  prims, (0.0, 0.0, body_w, h), ins, out

let shape_xor ~n_in =
  let prims, (x1, y1, x2, y2), ins, out = shape_or ~n_in in
  let h = y2 -. y1 in
  let back_bow = h *. 0.25 in
  let xor_back = arc_polyline
    (-. back_bow *. 0.6, h)
    (back_bow *. 0.4, h /. 2.0)
    (-. back_bow *. 0.6, 0.0) in
  xor_back @ prims, (x1 -. back_bow *. 0.6, y1, x2, y2), ins, out

(* INV / BUF: pointing-right triangle. *)
let shape_triangle ~bubble =
  let body_w = 25.0 and h = 30.0 in
  let prims = [
    PLine ((0.0, 0.0), (0.0, h));
    PLine ((0.0, h),  (body_w, h /. 2.0));
    PLine ((0.0, 0.0),(body_w, h /. 2.0));
  ] in
  let prims, out_x =
    if bubble then
      let bx = body_w +. 2.0 in
      prims @ circle_polyline (bx, h /. 2.0) 2.0, bx +. 2.0
    else
      prims, body_w in
  let ins = [ (0.0, h /. 2.0) ] in
  let out = (out_x, h /. 2.0) in
  prims, (0.0, 0.0, out_x, h), ins, out

(* MUX: trapezoid, wider on the left. *)
let shape_mux ~n_in =
  let h = max 50.0 (float_of_int n_in *. 16.0) in
  let body_w = 32.0 in
  let trim = h *. 0.15 in
  let prims = [
    PLine ((0.0, 0.0), (0.0, h));
    PLine ((0.0, h),   (body_w, h -. trim));
    PLine ((body_w, h -. trim), (body_w, trim));
    PLine ((0.0, 0.0), (body_w, trim));
  ] in
  let ins = List.init n_in (fun i -> (0.0, pin_y h n_in i)) in
  let out = (body_w, h /. 2.0) in
  prims, (0.0, 0.0, body_w, h), ins, out

(* Flip-flop: rectangle with a small clock-input triangle drawn inside
   the body on the pin labelled "CK", "CLK", "CP" or "C". *)
let shape_rect ~name:_ ~n_in ~n_out ~clock_pin =
  let h = max 50.0 (float_of_int (max n_in n_out) *. 16.0 +. 12.0) in
  let body_w = 70.0 in
  let prims = ref [
    PLine ((0.0, 0.0), (0.0, h));
    PLine ((body_w, 0.0), (body_w, h));
    PLine ((0.0, 0.0), (body_w, 0.0));
    PLine ((0.0, h),  (body_w, h));
  ] in
  let ins  = List.init n_in  (fun i -> (0.0,    pin_y h n_in i)) in
  let outs = List.init n_out (fun i -> (body_w, pin_y h n_out i)) in
  (match clock_pin with
   | Some i when i >= 0 && i < n_in ->
       let (_, y) = List.nth ins i in
       prims := !prims @ [
         PLine ((0.0, y +. 4.0), (6.0, y));
         PLine ((0.0, y -. 4.0), (6.0, y));
       ]
   | _ -> ());
  !prims, (0.0, 0.0, body_w, h), ins, outs

(* ---- main entry point ---- *)

let auto_generate ~(cell_name : string)
                  ~(pins : (string * string) list) : symbol =
  let classify (_, d) =
    match String.lowercase_ascii d with
    | "input"  | "in"  -> PinIn
    | "output" | "out" -> PinOut
    | _                -> PinInOut in
  let ins_l  = List.filter (fun p -> classify p = PinIn)  pins in
  let outs_l = List.filter (fun p -> classify p = PinOut) pins in
  let other  = List.filter (fun p ->
    let c = classify p in c <> PinIn && c <> PinOut) pins in
  let outs_l = outs_l @ other in
  let n_in  = max 1 (List.length ins_l) in
  let n_out = max 1 (List.length outs_l) in
  let kind = classify_cell cell_name in
  let bubble_at p =
    (* Add an inversion bubble at point (x,y); returns (new_prims, new_x).*)
    let (x, y) = p in
    circle_polyline (x +. 2.0, y) 2.0, x +. 4.0
  in
  let mk_in i (name, _) (xy : float * float) = stub_in ~i name xy in
  let mk_out (name, _) (xy : float * float) = stub_out name xy in
  let mk
      (prims, bbox, in_xys, out_xy)
      ?(extra_prims = []) ?(out_offset = 0.0) () : symbol =
    let (x1, y1, x2, y2) = bbox in
    let bbox' = (x1, y1, x2 +. out_offset, y2) in
    let out_xy' = (fst out_xy +. out_offset, snd out_xy) in
    (* Pair input pin names with their xy positions; if names < spots,
       leave extras unnamed. *)
    let take n lst =
      let rec go n lst acc = match n, lst with
        | 0, _ | _, [] -> List.rev acc
        | n, x :: xs   -> go (n - 1) xs (x :: acc) in
      go n lst [] in
    let in_xys = take (min (List.length in_xys) (List.length ins_l)) in_xys in
    let names_in = List.mapi (fun i p ->
      try mk_in i p (List.nth in_xys i)
      with _ -> stub_in ~i (fst p) (x1, pin_y (y2 -. y1) (List.length ins_l) i)
    ) ins_l in
    let out_pins = match outs_l with
      | [p] -> [ mk_out p out_xy' ]
      | ps  -> List.mapi (fun i p ->
                 if i = 0 then mk_out p out_xy'
                 else stub_out (fst p) (x2,
                        pin_y (y2 -. y1) (List.length ps) i)) ps in
    { sym_name  = cell_name;
      sym_bbox  = bbox';
      sym_prims = prims @ extra_prims;
      sym_pins  = names_in @ out_pins }
  in
  match kind with
  | GK_And ->
      let p, b, ins, out = shape_and ~n_in in mk (p, b, ins, out) ()
  | GK_Nand ->
      let p, b, ins, out = shape_and ~n_in in
      let extra, new_x = bubble_at out in
      mk (p, b, ins, (new_x, snd out)) ~extra_prims:extra
        ~out_offset:(new_x -. fst out) ()
  | GK_Or ->
      let p, b, ins, out = shape_or  ~n_in in mk (p, b, ins, out) ()
  | GK_Nor ->
      let p, b, ins, out = shape_or  ~n_in in
      let extra, new_x = bubble_at out in
      mk (p, b, ins, (new_x, snd out)) ~extra_prims:extra
        ~out_offset:(new_x -. fst out) ()
  | GK_Xor ->
      let p, b, ins, out = shape_xor ~n_in in mk (p, b, ins, out) ()
  | GK_Xnor ->
      let p, b, ins, out = shape_xor ~n_in in
      let extra, new_x = bubble_at out in
      mk (p, b, ins, (new_x, snd out)) ~extra_prims:extra
        ~out_offset:(new_x -. fst out) ()
  | GK_Inv ->
      let p, b, ins, out = shape_triangle ~bubble:true  in mk (p, b, ins, out) ()
  | GK_Buf ->
      let p, b, ins, out = shape_triangle ~bubble:false in mk (p, b, ins, out) ()
  | GK_Mux ->
      let p, b, ins, out = shape_mux ~n_in in mk (p, b, ins, out) ()
  | GK_FF | GK_Generic ->
      (* Locate a clock pin by name. *)
      let clock_pin =
        let find_idx names =
          let lst = List.mapi (fun i (n, _) -> (i, String.lowercase_ascii n)) names in
          try Some (fst (List.find (fun (_, n) ->
            n = "clk" || n = "ck" || n = "cp" || n = "c"
            || n = "clock") lst))
          with Not_found -> None in
        if kind = GK_FF then find_idx ins_l else None in
      let p, b, ins, out =
        shape_rect ~name:cell_name ~n_in:(List.length ins_l)
                   ~n_out:(List.length outs_l) ~clock_pin in
      let (x1, y1, x2, y2) = b in
      (* Use the full multi-output rect path. *)
      let mk_multi () =
        let names_in = List.mapi (fun i p ->
          try mk_in i p (List.nth ins i)
          with _ -> stub_in ~i (fst p)
                     (x1, pin_y (y2 -. y1) (List.length ins_l) i)
        ) ins_l in
        let names_out = List.mapi (fun i p ->
          try mk_out p (List.nth out i)
          with _ -> stub_out (fst p)
                     (x2, pin_y (y2 -. y1) (List.length outs_l) i)
        ) outs_l in
        { sym_name = cell_name; sym_bbox = b;
          sym_prims = p; sym_pins = names_in @ names_out } in
      mk_multi ()

(* Build a library by auto-generating from a Liberty cellhash.
   `cellhash` matches the structure produced by Liberty_rewrite. *)
let auto_from_liberty_cellhash
    (cellhash : (string, ((string * string) list
                          * (string * string) list
                          * 'a)) Hashtbl.t) : library =
  let lib = empty () in
  Hashtbl.iter (fun cell (pins, _fns, _purpose) ->
    let sym = auto_generate ~cell_name:cell ~pins in
    Hashtbl.replace lib cell sym
  ) cellhash;
  lib

(* Fallback when nothing is known about a cell: synthesise a 2-input/
   1-output stub from a list of port names + a guessed direction.  The
   port direction comes from the BIR library_port record in the caller.
*)
let stub ~cell_name ~ports : symbol =
  auto_generate ~cell_name ~pins:ports
