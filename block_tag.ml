(* Cell-name encoding so an arbitrary post-synth pass (timing report
   prettifier, critical-path block-swap, fanout-cone extractor) can
   recover (a) which RTL signal/bit a gate contributes to and (b)
   which multi-cell BLOCK (e.g. one ripple-carry adder) it's part of.

   Format
   ------
   Cell inst names follow a strict, parseable shape:

       _T__<modhash>__<kind>__<blockid>__<sig>__<bit>__<role>__<seq>_

   - Leading "_T__" is a magic prefix telling readers "this is one of
     ours, decode the fields" (T = tag).  Anything that doesn't start
     with `_T__` is foreign — typically a buffer OpenROAD inserted
     during repair_timing or CTS — and stays anonymous from the tag
     decoder's perspective.

   - <modhash>   : 6-char short hash of the parent module name
                   (collision is fine — the per-module netlist scope
                   keeps tags unique; the hash just helps grep).
   - <kind>      : OP   for atomic per-bit blasts (one cell per bit)
                   ADD  / SUB / EQ  / LT  / MUL  for multi-cell blocks
                   REG  for sequential FF-blasts
                   WRAP for memory-wrapper standard cells
                   AUX  for everything else (tie cells, OR-tree, …)
   - <blockid>   : "Bnnn" — only meaningful for multi-cell blocks
                   (kind ∈ {ADD,SUB,EQ,LT,MUL,REG}).  For OP / AUX
                   this slot is "B___".
   - <sig>       : sanitized RTL output signal name this cell
                   contributes to.  Truncated to ≤24 chars.
   - <bit>       : "bNN" — bit position within <sig>; "b__" if scalar.
   - <role>      : sub-role within the block — e.g. ab / abc / co /
                   sum / aab / nb / lt / eq / xor / pp ...
                   For OP cells, <role> = the cell-type prefix
                   (AND2 / OR2 / XOR2 / INV / MUX2 / DFF ...).
   - <seq>       : monotonic global sequence — uniqueness guarantee.

   None of these fields contains `__` so they're easily separated by a
   double-underscore split.  Field values are sanitised (only
   [A-Za-z0-9_]) so the resulting name is a valid Verilog identifier.

   Design rationale
   ----------------
   - Reversible: any reader can run [decode] on a cell name and get
     back the structured info — no sidecar lookup needed for the
     basic case (back-substituting a timing report).  The optional
     sidecar JSON (see [Block_tag.write_blocks_json]) carries the
     *full* RTL signal name (in case it was truncated) and the
     module-name expansion of <modhash>.
   - Survives layout transforms: OpenROAD's repair_design /
     repair_timing / CTS rename newly-inserted buffer cells but does
     NOT rename the cells they're inserted between.  Our tagged
     cells therefore keep their tags through the whole flow — only
     the buffers between them are anonymous.
   - Block grouping by tag walk: to recover all cells of block B17,
     just regex-match `_T__.*__Bnnn017__` across the netlist.  No
     structural pattern matcher needed for the common case.  *)

(* ─── Schema ─────────────────────────────────────────────────────── *)

type kind =
  | OP            (* one cell per bit — atomic blast *)
  | ADD           (* ripple-carry adder block *)
  | SUB           (* ripple-borrow subtractor block *)
  | EQ            (* equality reduce-tree *)
  | LT            (* less-than (sub + invert) *)
  | MUL           (* array multiplier *)
  | REG           (* DFF blast *)
  | WRAP          (* memory-wrapper standard cells *)
  | AUX           (* tie cells, OR-trees, miscellaneous *)

let kind_to_string = function
  | OP -> "OP"   | ADD -> "ADD" | SUB -> "SUB" | EQ -> "EQ"  | LT -> "LT"
  | MUL -> "MUL" | REG -> "REG" | WRAP -> "WRAP" | AUX -> "AUX"

let kind_of_string = function
  | "OP" -> Some OP   | "ADD" -> Some ADD | "SUB" -> Some SUB
  | "EQ" -> Some EQ   | "LT"  -> Some LT  | "MUL" -> Some MUL
  | "REG" -> Some REG | "WRAP" -> Some WRAP | "AUX" -> Some AUX
  | _ -> None

type tag = {
  modhash  : string;       (* 6-char hash of parent module *)
  kind     : kind;
  block_id : int option;   (* monotonic per multi-cell block; None for OP/AUX *)
  signal   : string;       (* RTL output signal (truncated) *)
  bit      : int option;
  role     : string;       (* sub-role within block, or celltype prefix *)
  seq      : int;
}

(* ─── Sanitisation + encoding ────────────────────────────────────── *)

let sanitize_id ?(maxlen=24) s =
  let n0 = String.length s in
  let n = if n0 > maxlen then maxlen else n0 in
  let b = Bytes.create n in
  for i = 0 to n - 1 do
    let c = s.[i] in
    Bytes.set b i
      (match c with
       | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> c
       | _ -> '_')
  done;
  Bytes.to_string b

(* Tiny stable hash of the module name — 6 hex chars.  Collision risk
   is fine because tags only need to be unique within a module's
   netlist, which the seq counter already guarantees.  The hash is
   for greppability ("show me everything from this module"). *)
let modhash_of name =
  let h = ref 0xcbf29ce484222325 in
  String.iter (fun c ->
    h := (!h lxor (Char.code c)) * 0x100000001b3 land 0xffffffffff) name;
  Printf.sprintf "%06x" (!h land 0xffffff)

let int_field n width = function
  | None -> String.make width '_'
  | Some v -> Printf.sprintf "%0*d" width v

let encode t =
  Printf.sprintf "_T__%s__%s__B%s__%s__b%s__%s__%d_"
    t.modhash
    (kind_to_string t.kind)
    (int_field 6 3 t.block_id)
    (sanitize_id t.signal)
    (int_field 3 2 t.bit)
    (sanitize_id ~maxlen:8 t.role)
    t.seq

(* ─── Decoding ───────────────────────────────────────────────────── *)

let decode (s : string) : tag option =
  let n = String.length s in
  if n < 4 || not (String.length s >= 4 && String.sub s 0 4 = "_T__")
  then None
  else
    let inner =
      let s' = String.sub s 4 (n - 4) in
      (* drop trailing underscore if present *)
      if String.length s' > 0 && s'.[String.length s' - 1] = '_'
      then String.sub s' 0 (String.length s' - 1) else s' in
    (* split on "__" *)
    let parts =
      let rec split acc i =
        match String.index_from_opt inner i '_' with
        | None -> List.rev (String.sub inner i (String.length inner - i) :: acc)
        | Some j when j + 1 < String.length inner && inner.[j+1] = '_' ->
            let part = String.sub inner i (j - i) in
            split (part :: acc) (j + 2)
        | Some _ -> split acc (i + 1)
      in split [] 0 in
    match parts with
    | [modhash; kind_s; bid_s; sig_s; bit_s; role; seq_s] ->
        (try
           let kind = match kind_of_string kind_s with
             | Some k -> k | None -> raise Exit in
           let block_id =
             let b = String.sub bid_s 1 (String.length bid_s - 1) in
             if String.contains b '_' then None
             else (try Some (int_of_string b) with _ -> None) in
           let bit =
             let b = String.sub bit_s 1 (String.length bit_s - 1) in
             if String.contains b '_' then None
             else (try Some (int_of_string b) with _ -> None) in
           Some { modhash; kind; block_id; signal = sig_s;
                  bit; role; seq = int_of_string seq_s }
         with _ -> None)
    | _ -> None

(* ─── Pretty-printing for timing reports ─────────────────────────── *)

(* Render a tag as a human-friendly cell name for back-substitution
   into placement / timing reports.  Example:
     full encoded:  _T__a3f1c2__ADD__B007__alu_out__b15__abc__1234_
     pretty:        cpu/alu_out[15]/add.abc                          *)
let pretty ?(module_name="") (t : tag) : string =
  let mod_part =
    if module_name = "" then ""
    else module_name ^ "/" in
  let sig_part = match t.bit with
    | Some b -> Printf.sprintf "%s[%d]" t.signal b
    | None -> t.signal in
  let block_part = match t.kind, t.block_id with
    | (ADD|SUB|EQ|LT|MUL|REG), Some _ ->
        Printf.sprintf "%s.%s"
          (String.lowercase_ascii (kind_to_string t.kind)) t.role
    | _ -> t.role in
  Printf.sprintf "%s%s/%s" mod_part sig_part block_part

(* Walk a string (e.g. a line of an STA report) and replace every
   occurrence of an encoded tag with its pretty form.  Caller passes
   a [resolve_module] mapping modhash → module name for the optional
   prefix. *)
let prettify_line ~resolve_module line =
  let buf = Buffer.create (String.length line) in
  let n = String.length line in
  let i = ref 0 in
  while !i < n do
    if !i + 4 <= n && String.sub line !i 4 = "_T__" then begin
      (* find end of identifier *)
      let j = ref (!i + 4) in
      while !j < n && (
        let c = line.[!j] in
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
        || (c >= '0' && c <= '9') || c = '_'
      ) do incr j done;
      let name = String.sub line !i (!j - !i) in
      (match decode name with
       | Some t ->
           let m = try resolve_module t.modhash with Not_found -> "" in
           Buffer.add_string buf (pretty ~module_name:m t)
       | None -> Buffer.add_string buf name);
      i := !j
    end else begin
      Buffer.add_char buf line.[!i]; incr i
    end
  done;
  Buffer.contents buf

(* ─── Per-process counters + builders ────────────────────────────── *)

(* Sequence counter (shared with [Lib_map.next_id] would be cleanest
   but for now keep separate; [Lib_map.mint_tag] increments both). *)
let next_seq = ref 0
let next_block = ref 0

(* Allocate a block id for a new multi-cell block (one add, one sub,
   one mul, …).  Stays unique across the whole synth run. *)
let alloc_block_id () = incr next_block; !next_block

let mint ?(block_id=None) ~kind ~modhash ~signal ?bit ~role () =
  incr next_seq;
  encode { modhash; kind; block_id; signal; bit; role; seq = !next_seq }

(* ─── Sidecar JSON ───────────────────────────────────────────────── *)

(* Per-block record collected during synth; emitted alongside the
   netlist so back-substitution tools can recover the *full* RTL
   signal name (in case it was truncated for the inst-name field) and
   the module-hash → module-name mapping. *)
type block_record = {
  br_id      : int;
  br_kind    : kind;
  br_module  : string;
  br_signal  : string;     (* untruncated *)
  br_width   : int;
  br_arch    : string;     (* "ripple", "cla", "wallace", … *)
}

let blocks : block_record list ref = ref []

let record b = blocks := b :: !blocks

let module_names : (string, string) Hashtbl.t = Hashtbl.create 8
let register_module name =
  let h = modhash_of name in
  Hashtbl.replace module_names h name;
  h

let resolve_module h =
  try Hashtbl.find module_names h with Not_found -> ""

let write_blocks_json path =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf "{\n  \"modules\": [\n";
  let first = ref true in
  Hashtbl.iter (fun h name ->
    if !first then first := false else Buffer.add_string buf ",\n";
    Buffer.add_string buf
      (Printf.sprintf "    { \"hash\": \"%s\", \"name\": \"%s\" }" h name)
  ) module_names;
  Buffer.add_string buf "\n  ],\n  \"blocks\": [\n";
  let first = ref true in
  List.iter (fun b ->
    if !first then first := false else Buffer.add_string buf ",\n";
    Buffer.add_string buf
      (Printf.sprintf
         "    { \"id\": %d, \"kind\": \"%s\", \"module\": \"%s\", \
                  \"signal\": \"%s\", \"width\": %d, \"arch\": \"%s\" }"
         b.br_id (kind_to_string b.br_kind) b.br_module
         b.br_signal b.br_width b.br_arch)
  ) (List.rev !blocks);
  Buffer.add_string buf "\n  ]\n}\n";
  let oc = open_out path in
  output_string oc (Buffer.contents buf);
  close_out oc

let reset () =
  next_seq := 0;
  next_block := 0;
  blocks := [];
  Hashtbl.clear module_names
