(* Slang AST JSON → Behavioral IR.
 *
 * Slang (https://github.com/MikePopoloski/slang) is an independent
 * SystemVerilog elaborator. Its `slang --ast-json` driver dumps the
 * fully-elaborated AST as JSON: parameters resolved, generates
 * unrolled, types canonicalised, expressions kept in source-shape.
 * This module parses that JSON into the same Behavioral_ir.bprogram
 * the Verible/Verilator/Yosys frontends emit.
 *
 * Why a third independent SV reader: the goal is a canonical
 * elaborated-but-unoptimised reference netlist for the miter, with
 * no Xilinx-specific tricks (`$aldff`, RTL_REG_anything) and no Yosys
 * transformations (`proc;flatten` introducing `$buf` collectors,
 * anonymous `$Ny` intermediate wires). If Verible-BIR and Slang-BIR
 * agree, we have proof we understood the source the same way. *)

open Behavioral_ir

(* ─── JSON helpers ───────────────────────────────────────────────── *)

type json = Yojson.Safe.t

let assoc k (j : json) =
  match j with
  | `Assoc xs -> List.assoc_opt k xs
  | _ -> None

let str_field k j =
  match assoc k j with
  | Some (`String s) -> Some s
  | _ -> None

let list_field k j =
  match assoc k j with
  | Some (`List xs) -> xs
  | _ -> []

let kind_of j = match str_field "kind" j with Some s -> s | None -> ""

let name_of j = match str_field "name" j with Some s -> s | None -> ""

(* ─── Symbol address ↔ name table ────────────────────────────────── *)

(* Slang's NamedValue references a symbol via a string of the form
 * "<addr> <name>". We don't actually need the addr — the name is
 * sufficient because Slang elaborates duplicate-named variables
 * out of scope. But the format also distinguishes
 * `<scope>.<name>`, which matters for hierarchical references. *)
let strip_addr s =
  match String.index_opt s ' ' with
  | Some i -> String.sub s (i + 1) (String.length s - i - 1)
  | None -> s

(* ─── Type → width ────────────────────────────────────────────────── *)

(* Slang's "type" strings look like "logic", "logic[3:0]", "int",
 * "logic[63:0][7:0]" (2-D packed), "shortint", … Returns
 * (total_bits, elem_bits, ndims). For 1-D packed `logic[3:0]`
 * elem_bits = total_bits (it's a single 4-bit BInt). For 2-D packed
 * `logic[3:0][1:0]` total = 8, elem = 2 (BArray of size 4). *)
let parse_type s =
  let s = String.trim s in
  if s = "logic" || s = "bit" || s = "reg" then (1, 1, 0)
  else if s = "int" || s = "integer" || s = "shortint"
       || s = "longint" then (32, 32, 0)
  else if s = "byte" then (8, 8, 0)
  else
    let n = String.length s in
    let dims = ref [] in
    let i = ref 0 in
    while !i < n do
      if s.[!i] = '[' then begin
        let j = ref (!i + 1) in
        while !j < n && s.[!j] <> ']' do incr j done;
        if !j < n then begin
          let body = String.sub s (!i + 1) (!j - !i - 1) in
          (match String.split_on_char ':' body with
           | [a; b] ->
               (try
                  let hi = int_of_string (String.trim a) in
                  let lo = int_of_string (String.trim b) in
                  dims := (abs (hi - lo) + 1) :: !dims
                with _ -> ())
           | _ -> ());
          i := !j + 1
        end else incr i
      end else incr i
    done;
    let ds = List.rev !dims in
    match ds with
    | [] -> (1, 1, 0)
    | [w] -> (w, w, 1)
    | outer :: inner :: _ -> (outer * inner, inner, List.length ds)

(* ─── Operators ──────────────────────────────────────────────────── *)

let bop_of_string = function
  | "Add" -> Some BAdd
  | "Subtract" -> Some BSub
  | "Multiply" -> Some BMul
  | "Divide" -> Some BDiv
  | "Mod" -> Some BMod
  | "BinaryAnd" | "LogicalAnd" -> Some BAnd
  | "BinaryOr"  | "LogicalOr"  -> Some BOr
  | "BinaryXor" -> Some BXor
  | "Equality" | "CaseEquality" -> Some BEq
  | "Inequality" | "CaseInequality" -> Some BNe
  | "LessThan" -> Some BLt
  | "LessThanEqual" -> Some BLe
  | "GreaterThan" -> Some BGt
  | "GreaterThanEqual" -> Some BGe
  | "LogicalShiftLeft" | "ArithmeticShiftLeft" -> Some BShl
  | "LogicalShiftRight" -> Some BShr
  | "ArithmeticShiftRight" -> Some BAshr
  | _ -> None

let uop_of_string = function
  | "BitwiseNot" | "LogicalNot" -> Some BNot
  | "Minus" -> Some BNeg
  | "BitwiseAnd" -> Some BRedAnd
  | "BitwiseOr"  -> Some BRedOr
  | "BitwiseXor" -> Some BRedXor
  | _ -> None

(* ─── Constant-literal parsing ───────────────────────────────────── *)

(* Slang's IntegerLiteral has fields `value` (decimal string) and
 * sometimes a typed shape like `4'd0`, `4'b1010`. We accept either. *)
let parse_const value_str type_str =
  let s = String.trim value_str in
  let total_w =
    let w, _, _ = parse_type type_str in w
  in
  if String.contains s '\'' then begin
    (* SV-style literal: <width>'<base><digits>. *)
    match String.split_on_char '\'' s with
    | [w_s; rest] when String.length rest >= 1 ->
        let w =
          try int_of_string w_s with _ -> total_w
        in
        let v = match rest.[0] with
          | 'b' | 'B' ->
              (try int_of_string ("0b" ^ String.sub rest 1
                                   (String.length rest - 1))
               with _ -> 0)
          | 'h' | 'H' ->
              (try int_of_string ("0x" ^ String.sub rest 1
                                   (String.length rest - 1))
               with _ -> 0)
          | 'd' | 'D' ->
              (try int_of_string (String.sub rest 1
                                    (String.length rest - 1))
               with _ -> 0)
          | _ -> (try int_of_string rest with _ -> 0)
        in
        BConst { value = v; width = w }
    | _ -> BConst { value = (try int_of_string s with _ -> 0);
                    width = total_w }
  end else
    BConst { value = (try int_of_string s with _ -> 0);
             width = total_w }

(* ─── Expressions ────────────────────────────────────────────────── *)

let rec expr_to_bexpr j =
  match kind_of j with
  | "NamedValue" ->
      let sym = match str_field "symbol" j with
        | Some s -> strip_addr s
        | None -> "?" in
      BVar sym
  | "IntegerLiteral" ->
      let v = match str_field "value" j with Some s -> s | None -> "0" in
      let t = match str_field "type" j with Some s -> s | None -> "int" in
      parse_const v t
  | "UnbasedUnsizedIntegerLiteral" ->
      (* `'0`, `'1`, `'x`, `'z` — the value is the bit pattern,
       * the LHS context determines the width. We size to the
       * declared type. *)
      let v = match str_field "value" j with Some s -> s | None -> "0" in
      let t = match str_field "type" j with Some s -> s | None -> "logic" in
      parse_const v t
  | "BinaryOp" ->
      let op_s = match str_field "op" j with Some s -> s | None -> "" in
      let lhs = match assoc "left" j with
        | Some j' -> expr_to_bexpr j'
        | None -> BConst { value = 0; width = 1 } in
      let rhs = match assoc "right" j with
        | Some j' -> expr_to_bexpr j'
        | None -> BConst { value = 0; width = 1 } in
      let t = match str_field "type" j with Some s -> s | None -> "logic" in
      let w, _, _ = parse_type t in
      (match bop_of_string op_s with
       | Some op -> BBinOp { op; lhs; rhs;
                             result_type = BInt { width = w; signed = Unsigned } }
       | None -> lhs)
  | "UnaryOp" ->
      let op_s = match str_field "op" j with Some s -> s | None -> "" in
      let operand = match assoc "operand" j with
        | Some j' -> expr_to_bexpr j'
        | None -> BConst { value = 0; width = 1 } in
      let t = match str_field "type" j with Some s -> s | None -> "logic" in
      let w, _, _ = parse_type t in
      (match uop_of_string op_s with
       | Some op -> BUnOp { op; operand;
                            result_type = BInt { width = w; signed = Unsigned } }
       | None -> operand)
  | "ConditionalOp" ->
      (* Slang uses `conditions: [{expr: …}]` for the predicate (a
       * list to support `c1 &&& c2 ? a : b` SV-2017 patterns), and
       * `left`/`right` for then/else. *)
      let cond = match list_field "conditions" j with
        | first :: _ ->
            (match assoc "expr" first with
             | Some j' -> expr_to_bexpr j'
             | None -> BConst { value = 0; width = 1 })
        | [] -> BConst { value = 0; width = 1 }
      in
      let then_val = match assoc "left" j with
        | Some j' -> expr_to_bexpr j'
        | None -> BConst { value = 0; width = 1 } in
      let else_val = match assoc "right" j with
        | Some j' -> expr_to_bexpr j'
        | None -> BConst { value = 0; width = 1 } in
      BCond { condition = cond; then_val; else_val }
  | "Concatenation" ->
      (* `operands` is a list, MSB-first per SV convention — same
       * order BConcat expects. *)
      let ops = list_field "operands" j in
      BConcat (List.map expr_to_bexpr ops)
  | "Replication" ->
      let count = match assoc "count" j with
        | Some j' ->
            (match expr_to_bexpr j' with
             | BConst { value; _ } -> value
             | _ -> 1)
        | None -> 1
      in
      let value = match assoc "concat" j with
        | Some j' -> expr_to_bexpr j'
        | None -> BConst { value = 0; width = 1 }
      in
      BReplicate { count; value }
  | "ElementSelect" ->
      let arr = match assoc "value" j with
        | Some j' -> expr_to_bexpr j'
        | None -> BConst { value = 0; width = 1 } in
      let idx = match assoc "selector" j with
        | Some j' -> expr_to_bexpr j'
        | None -> BConst { value = 0; width = 1 } in
      BSelect { array = arr; index = idx }
  | "RangeSelect" ->
      let arr = match assoc "value" j with
        | Some j' -> expr_to_bexpr j'
        | None -> BConst { value = 0; width = 1 } in
      let msb = match assoc "left" j with
        | Some j' ->
            (match expr_to_bexpr j' with
             | BConst { value; _ } -> value | _ -> 0)
        | None -> 0 in
      let lsb = match assoc "right" j with
        | Some j' ->
            (match expr_to_bexpr j' with
             | BConst { value; _ } -> value | _ -> 0)
        | None -> 0 in
      BSlice { signal = arr; msb; lsb }
  | "Conversion" ->
      (* Type cast — value-preserving for our integer arithmetic. *)
      (match assoc "operand" j with
       | Some j' -> expr_to_bexpr j'
       | None -> BConst { value = 0; width = 1 })
  | "Assignment" ->
      (* Embedded assignment as an expression — return the RHS. The
       * outer ExpressionStatement is what creates the BAssign. *)
      (match assoc "right" j with
       | Some j' -> expr_to_bexpr j'
       | None -> BConst { value = 0; width = 1 })
  | _ ->
      (* Unrecognised — emit a placeholder zero. Matching strict mode
       * here would catch shapes the converter doesn't model. *)
      BConst { value = 0; width = 1 }

(* ─── Statements ─────────────────────────────────────────────────── *)

let rec stmt_to_bstmt j =
  match kind_of j with
  | "Block" ->
      (* `body` is either a single statement or a List node. *)
      (match assoc "body" j with
       | Some inner -> stmt_to_bstmt inner
       | None -> BBlock [])
  | "List" ->
      (* Slang sometimes wraps a sequence of statements as a `List`
       * with a `list` field. *)
      let items = list_field "list" j in
      BBlock (List.map stmt_to_bstmt items)
  | "Conditional" ->
      let cond = match list_field "conditions" j with
        | first :: _ ->
            (match assoc "expr" first with
             | Some j' -> expr_to_bexpr j'
             | None -> BConst { value = 0; width = 1 })
        | [] -> BConst { value = 0; width = 1 }
      in
      let then_stmts = match assoc "ifTrue" j with
        | Some j' -> [stmt_to_bstmt j']
        | None -> [] in
      let else_stmts = match assoc "ifFalse" j with
        | Some j' -> [stmt_to_bstmt j']
        | None -> [] in
      BIf { condition = cond; then_stmts; else_stmts }
  | "ExpressionStatement" ->
      (* The wrapped expression is usually an Assignment. *)
      (match assoc "expr" j with
       | Some inner ->
           (match kind_of inner with
            | "Assignment" ->
                let lhs = match assoc "left" inner with
                  | Some l ->
                      (match str_field "symbol" l with
                       | Some s -> strip_addr s
                       | None ->
                           (* Sliced/indexed LHS — pull the base name. *)
                           let rec base = function
                             | "NamedValue" -> str_field "symbol" l
                             | _ ->
                                 (match assoc "value" l with
                                  | Some inner -> str_field "symbol" inner
                                  | None -> None)
                           in
                           (match base (kind_of l) with
                            | Some s -> strip_addr s
                            | None -> "?"))
                  | None -> "?"
                in
                let rhs = match assoc "right" inner with
                  | Some r -> expr_to_bexpr r
                  | None -> BConst { value = 0; width = 1 } in
                BAssign { lhs; rhs }
            | _ -> BBlock [])
       | None -> BBlock [])
  | "Case" ->
      let sel = match assoc "expr" j with
        | Some j' -> expr_to_bexpr j'
        | None -> BConst { value = 0; width = 1 } in
      let items = list_field "items" j in
      let cases, default =
        List.fold_left (fun (cs, def) ci ->
          match assoc "expressions" ci with
          | Some (`List exprs) ->
              let body = match assoc "stmt" ci with
                | Some j' -> [stmt_to_bstmt j'] | None -> [] in
              let arm_keys = List.map expr_to_bexpr exprs in
              (cs @ List.map (fun k -> (k, body)) arm_keys, def)
          | _ ->
              let body = match assoc "stmt" ci with
                | Some j' -> [stmt_to_bstmt j'] | None -> [] in
              (cs, body)
        ) ([], []) items
      in
      BCase { selector = sel; cases; default }
  | _ -> BBlock []

(* ─── Module body extraction ─────────────────────────────────────── *)

(* Slang emits two members per signal — a `Port` (carries direction)
 * and a `Variable` (carries storage type). Match by name and merge
 * into a single bsignal. *)
let extract_signals members =
  let ports : (string, [`Input|`Output|`Internal]) Hashtbl.t =
    Hashtbl.create 16 in
  List.iter (fun m ->
    match kind_of m with
    | "Port" ->
        let name = name_of m in
        let dir = match str_field "direction" m with
          | Some "In" -> `Input
          | Some "Out" -> `Output
          | Some "InOut" -> `Internal
          | _ -> `Internal
        in
        Hashtbl.replace ports name dir
    | _ -> ()
  ) members;
  List.filter_map (fun m ->
    match kind_of m with
    | "Variable" | "Net" ->
        let name = name_of m in
        if name = "" then None
        else
          let t = match str_field "type" m with Some s -> s | None -> "logic" in
          let w, elem_w, ndims = parse_type t in
          let stype =
            if ndims >= 2 && w > 0 && elem_w > 0 then
              BArray {
                element = BInt { width = elem_w; signed = Unsigned };
                size = w / elem_w;
              }
            else
              BInt { width = w; signed = Unsigned }
          in
          let dir = try Hashtbl.find ports name with Not_found -> `Internal in
          Some {
            name;
            stype;
            direction = dir;
            initial_value = None;
          }
    | _ -> None
  ) members

(* Pull the clock + edge from the FIRST SignalEvent in an EventList.
 * Slang emits the events in source order; the first edge identifier
 * is the clock for `always_ff @(posedge clk or negedge rst_n)` —
 * synthesis treats `negedge rst_n` as the async-reset trigger,
 * not as a second clock. *)
let extract_clock_event timing =
  match assoc "events" timing with
  | Some (`List (e :: _)) ->
      let clk = match assoc "expr" e with
        | Some j' ->
            (match str_field "symbol" j' with
             | Some s -> strip_addr s | None -> "clk")
        | None -> "clk"
      in
      let edge = match str_field "edge" e with
        | Some "PosEdge" -> `Pos
        | Some "NegEdge" -> `Neg
        | _ -> `Pos
      in
      (clk, edge)
  | _ -> ("clk", `Pos)

let extract_processes members =
  List.filter_map (fun m ->
    match kind_of m with
    | "ContinuousAssign" ->
        let assignment = match assoc "assignment" m with
          | Some j' -> j'
          | None -> `Null in
        let lhs = match assoc "left" assignment with
          | Some l ->
              (match str_field "symbol" l with
               | Some s -> strip_addr s
               | None -> "?")
          | None -> "?" in
        let rhs = match assoc "right" assignment with
          | Some r -> expr_to_bexpr r
          | None -> BConst { value = 0; width = 1 } in
        Some (BCombinational {
          name = "assign_" ^ lhs;
          sensitivity = [BAny];
          body = [BAssign { lhs; rhs }];
        })
    | "ProceduralBlock" ->
        let kind = match str_field "procedureKind" m with
          | Some s -> s | None -> "Always" in
        let inner = match assoc "body" m with Some j' -> j' | None -> `Null in
        (match kind with
         | "AlwaysFF" | "Always_FF" ->
             let timing, body_stmt =
               match kind_of inner with
               | "Timed" ->
                   let t = match assoc "timing" inner with
                     | Some j' -> j' | None -> `Null in
                   let s = match assoc "stmt" inner with
                     | Some j' -> j' | None -> `Null in
                   (t, s)
               | _ -> (`Null, inner)
             in
             let (clk, edge) = extract_clock_event timing in
             let body = [stmt_to_bstmt body_stmt] in
             Some (BSequential {
               name = "always_ff";
               clock = clk;
               clock_edge = edge;
               reset = None;
               reset_edge = None;
               reset_async = false;
               body;
             })
         | "AlwaysComb" | "Always_Comb" | "Always" | "AlwaysLatch" ->
             let body = [stmt_to_bstmt inner] in
             Some (BCombinational {
               name = "always_comb";
               sensitivity = [BAny];
               body;
             })
         | _ -> None)
    | _ -> None
  ) members

(* ─── Top-level ──────────────────────────────────────────────────── *)

(* Walk every Instance / InstanceBody under `design.members` and
 * produce one bmodule each. *)
let rec collect_instances acc j =
  match kind_of j with
  | "InstanceBody" ->
      let name = name_of j in
      let members = list_field "members" j in
      let signals = extract_signals members in
      let processes = extract_processes members in
      let m = {
        name;
        params = [];
        signals;
        processes;
        instances = [];
        funcs = [];
        mems = [];
      } in
      m :: acc
  | _ ->
      let acc =
        match assoc "body" j with
        | Some j' -> collect_instances acc j'
        | None -> acc
      in
      List.fold_left collect_instances acc (list_field "members" j)

let convert_json (j : json) : bprogram =
  let design = match assoc "design" j with Some d -> d | None -> j in
  let mods = List.rev (collect_instances [] design) in
  { modules = mods; library_cells = [] }

(* ─── Driver invocation ──────────────────────────────────────────── *)

let find_slang () =
  let home = try Sys.getenv "HOME" with Not_found -> "/root" in
  let candidates = [
    home ^ "/sv-tests/third_party/tools/slang/build/bin/slang";
    "/usr/local/bin/slang";
  ] in
  List.find_opt Sys.file_exists candidates

let convert_files ~top files : bprogram option =
  match find_slang () with
  | None ->
      Printf.eprintf "[slang] driver not found\n";
      None
  | Some slang ->
      let json_path = Filename.temp_file "slang_" ".json" in
      let cmd = Printf.sprintf
        "%s --ast-json %s --top %s %s 2>/dev/null"
        (Filename.quote slang)
        (Filename.quote json_path)
        (Filename.quote top)
        (String.concat " " (List.map Filename.quote files))
      in
      let rc = Sys.command cmd in
      if rc <> 0 then begin
        Printf.eprintf "[slang] driver returned rc=%d\n" rc;
        (try Sys.remove json_path with _ -> ());
        None
      end else begin
        let j = try Yojson.Safe.from_file json_path
                with e ->
                  Printf.eprintf "[slang] json parse failed: %s\n"
                    (Printexc.to_string e);
                  `Null
        in
        (try Sys.remove json_path with _ -> ());
        Some (convert_json j)
      end
