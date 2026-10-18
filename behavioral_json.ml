(* Explicit JSON printer for a bmodule (the IR's [@@deriving yojson] is inert:
   its library stanza carries no ppx).  Shape: variants as ["Ctor", payload],
   records as objects, Z.t as decimal strings.  Consumed by
   test/qor/bir_share_scan.py. *)
open Behavioral_ir

let rec ty = function
  | BInt { width; signed } -> `List [ `String "BInt"; `Assoc [ ("width", `Int width); ("signed", `String (match signed with Signed -> "Signed" | Unsigned -> "Unsigned")) ] ]
  | BBool -> `String "BBool"
  | BArray { element; size } -> `List [ `String "BArray"; `Assoc [ ("element", ty element); ("size", `Int size) ] ]
  | BStruct { fields } -> `List [ `String "BStruct"; `Assoc [ ("fields", `List (List.map (fun (n, t) -> `List [ `String n; ty t ]) fields)) ] ]

let binop o = `String (match o with
  | BAdd -> "BAdd" | BSub -> "BSub" | BMul -> "BMul" | BDiv -> "BDiv" | BMod -> "BMod"
  | BAnd -> "BAnd" | BOr -> "BOr" | BXor -> "BXor" | BShl -> "BShl" | BShr -> "BShr" | BAshr -> "BAshr"
  | BEq -> "BEq" | BNe -> "BNe" | BLt -> "BLt" | BLe -> "BLe" | BGt -> "BGt" | BGe -> "BGe")
let unop o = `String (match o with
  | BNot -> "BNot" | BNeg -> "BNeg" | BRedAnd -> "BRedAnd" | BRedOr -> "BRedOr" | BRedXor -> "BRedXor")

let rec expr = function
  | BVar n -> `List [ `String "BVar"; `String n ]
  | BConst { value; width } -> `List [ `String "BConst"; `Assoc [ ("value", `String (Z.to_string value)); ("width", `Int width) ] ]
  | BBinOp { op; lhs; rhs; result_type } ->
      `List [ `String "BBinOp"; `Assoc [ ("op", binop op); ("lhs", expr lhs); ("rhs", expr rhs); ("result_type", ty result_type) ] ]
  | BUnOp { op; operand; result_type } ->
      `List [ `String "BUnOp"; `Assoc [ ("op", unop op); ("operand", expr operand); ("result_type", ty result_type) ] ]
  | BSelect { array; index } -> `List [ `String "BSelect"; `Assoc [ ("array", expr array); ("index", expr index) ] ]
  | BSlice { signal; msb; lsb } -> `List [ `String "BSlice"; `Assoc [ ("signal", expr signal); ("msb", `Int msb); ("lsb", `Int lsb) ] ]
  | BConcat l -> `List [ `String "BConcat"; `List (List.map expr l) ]
  | BReplicate { count; value } -> `List [ `String "BReplicate"; `Assoc [ ("count", `Int count); ("value", expr value) ] ]
  | BCond { condition; then_val; else_val } ->
      `List [ `String "BCond"; `Assoc [ ("condition", expr condition); ("then_val", expr then_val); ("else_val", expr else_val) ] ]
  | BCall { func; args } -> `List [ `String "BCall"; `Assoc [ ("func", `String func); ("args", `List (List.map expr args)) ] ]

let rec stmt = function
  | BAssign { lhs; rhs } -> `List [ `String "BAssign"; `Assoc [ ("lhs", `String lhs); ("rhs", expr rhs) ] ]
  | BIf { condition; then_stmts; else_stmts } ->
      `List [ `String "BIf"; `Assoc [ ("condition", expr condition); ("then_stmts", stmts then_stmts); ("else_stmts", stmts else_stmts) ] ]
  | BCase { selector; cases; default } ->
      `List [ `String "BCase"; `Assoc [ ("selector", expr selector)
                                      ; ("cases", `List (List.map (fun (k, b) -> `List [ expr k; stmts b ]) cases))
                                      ; ("default", stmts default) ] ]
  | BWhile { condition; body } -> `List [ `String "BWhile"; `Assoc [ ("condition", expr condition); ("body", stmts body) ] ]
  | BFor { init; condition; update; body } ->
      `List [ `String "BFor"; `Assoc [ ("init", stmt init); ("condition", expr condition); ("update", stmt update); ("body", stmts body) ] ]
  | BBlock b -> `List [ `String "BBlock"; stmts b ]
  | BCallStmt { func; args } -> `List [ `String "BCallStmt"; `Assoc [ ("func", `String func); ("args", `List (List.map expr args)) ] ]
  | BReturn e -> `List [ `String "BReturn"; (match e with Some e -> expr e | None -> `Null) ]
and stmts l = `List (List.map stmt l)

let proc = function
  | BCombinational { name; body; _ } -> `List [ `String "BCombinational"; `Assoc [ ("name", `String name); ("body", stmts body) ] ]
  | BSequential { name; clock; reset; body; blocking_vars; _ } ->
      `List [ `String "BSequential"; `Assoc [ ("name", `String name); ("clock", `String clock)
                                            ; ("reset", (match reset with Some r -> `String r | None -> `Null))
                                            ; ("blocking_vars", `List (List.map (fun v -> `String v) blocking_vars))
                                            ; ("body", stmts body) ] ]

let signal (s : bsignal) =
  `Assoc [ ("name", `String s.name); ("stype", ty s.stype)
         ; ("direction", `String (match s.direction with `Input -> "Input" | `Output -> "Output" | `Internal -> "Internal" | `Inout -> "Inout")) ]

let bmodule_to_json (m : bmodule) : Yojson.Safe.t =
  `Assoc [ ("name", `String m.name)
         ; ("signals", `List (List.map signal m.signals))
         ; ("processes", `List (List.map proc m.processes))
         ; ("funcs", `List (List.map (fun (f : bfunc) ->
             `Assoc [ ("fname", `String f.fname); ("is_task", `Bool f.is_task)
                    ; ("params", `List (List.map (fun (n, t, _) -> `List [ `String n; ty t ]) f.params))
                    ; ("locals", `List (List.map (fun (n, t) -> `List [ `String n; ty t ]) f.locals))
                    ; ("body", stmts f.body) ]) m.funcs))
         ; ("instances", `List (List.map (fun (i : binstance) ->
             `Assoc [ ("inst_name", `String i.inst_name); ("module_name", `String i.module_name)
                    ; ("port_connections", `List (List.map (fun (p, e) -> `List [ `String p; expr e ]) i.port_connections)) ]) m.instances)) ]
