(* JSON dumping utility for vhdintf structures *)

[@@@warning "-8"]  (* Suppress non-exhaustive pattern matching warnings *)

open Vhd_front.VhdlTree

(* Convert vhdintf tag to string *)
let tag_to_string vhd =
  match vhd with
  | VhdNone -> "VhdNone"
  | Str _ -> "Str"
  | Char _ -> "Char"
  | Real _ -> "Real"
  | Num _ -> "Num"
  | List _ -> "List"
  | Double (_, _) -> "Double"
  | Triple (_, _, _) -> "Triple"
  | Quadruple (_, _, _, _) -> "Quadruple"
  | Quintuple (_, _, _, _, _) -> "Quintuple"
  | Sextuple (_, _, _, _, _, _) -> "Sextuple"
  | Septuple (_, _, _, _, _, _, _) -> "Septuple"
  | Octuple (_, _, _, _, _, _, _, _) -> "Octuple"
  | Nonuple (_, _, _, _, _, _, _, _, _) -> "Nonuple"
  | Decuple (_, _, _, _, _, _, _, _, _, _) -> "Decuple"
  | _ -> "Other"  (* Catch-all for higher-arity constructors *)

(* Limit recursion depth to avoid huge JSON files *)
let max_depth = 5

(* Convert vhdintf to JSON *)
let rec to_json ?(depth=0) vhd =
  if depth > max_depth then
    `String "<truncated>"
  else
    match vhd with
    | VhdNone -> `Assoc ["type", `String "VhdNone"]
    | Str s -> `Assoc ["type", `String "Str"; "value", `String s]
    | Char c -> `Assoc ["type", `String "Char"; "value", `String (String.make 1 c)]
    | Real f -> `Assoc ["type", `String "Real"; "value", `Float f]
    | Num s -> `Assoc ["type", `String "Num"; "value", `String s]
    | List items ->
        `Assoc [
          "type", `String "List";
          "items", `List (List.map (to_json ~depth:(depth+1)) items)
        ]
    | Double (tag, a) ->
        `Assoc [
          "type", `String "Double";
          "tag", to_json ~depth:(depth+1) tag;
          "field1", to_json ~depth:(depth+1) a
        ]
    | Triple (tag, a, b) ->
        `Assoc [
          "type", `String "Triple";
          "tag", to_json ~depth:(depth+1) tag;
          "field1", to_json ~depth:(depth+1) a;
          "field2", to_json ~depth:(depth+1) b
        ]
    | Quadruple (tag, a, b, c) ->
        `Assoc [
          "type", `String "Quadruple";
          "tag", to_json ~depth:(depth+1) tag;
          "field1", to_json ~depth:(depth+1) a;
          "field2", to_json ~depth:(depth+1) b;
          "field3", to_json ~depth:(depth+1) c
        ]
    | Quintuple (tag, a, b, c, d) ->
        `Assoc [
          "type", `String "Quintuple";
          "tag", to_json ~depth:(depth+1) tag;
          "field1", to_json ~depth:(depth+1) a;
          "field2", to_json ~depth:(depth+1) b;
          "field3", to_json ~depth:(depth+1) c;
          "field4", to_json ~depth:(depth+1) d
        ]
    | Sextuple (tag, a, b, c, d, e) ->
        `Assoc [
          "type", `String "Sextuple";
          "tag", to_json ~depth:(depth+1) tag;
          "field1", to_json ~depth:(depth+1) a;
          "field2", to_json ~depth:(depth+1) b;
          "field3", to_json ~depth:(depth+1) c;
          "field4", to_json ~depth:(depth+1) d;
          "field5", to_json ~depth:(depth+1) e
        ]
    | Septuple (tag, a, b, c, d, e, f) ->
        `Assoc [
          "type", `String "Septuple";
          "tag", to_json ~depth:(depth+1) tag;
          "field1", to_json ~depth:(depth+1) a;
          "field2", to_json ~depth:(depth+1) b;
          "field3", to_json ~depth:(depth+1) c;
          "field4", to_json ~depth:(depth+1) d;
          "field5", to_json ~depth:(depth+1) e;
          "field6", to_json ~depth:(depth+1) f
        ]
    | Octuple (tag, a, b, c, d, e, f, g) ->
        `Assoc [
          "type", `String "Octuple";
          "tag", to_json ~depth:(depth+1) tag;
          "field1", to_json ~depth:(depth+1) a;
          "field2", to_json ~depth:(depth+1) b;
          "field3", to_json ~depth:(depth+1) c;
          "field4", to_json ~depth:(depth+1) d;
          "field5", to_json ~depth:(depth+1) e;
          "field6", to_json ~depth:(depth+1) f;
          "field7", to_json ~depth:(depth+1) g
        ]
    | Nonuple (tag, a, b, c, d, e, f, g, h) ->
        `Assoc [
          "type", `String "Nonuple";
          "tag", to_json ~depth:(depth+1) tag;
          "field1", to_json ~depth:(depth+1) a;
          "field2", to_json ~depth:(depth+1) b;
          "field3", to_json ~depth:(depth+1) c;
          "field4", to_json ~depth:(depth+1) d;
          "field5", to_json ~depth:(depth+1) e;
          "field6", to_json ~depth:(depth+1) f;
          "field7", to_json ~depth:(depth+1) g;
          "field8", to_json ~depth:(depth+1) h
        ]
    | Decuple (tag, a, b, c, d, e, f, g, h, i) ->
        `Assoc [
          "type", `String "Decuple";
          "tag", to_json ~depth:(depth+1) tag;
          "field1", to_json ~depth:(depth+1) a;
          "field2", to_json ~depth:(depth+1) b;
          "field3", to_json ~depth:(depth+1) c;
          "field4", to_json ~depth:(depth+1) d;
          "field5", to_json ~depth:(depth+1) e;
          "field6", to_json ~depth:(depth+1) f;
          "field7", to_json ~depth:(depth+1) g;
          "field8", to_json ~depth:(depth+1) h;
          "field9", to_json ~depth:(depth+1) i
        ]
    | _ ->
        (* Catch-all for higher-arity tuples *)
        `Assoc [
          "type", `String "HighArityTuple";
          "description", `String "<truncated - too many fields>"
        ]

(* Dump unhandled pattern to JSON file *)
let dump_unhandled context pattern_name vhd =
  let filename = Printf.sprintf "unhandled_%s.json" pattern_name in
  let json = `Assoc [
    "context", `String context;
    "pattern", `String pattern_name;
    "structure", to_json vhd
  ] in
  let oc = open_out filename in
  Yojson.Basic.pretty_to_channel oc json;
  close_out oc;
  Printf.printf "  ⚠️  Unhandled %s: dumped to %s\n" pattern_name filename

(* Get compact description for logging *)
let rec get_description vhd =
  match vhd with
  | VhdNone -> "VhdNone"
  | Str s -> Printf.sprintf "Str(%s)" (if String.length s > 20 then String.sub s 0 20 ^ "..." else s)
  | Char c -> Printf.sprintf "Char(%c)" c
  | Real f -> Printf.sprintf "Real(%f)" f
  | Num s -> Printf.sprintf "Num(%s)" s
  | List items -> Printf.sprintf "List[%d items]" (List.length items)
  | Double (tag, _) -> Printf.sprintf "Double(%s, ...)" (get_description tag)
  | Triple (tag, _, _) -> Printf.sprintf "Triple(%s, ...)" (get_description tag)
  | Quadruple (tag, _, _, _) -> Printf.sprintf "Quadruple(%s, ...)" (get_description tag)
  | Quintuple (tag, _, _, _, _) -> Printf.sprintf "Quintuple(%s, ...)" (get_description tag)
  | Sextuple (tag, _, _, _, _, _) -> Printf.sprintf "Sextuple(%s, ...)" (get_description tag)
  | Septuple (tag, _, _, _, _, _, _) -> Printf.sprintf "Septuple(%s, ...)" (get_description tag)
  | Octuple (tag, _, _, _, _, _, _, _) -> Printf.sprintf "Octuple(%s, ...)" (get_description tag)
  | Nonuple (tag, _, _, _, _, _, _, _, _) -> Printf.sprintf "Nonuple(%s, ...)" (get_description tag)
  | Decuple (tag, _, _, _, _, _, _, _, _, _) -> Printf.sprintf "Decuple(%s, ...)" (get_description tag)
  | _ -> "HighArityTuple(...)"  (* Catch-all for higher-arity tuples *)
