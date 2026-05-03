(* Minimal type stubs for the Surelog UHDM dump grammar/lexer.
 *
 * The grammar (surelog_uhdm.mly) and lexer (surelog_uhdm_lex.mll)
 * were lifted from $HOME/hardcaml-lua.0.0.1 (Input.mly / Input_lex.mll).
 * The original code references `Dump_types.cexp`, `Dump_types.dirop`,
 * `Dump_types.cmpop` for token payloads — this file extracts just those
 * three so we don't pull the full hardcaml-lua dependency tree. *)

type cmpop =
  | Cunknown
  | Ceq | Cneq
  | Cgt | Cgts
  | Cgte | Cgtes
  | Ceqwild | Cneqwild
  | Clt | Clts
  | Clte | Cltes
  | Cnewild | Cgewild

type dirop =
  | Dunknown
  | Dinput
  | Doutput
  | Dinout
  | Dvif of string ref
  | Dinam of string

type arithop =
  | Aunknown
  | Aadd of string
  | Asub of string
  | Amul of string
  | Amuls of string
  | Adiv | Adivs
  | Amod | Amods
  | Apow | Apows
  | Ashiftl | Ashiftr
  | Aashiftr

type cexp =
  | ERR of string
  | BIN of char
  | HEX of int
  | SHEX of int
  | STRING of string
  | ENUMVAL of int * string
  | FLT of float
  | BIGINT of Int64.t
  | CNSTEXP of arithop * cexp list
