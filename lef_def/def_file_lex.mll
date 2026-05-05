(**************************************************************************)
(* DEF (Design Exchange Format) lexer.                                    *)
(* Structure adapted from lef_file_lex.mll; keyword set regenerated from  *)
(* def_file.mly's K_* tokens.                                             *)
(**************************************************************************)

{
  open Lexing
  open Def_file

  let verbose = ref false
  let lincnt = ref 0

  let keyword =
    let h = Hashtbl.create 257 in
    List.iter
      (fun (k,s) -> Hashtbl.add h s k)
      [
    K_ALIGN, "ALIGN";
    K_ANALOG, "ANALOG";
    K_AND, "AND";
    K_ANTENNAMODEL, "ANTENNAMODEL";
    K_ANTENNAPINDIFFAREA, "ANTENNAPINDIFFAREA";
    K_ANTENNAPINGATEAREA, "ANTENNAPINGATEAREA";
    K_ANTENNAPINMAXAREACAR, "ANTENNAPINMAXAREACAR";
    K_ANTENNAPINMAXCUTCAR, "ANTENNAPINMAXCUTCAR";
    K_ANTENNAPINMAXSIDEAREACAR, "ANTENNAPINMAXSIDEAREACAR";
    K_ANTENNAPINPARTIALCUTAREA, "ANTENNAPINPARTIALCUTAREA";
    K_ANTENNAPINPARTIALMETALAREA, "ANTENNAPINPARTIALMETALAREA";
    K_ANTENNAPINPARTIALMETALSIDEAREA, "ANTENNAPINPARTIALMETALSIDEAREA";
    K_ARRAY, "ARRAY";
    K_ASSERTIONS, "ASSERTIONS";
    K_BALANCED, "BALANCED";
    K_BEGINEXT, "BEGINEXT";
    K_BLOCKAGES, "BLOCKAGES";
    K_BLOCKAGEWIRE, "BLOCKAGEWIRE";
    K_BLOCKRING, "BLOCKRING";
    K_BLOCKWIRE, "BLOCKWIRE";
    K_BOTTOMLEFT, "BOTTOMLEFT";
    K_BUSBITCHARS, "BUSBITCHARS";
    K_BY, "BY";
    K_CANNOTOCCUPY, "CANNOTOCCUPY";
    K_CANPLACE, "CANPLACE";
    K_CAPACITANCE, "CAPACITANCE";
    K_CLOCK, "CLOCK";
    K_COMMONSCANPINS, "COMMONSCANPINS";
    K_COMPONENT, "COMPONENT";
    K_COMPONENTPIN, "COMPONENTPIN";
    K_COMPS, "COMPS";
    K_COMPSMASKSHIFT, "COMPSMASKSHIFT";
    K_COMP_GEN, "COMP_GEN";
    K_CONSTRAINTS, "CONSTRAINTS";
    K_COREWIRE, "COREWIRE";
    K_COVER, "COVER";
    K_CUTSIZE, "CUTSIZE";
    K_CUTSPACING, "CUTSPACING";
    K_DEFAULTCAP, "DEFAULTCAP";
    K_DEFINE, "DEFINE";
    K_DEFINEB, "DEFINEB";
    K_DEFINES, "DEFINES";
    K_DESIGN, "DESIGN";
    K_DESIGNRULEWIDTH, "DESIGNRULEWIDTH";
    K_DIAGWIDTH, "DIAGWIDTH";
    K_DIEAREA, "DIEAREA";
    K_DIFF, "DIFF";
    K_DIRECTION, "DIRECTION";
    K_DIST, "DIST";
    K_DISTANCE, "DISTANCE";
    K_DIVIDERCHAR, "DIVIDERCHAR";
    K_DO, "DO";
    K_DRCFILL, "DRCFILL";
    K_DRIVECELL, "DRIVECELL";
    K_E, "E";
    K_EEQMASTER, "EEQMASTER";
    K_ELSE, "ELSE";
    K_ENCLOSURE, "ENCLOSURE";
    K_END, "END";
    K_ENDEXT, "ENDEXT";
    K_EQ, "EQ";
    K_EQUAL, "EQUAL";
    K_ESTCAP, "ESTCAP";
    K_EXCEPTPGNET, "EXCEPTPGNET";
    K_FALL, "FALL";
    K_FALLMAX, "FALLMAX";
    K_FALLMIN, "FALLMIN";
    K_FALSE, "FALSE";
    K_FE, "FE";
    K_FENCE, "FENCE";
    K_FILLS, "FILLS";
    K_FILLWIRE, "FILLWIRE";
    K_FILLWIREOPC, "FILLWIREOPC";
    K_FIXED, "FIXED";
    K_FIXEDBUMP, "FIXEDBUMP";
    K_FLOATING, "FLOATING";
    K_FLOORPLAN, "FLOORPLAN";
    K_FN, "FN";
    K_FOLLOWPIN, "FOLLOWPIN";
    K_FOREIGN, "FOREIGN";
    K_FPC, "FPC";
    K_FREQUENCY, "FREQUENCY";
    K_FROMCLOCKPIN, "FROMCLOCKPIN";
    K_FROMCOMPPIN, "FROMCOMPPIN";
    K_FROMIOPIN, "FROMIOPIN";
    K_FROMPIN, "FROMPIN";
    K_FS, "FS";
    K_FW, "FW";
    K_GCELLGRID, "GCELLGRID";
    K_GE, "GE";
    K_GROUND, "GROUND";
    K_GROUNDSENSITIVITY, "GROUNDSENSITIVITY";
    K_GROUP, "GROUP";
    K_GROUPS, "GROUPS";
    K_GT, "GT";
    K_GUIDE, "GUIDE";
    K_HALO, "HALO";
    K_HARDSPACING, "HARDSPACING";
    K_HISTORY, "HISTORY";
    K_HOLDFALL, "HOLDFALL";
    K_HOLDRISE, "HOLDRISE";
    K_HORIZONTAL, "HORIZONTAL";
    K_IF, "IF";
    K_IN, "IN";
    K_INTEGER, "INTEGER";
    K_IOTIMINGS, "IOTIMINGS";
    K_IOWIRE, "IOWIRE";
    K_LAYER, "LAYER";
    K_LAYERS, "LAYERS";
    K_LE, "LE";
    K_LT, "LT";
    K_MACRO, "MACRO";
    K_MASK, "MASK";
    K_MASKSHIFT, "MASKSHIFT";
    K_MAX, "MAX";
    K_MAXBITS, "MAXBITS";
    K_MAXDIST, "MAXDIST";
    K_MAXHALFPERIMETER, "MAXHALFPERIMETER";
    K_MAXX, "MAXX";
    K_MAXY, "MAXY";
    K_MICRONS, "MICRONS";
    K_MIN, "MIN";
    K_MINCUTS, "MINCUTS";
    K_MINPINS, "MINPINS";
    K_MUSTJOIN, "MUSTJOIN";
    K_N, "N";
    K_NAMEMAPSTRING, "NAMEMAPSTRING";
    K_NAMESCASESENSITIVE, "NAMESCASESENSITIVE";
    K_NE, "NE";
    K_NET, "NET";
    K_NETEXPR, "NETEXPR";
    K_NETLIST, "NETLIST";
    K_NETS, "NETS";
    K_NEW, "NEW";
    K_NONDEFAULTRULE, "NONDEFAULTRULE";
    K_NONDEFAULTRULES, "NONDEFAULTRULES";
    K_NOSHIELD, "NOSHIELD";
    K_NOT, "NOT";
    K_OFF, "OFF";
    K_OFFSET, "OFFSET";
    K_ON, "ON";
    K_OPC, "OPC";
    K_OR, "OR";
    K_ORDERED, "ORDERED";
    K_ORIGIN, "ORIGIN";
    K_ORIGINAL, "ORIGINAL";
    K_OUT, "OUT";
    K_OXIDE1, "OXIDE1";
    K_OXIDE2, "OXIDE2";
    K_OXIDE3, "OXIDE3";
    K_OXIDE4, "OXIDE4";
    K_PADRING, "PADRING";
    K_PARALLEL, "PARALLEL";
    K_PARTIAL, "PARTIAL";
    K_PARTITION, "PARTITION";
    K_PARTITIONS, "PARTITIONS";
    K_PATH, "PATH";
    K_PATTERN, "PATTERN";
    K_PATTERNNAME, "PATTERNNAME";
    K_PIN, "PIN";
    K_PINPROPERTIES, "PINPROPERTIES";
    K_PINS, "PINS";
    K_PLACED, "PLACED";
    K_PLACEMENT, "PLACEMENT";
    K_POLYGON, "POLYGON";
    K_PORT, "PORT";
    K_POWER, "POWER";
    K_PROPERTY, "PROPERTY";
    K_PROPERTYDEFINITIONS, "PROPERTYDEFINITIONS";
    K_PUSHDOWN, "PUSHDOWN";
    K_RANGE, "RANGE";
    K_REAL, "REAL";
    K_RECT, "RECT";
    K_REENTRANTPATHS, "REENTRANTPATHS";
    K_REGION, "REGION";
    K_REGIONS, "REGIONS";
    K_RESET, "RESET";
    K_RING, "RING";
    K_RISE, "RISE";
    K_RISEMAX, "RISEMAX";
    K_RISEMIN, "RISEMIN";
    K_ROUTED, "ROUTED";
    K_ROUTEHALO, "ROUTEHALO";
    K_ROW, "ROW";
    K_ROWCOL, "ROWCOL";
    K_ROWS, "ROWS";
    K_S, "S";
    K_SAMEMASK, "SAMEMASK";
    K_SCAN, "SCAN";
    K_SCANCHAINS, "SCANCHAINS";
    K_SETUPFALL, "SETUPFALL";
    K_SETUPRISE, "SETUPRISE";
    K_SHAPE, "SHAPE";
    K_SHIELD, "SHIELD";
    K_SHIELDNET, "SHIELDNET";
    K_SIGNAL, "SIGNAL";
    K_SITE, "SITE";
    K_SLEWRATE, "SLEWRATE";
    K_SLOTS, "SLOTS";
    K_SNET, "SNET";
    K_SNETS, "SNETS";
    K_SOFT, "SOFT";
    K_SOURCE, "SOURCE";
    K_SPACING, "SPACING";
    K_SPECIAL, "SPECIAL";
    K_START, "START";
    K_START_NET, "START_NET";
    K_STEINER, "STEINER";
    K_STEP, "STEP";
    K_STOP, "STOP";
    K_STRING, "STRING";
    K_STRIPE, "STRIPE";
    K_STYLE, "STYLE";
    K_STYLES, "STYLES";
    K_SUBNET, "SUBNET";
    K_SUM, "SUM";
    K_SUPPLYSENSITIVITY, "SUPPLYSENSITIVITY";
    K_SYNTHESIZED, "SYNTHESIZED";
    K_TAPER, "TAPER";
    K_TAPERRULE, "TAPERRULE";
    K_TECH, "TECH";
    K_TEST, "TEST";
    K_THEN, "THEN";
    K_THRUPIN, "THRUPIN";
    K_TIEOFF, "TIEOFF";
    K_TIMING, "TIMING";
    K_TIMINGDISABLES, "TIMINGDISABLES";
    K_TOCLOCKPIN, "TOCLOCKPIN";
    K_TOCOMPPIN, "TOCOMPPIN";
    K_TOIOPIN, "TOIOPIN";
    K_TOPIN, "TOPIN";
    K_TOPRIGHT, "TOPRIGHT";
    K_TRACKS, "TRACKS";
    K_TRUE, "TRUE";
    K_TRUNK, "TRUNK";
    K_TURNOFF, "TURNOFF";
    K_TYPE, "TYPE";
    K_UNITS, "UNITS";
    K_UNPLACED, "UNPLACED";
    K_USE, "USE";
    K_USER, "USER";
    K_VARIABLE, "VARIABLE";
    K_VERSION, "VERSION";
    K_VERTICAL, "VERTICAL";
    K_VIA, "VIA";
    K_VIARULE, "VIARULE";
    K_VIAS, "VIAS";
    K_VIRTUAL, "VIRTUAL";
    K_VOLTAGE, "VOLTAGE";
    K_VPIN, "VPIN";
    K_W, "W";
    K_WEIGHT, "WEIGHT";
    K_WIDTH, "WIDTH";
    K_WIRECAP, "WIRECAP";
    K_WIREDLOGIC, "WIREDLOGIC";
    K_WIREEXT, "WIREEXT";
    K_X, "X";
    K_XTALK, "XTALK";
    K_Y, "Y"
      ];
    fun s -> Hashtbl.find h (String.uppercase_ascii s)

  let tok arg =
    if !verbose then print_endline (Def_file_tokens.getstr arg);
    arg
}

let ident   = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*
let digit   = ['0'-'9']
let int     = ['-' '+']? digit+
let fltnum  = ['-' '+']? digit+ ('.' digit*)? (['e' 'E'] ['-' '+']? digit+)?
let percent = digit+'%'
let space   = [' ' '\t' '\r']+
let newline = ['\n']
let qstring = '"'[^'"']*'"'
let ampident = '&'[^' ']*
let comment = '#'[^'\n']*

rule token = parse
  | comment           { token lexbuf }
  | space             { token lexbuf }
  | newline           { incr lincnt; token lexbuf }
  | percent as s      { tok ( STRING s ) }
  | fltnum as n       { tok ( NUMBER (int_of_float (float_of_string n)) ) }
  | int as n          { tok ( NUMBER (int_of_string n) ) }
  | ident as s        { tok ( try keyword s with Not_found -> STRING s ) }
  | ampident as s     { tok ( try keyword s with Not_found -> STRING s ) }
  | qstring as s      { tok ( QSTRING s ) }
  | eof               { tok ( EOF_TOKEN ) }
  | '!'  { tok ( PLING ) }
  | '"'  { tok ( DOUBLEQUOTE ) }
  | '#'  { tok ( HASH ) }
  | '$'  { tok ( DOLLAR ) }
  | '%'  { tok ( PERCENT ) }
  | '&'  { tok ( AMPERSAND ) }
  | '\'' { tok ( QUOTE ) }
  | '('  { tok ( LPAREN ) }
  | '['  { tok ( LBRACK ) }
  | '{'  { tok ( LBRACE ) }
  | '<'  { tok ( LESS ) }
  | ')'  { tok ( RPAREN ) }
  | ']'  { tok ( RBRACK ) }
  | '}'  { tok ( RBRACE ) }
  | '>'  { tok ( GREATER ) }
  | '*'  { tok ( STAR ) }
  | '+'  { tok ( PLUS ) }
  | ','  { tok ( COMMA ) }
  | '-'  { tok ( HYPHEN ) }
  | '.'  { tok ( DOT ) }
  | '\\' { tok ( BACKSLASH ) }
  | ':'  { tok ( COLON ) }
  | ';'  { tok ( SEMICOLON ) }
  | '?'  { tok ( QUERY ) }
  | '@'  { tok ( AT ) }
  | '^'  { tok ( CARET ) }
  | '_'  { tok ( UNDERSCORE ) }
  | '`' { tok ( BACKQUOTE ) }
  | '|'  { tok ( VBAR ) }
  | '~'  { tok ( TILDE ) }
  | _ as oth { tok ( failwith ("def_file_lex: unrecognised char '"^String.make 1 oth^"'") ) }
