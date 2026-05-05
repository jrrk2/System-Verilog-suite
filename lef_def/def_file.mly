%{
  open Def_file_types
  let _ = (packhash, typehash) (* silence unused-open *)
%}

%token  ACCEPT
%token  AMPERSAND
%token  AT
%token  BACKQUOTE
%token  BACKSLASH
%token  CARET
%token  COLON
%token  COMMA
%token <token> CONS1
%token <token*token> CONS2
%token <token*token*token> CONS3
%token <token*token*token*token> CONS4
%token <token*token*token*token*token> CONS5
%token  DEFAULT
%token  DOLLAR
%token  DOT
%token  DOUBLEQUOTE
%token <token list> ELIST
%token  EMPTY_TOKEN
%token  END
%token  EOF_TOKEN
%token  ERROR_TOKEN
%token  Error
%token  GREATER
%token  HASH
%token  HYPHEN
%token  K_ALIGN
%token  K_ANALOG
%token  K_AND
%token  K_ANTENNAMODEL
%token  K_ANTENNAPINDIFFAREA
%token  K_ANTENNAPINGATEAREA
%token  K_ANTENNAPINMAXAREACAR
%token  K_ANTENNAPINMAXCUTCAR
%token  K_ANTENNAPINMAXSIDEAREACAR
%token  K_ANTENNAPINPARTIALCUTAREA
%token  K_ANTENNAPINPARTIALMETALAREA
%token  K_ANTENNAPINPARTIALMETALSIDEAREA
%token  K_ARRAY
%token  K_ASSERTIONS
%token  K_BALANCED
%token  K_BEGINEXT
%token  K_BLOCKAGES
%token  K_BLOCKAGEWIRE
%token  K_BLOCKRING
%token  K_BLOCKWIRE
%token  K_BOTTOMLEFT
%token  K_BUSBITCHARS
%token  K_BY
%token  K_CANNOTOCCUPY
%token  K_CANPLACE
%token  K_CAPACITANCE
%token  K_CLOCK
%token  K_COMMONSCANPINS
%token  K_COMPONENT
%token  K_COMPONENTPIN
%token  K_COMPS
%token  K_COMPSMASKSHIFT
%token  K_COMP_GEN
%token  K_CONSTRAINTS
%token  K_COREWIRE
%token  K_COVER
%token  K_CUTSIZE
%token  K_CUTSPACING
%token  K_DEFAULTCAP
%token  K_DEFINE
%token  K_DEFINEB
%token  K_DEFINES
%token  K_DESIGN
%token  K_DESIGNRULEWIDTH
%token  K_DIAGWIDTH
%token  K_DIEAREA
%token  K_DIFF
%token  K_DIRECTION
%token  K_DIST
%token  K_DISTANCE
%token  K_DIVIDERCHAR
%token  K_DO
%token  K_DRCFILL
%token  K_DRIVECELL
%token  K_E
%token  K_EEQMASTER
%token  K_ELSE
%token  K_ENCLOSURE
%token  K_END
%token  K_ENDEXT
%token  K_EQ
%token  K_EQUAL
%token  K_ESTCAP
%token  K_EXCEPTPGNET
%token  K_FALL
%token  K_FALLMAX
%token  K_FALLMIN
%token  K_FALSE
%token  K_FE
%token  K_FENCE
%token  K_FILLS
%token  K_FILLWIRE
%token  K_FILLWIREOPC
%token  K_FIXED
%token  K_FIXEDBUMP
%token  K_FLOATING
%token  K_FLOORPLAN
%token  K_FN
%token  K_FOLLOWPIN
%token  K_FOREIGN
%token  K_FPC
%token  K_FREQUENCY
%token  K_FROMCLOCKPIN
%token  K_FROMCOMPPIN
%token  K_FROMIOPIN
%token  K_FROMPIN
%token  K_FS
%token  K_FW
%token  K_GCELLGRID
%token  K_GE
%token  K_GROUND
%token  K_GROUNDSENSITIVITY
%token  K_GROUP
%token  K_GROUPS
%token  K_GT
%token  K_GUIDE
%token  K_HALO
%token  K_HARDSPACING
%token  K_HISTORY
%token  K_HOLDFALL
%token  K_HOLDRISE
%token  K_HORIZONTAL
%token  K_IF
%token  K_IN
%token  K_INTEGER
%token  K_IOTIMINGS
%token  K_IOWIRE
%token  K_LAYER
%token  K_LAYERS
%token  K_LE
%token  K_LT
%token  K_MACRO
%token  K_MASK
%token  K_MASKSHIFT
%token  K_MAX
%token  K_MAXBITS
%token  K_MAXDIST
%token  K_MAXHALFPERIMETER
%token  K_MAXX
%token  K_MAXY
%token  K_MICRONS
%token  K_MIN
%token  K_MINCUTS
%token  K_MINPINS
%token  K_MUSTJOIN
%token  K_N
%token  K_NAMEMAPSTRING
%token  K_NAMESCASESENSITIVE
%token  K_NE
%token  K_NET
%token  K_NETEXPR
%token  K_NETLIST
%token  K_NETS
%token  K_NEW
%token  K_NONDEFAULTRULE
%token  K_NONDEFAULTRULES
%token  K_NOSHIELD
%token  K_NOT
%token  K_OFF
%token  K_OFFSET
%token  K_ON
%token  K_OPC
%token  K_OR
%token  K_ORDERED
%token  K_ORIGIN
%token  K_ORIGINAL
%token  K_OUT
%token  K_OXIDE1
%token  K_OXIDE2
%token  K_OXIDE3
%token  K_OXIDE4
%token  K_PADRING
%token  K_PARALLEL
%token  K_PARTIAL
%token  K_PARTITION
%token  K_PARTITIONS
%token  K_PATH
%token  K_PATTERN
%token  K_PATTERNNAME
%token  K_PIN
%token  K_PINPROPERTIES
%token  K_PINS
%token  K_PLACED
%token  K_PLACEMENT
%token  K_POLYGON
%token  K_PORT
%token  K_POWER
%token  K_PROPERTY
%token  K_PROPERTYDEFINITIONS
%token  K_PUSHDOWN
%token  K_RANGE
%token  K_REAL
%token  K_RECT
%token  K_REENTRANTPATHS
%token  K_REGION
%token  K_REGIONS
%token  K_RESET
%token  K_RING
%token  K_RISE
%token  K_RISEMAX
%token  K_RISEMIN
%token  K_ROUTED
%token  K_ROUTEHALO
%token  K_ROW
%token  K_ROWCOL
%token  K_ROWS
%token  K_S
%token  K_SAMEMASK
%token  K_SCAN
%token  K_SCANCHAINS
%token  K_SETUPFALL
%token  K_SETUPRISE
%token  K_SHAPE
%token  K_SHIELD
%token  K_SHIELDNET
%token  K_SIGNAL
%token  K_SITE
%token  K_SLEWRATE
%token  K_SLOTS
%token  K_SNET
%token  K_SNETS
%token  K_SOFT
%token  K_SOURCE
%token  K_SPACING
%token  K_SPECIAL
%token  K_START
%token  K_START_NET
%token  K_STEINER
%token  K_STEP
%token  K_STOP
%token  K_STRING
%token  K_STRIPE
%token  K_STYLE
%token  K_STYLES
%token  K_SUBNET
%token  K_SUM
%token  K_SUPPLYSENSITIVITY
%token  K_SYNTHESIZED
%token  K_TAPER
%token  K_TAPERRULE
%token  K_TECH
%token  K_TEST
%token  K_THEN
%token  K_THRUPIN
%token  K_TIEOFF
%token  K_TIMING
%token  K_TIMINGDISABLES
%token  K_TOCLOCKPIN
%token  K_TOCOMPPIN
%token  K_TOIOPIN
%token  K_TOPIN
%token  K_TOPRIGHT
%token  K_TRACKS
%token  K_TRUE
%token  K_TRUNK
%token  K_TURNOFF
%token  K_TYPE
%token  K_UNITS
%token  K_UNPLACED
%token  K_USE
%token  K_USER
%token  K_VARIABLE
%token  K_VERSION
%token  K_VERTICAL
%token  K_VIA
%token  K_VIARULE
%token  K_VIAS
%token  K_VIRTUAL
%token  K_VOLTAGE
%token  K_VPIN
%token  K_W
%token  K_WEIGHT
%token  K_WIDTH
%token  K_WIRECAP
%token  K_WIREDLOGIC
%token  K_WIREEXT
%token  K_X
%token  K_XTALK
%token  K_Y
%token  LBRACE
%token  LBRACK
%token  LESS
%token  LINEFEED
%token  LPAREN
%token <int> NUMBER
%token  PERCENT
%token  PLING
%token  PLUS
%token <string> QSTRING
%token  QUERY
%token  QUOTE
%token  RBRACE
%token  RBRACK
%token  RPAREN
%token  SEMICOLON
%token  SITE_PATTERN
%token <string list> SLIST
%token  STAR
%token <string> STRING
%token  TILDE
%token <token list> TLIST
%token <token*token*token*token*token*token*token*token*token*token> TUPLE10
%token <token*token*token*token*token*token*token*token*token*token*token> TUPLE11
%token <token*token*token*token*token*token*token*token*token*token*token*token> TUPLE12
%token <token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE13
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE14
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE15
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE16
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE17
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE18
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE19
%token <token*token> TUPLE2
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE20
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE21
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE22
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE23
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE24
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE25
%token <token*token*token> TUPLE3
%token <token*token*token*token> TUPLE4
%token <token*token*token*token*token> TUPLE5
%token <token*token*token*token*token*token> TUPLE6
%token <token*token*token*token*token*token*token> TUPLE7
%token <token*token*token*token*token*token*token*token> TUPLE8
%token <token*token*token*token*token*token*token*token*token> TUPLE9
%token  T_STRING
%token  UNDERSCORE
%token  VBAR
%type <token> ml_start
%start ml_start
%%


ml_start: def_file EOF_TOKEN { TUPLE3(STRING("ml_start1"),$1,EOF_TOKEN) }

def_file: version_stmt case_sens_stmt rules end_design { TUPLE5(STRING("def_file1"),$1,$2,$3,$4) }

version_stmt: /* empty */ { EMPTY_TOKEN }
	|	K_VERSION /* 1 */ T_STRING SEMICOLON { TUPLE4(STRING("version_stmt1"),K_VERSION,T_STRING,SEMICOLON) }

case_sens_stmt: /* empty */ { EMPTY_TOKEN }
	|	K_NAMESCASESENSITIVE K_ON SEMICOLON { TUPLE4(STRING("case_sens_stmt2"),K_NAMESCASESENSITIVE,K_ON,SEMICOLON) }
	|	K_NAMESCASESENSITIVE K_OFF SEMICOLON { TUPLE4(STRING("case_sens_stmt3"),K_NAMESCASESENSITIVE,K_OFF,SEMICOLON) }

rules: /* empty */ { EMPTY_TOKEN }
	|	rules rule { TUPLE3(STRING("rules2"),$1,$2) }
	|	ERROR_TOKEN { (ERROR_TOKEN) }

rule: design_section { ($1) }
	|	assertions_section { ($1) }
	|	blockage_section { ($1) }
	|	comps_section { ($1) }
	|	constraint_section { ($1) }
	|	extension_section { ($1) }
	|	fill_section { ($1) }
	|	comps_maskShift_section { ($1) }
	|	floorplan_contraints_section { ($1) }
	|	groups_section { ($1) }
	|	iotiming_section { ($1) }
	|	nets_section { ($1) }
	|	nondefaultrule_section { ($1) }
	|	partitions_section { ($1) }
	|	pin_props_section { ($1) }
	|	regions_section { ($1) }
	|	scanchains_section { ($1) }
	|	slot_section { ($1) }
	|	snets_section { ($1) }
	|	styles_section { ($1) }
	|	timingdisables_section { ($1) }
	|	via_section { ($1) }

design_section: array_name { ($1) }
	|	bus_bit_chars { ($1) }
	|	canplace { ($1) }
	|	cannotoccupy { ($1) }
	|	design_name { ($1) }
	|	die_area { ($1) }
	|	divider_char { ($1) }
	|	floorplan_name { ($1) }
	|	gcellgrid { ($1) }
	|	history { ($1) }
	|	pin_cap_rule { ($1) }
	|	pin_rule { ($1) }
	|	prop_def_section { ($1) }
	|	row_rule { ($1) }
	|	tech_name { ($1) }
	|	tracks_rule { ($1) }
	|	units { ($1) }

design_name: K_DESIGN /* 2 */ T_STRING SEMICOLON { TUPLE4(STRING("design_name1"),K_DESIGN,T_STRING,SEMICOLON) }

end_design: K_END K_DESIGN { TUPLE3(STRING("end_design1"),K_END,K_DESIGN) }

tech_name: K_TECH /* 3 */ T_STRING SEMICOLON { TUPLE4(STRING("tech_name1"),K_TECH,T_STRING,SEMICOLON) }

array_name: K_ARRAY /* 4 */ T_STRING SEMICOLON { TUPLE4(STRING("array_name1"),K_ARRAY,T_STRING,SEMICOLON) }

floorplan_name: K_FLOORPLAN /* 5 */ T_STRING SEMICOLON { TUPLE4(STRING("floorplan_name1"),K_FLOORPLAN,T_STRING,SEMICOLON) }

history: K_HISTORY { (K_HISTORY) }

prop_def_section: K_PROPERTYDEFINITIONS /* 6 */ property_defs K_END K_PROPERTYDEFINITIONS { TUPLE5(STRING("prop_def_section1"),K_PROPERTYDEFINITIONS,$2,K_END,K_PROPERTYDEFINITIONS) }

property_defs: /* empty */ { EMPTY_TOKEN }
	|	property_defs property_def { TUPLE3(STRING("property_defs2"),$1,$2) }

property_def: K_DESIGN /* 7 */ T_STRING property_type_and_val SEMICOLON { TUPLE5(STRING("property_def1"),K_DESIGN,T_STRING,$3,SEMICOLON) }
	|	K_NET /* 8 */ T_STRING property_type_and_val SEMICOLON { TUPLE5(STRING("property_def1"),K_NET,T_STRING,$3,SEMICOLON) }
	|	K_SNET /* 9 */ T_STRING property_type_and_val SEMICOLON { TUPLE5(STRING("property_def1"),K_SNET,T_STRING,$3,SEMICOLON) }
	|	K_REGION /* 10 */ T_STRING property_type_and_val SEMICOLON { TUPLE5(STRING("property_def1"),K_REGION,T_STRING,$3,SEMICOLON) }
	|	K_GROUP /* 11 */ T_STRING property_type_and_val SEMICOLON { TUPLE5(STRING("property_def1"),K_GROUP,T_STRING,$3,SEMICOLON) }
	|	K_COMPONENT /* 12 */ T_STRING property_type_and_val SEMICOLON { TUPLE5(STRING("property_def1"),K_COMPONENT,T_STRING,$3,SEMICOLON) }
	|	K_ROW /* 13 */ T_STRING property_type_and_val SEMICOLON { TUPLE5(STRING("property_def1"),K_ROW,T_STRING,$3,SEMICOLON) }
	|	K_COMPONENTPIN /* 14 */ T_STRING property_type_and_val SEMICOLON { TUPLE5(STRING("property_def1"),K_COMPONENTPIN,T_STRING,$3,SEMICOLON) }
	|	K_NONDEFAULTRULE /* 15 */ T_STRING property_type_and_val SEMICOLON { TUPLE5(STRING("property_def1"),K_NONDEFAULTRULE,T_STRING,$3,SEMICOLON) }
	|	ERROR_TOKEN SEMICOLON { TUPLE3(STRING("property_def2"),ERROR_TOKEN,SEMICOLON) }

property_type_and_val: K_INTEGER /* 16 */ opt_range opt_num_val { TUPLE4(STRING("property_type_and_val1"),K_INTEGER,$2,$3) }
	|	K_REAL /* 17 */ opt_range opt_num_val { TUPLE4(STRING("property_type_and_val1"),K_REAL,$2,$3) }
	|	K_STRING { (K_STRING) }
	|	K_STRING QSTRING { TUPLE3(STRING("property_type_and_val3"),K_STRING,QSTRING $2) }
	|	K_NAMEMAPSTRING T_STRING { TUPLE3(STRING("property_type_and_val4"),K_NAMEMAPSTRING,T_STRING) }

opt_num_val: /* empty */ { EMPTY_TOKEN }
	|	NUMBER { (NUMBER $1) }

units: K_UNITS K_DISTANCE K_MICRONS NUMBER SEMICOLON { TUPLE6(STRING("units1"),K_UNITS,K_DISTANCE,K_MICRONS,NUMBER $4,SEMICOLON) }

divider_char: K_DIVIDERCHAR QSTRING SEMICOLON { TUPLE4(STRING("divider_char1"),K_DIVIDERCHAR,QSTRING $2,SEMICOLON) }

bus_bit_chars: K_BUSBITCHARS QSTRING SEMICOLON { TUPLE4(STRING("bus_bit_chars1"),K_BUSBITCHARS,QSTRING $2,SEMICOLON) }

canplace: K_CANPLACE /* 18 */ T_STRING NUMBER NUMBER orient K_DO NUMBER K_BY NUMBER K_STEP NUMBER NUMBER SEMICOLON { TUPLE14(STRING("canplace1"),K_CANPLACE,T_STRING,NUMBER $3,NUMBER $4,$5,K_DO,NUMBER $7,K_BY,NUMBER $9,K_STEP,NUMBER $11,NUMBER $12,SEMICOLON) }

cannotoccupy: K_CANNOTOCCUPY /* 19 */ T_STRING NUMBER NUMBER orient K_DO NUMBER K_BY NUMBER K_STEP NUMBER NUMBER SEMICOLON { TUPLE14(STRING("cannotoccupy1"),K_CANNOTOCCUPY,T_STRING,NUMBER $3,NUMBER $4,$5,K_DO,NUMBER $7,K_BY,NUMBER $9,K_STEP,NUMBER $11,NUMBER $12,SEMICOLON) }

orient: K_N { (K_N) }
	|	K_W { (K_W) }
	|	K_S { (K_S) }
	|	K_E { (K_E) }
	|	K_FN { (K_FN) }
	|	K_FW { (K_FW) }
	|	K_FS { (K_FS) }
	|	K_FE { (K_FE) }

die_area: K_DIEAREA /* 20 */ firstPt nextPt otherPts SEMICOLON { TUPLE6(STRING("die_area1"),K_DIEAREA,$2,$3,$4,SEMICOLON) }

pin_cap_rule: start_def_cap pin_caps end_def_cap { TUPLE4(STRING("pin_cap_rule1"),$1,$2,$3) }

start_def_cap: K_DEFAULTCAP NUMBER { TUPLE3(STRING("start_def_cap1"),K_DEFAULTCAP,NUMBER $2) }

pin_caps: /* empty */ { EMPTY_TOKEN }
	|	pin_caps pin_cap { TUPLE3(STRING("pin_caps2"),$1,$2) }

pin_cap: K_MINPINS NUMBER K_WIRECAP NUMBER SEMICOLON { TUPLE6(STRING("pin_cap1"),K_MINPINS,NUMBER $2,K_WIRECAP,NUMBER $4,SEMICOLON) }

end_def_cap: K_END K_DEFAULTCAP { TUPLE3(STRING("end_def_cap1"),K_END,K_DEFAULTCAP) }

pin_rule: start_pins pins end_pins { TUPLE4(STRING("pin_rule1"),$1,$2,$3) }

start_pins: K_PINS NUMBER SEMICOLON { TUPLE4(STRING("start_pins1"),K_PINS,NUMBER $2,SEMICOLON) }

pins: /* empty */ { EMPTY_TOKEN }
	|	pins pin { TUPLE3(STRING("pins2"),$1,$2) }

pin: HYPHEN /* 21 */ T_STRING PLUS K_NET /* 22 */ T_STRING /* 23 */ pin_options SEMICOLON { TUPLE8(STRING("pin1"),HYPHEN,T_STRING,PLUS,K_NET,T_STRING,$6,SEMICOLON) }

pin_options: /* empty */ { EMPTY_TOKEN }
	|	pin_options pin_option { TUPLE3(STRING("pin_options2"),$1,$2) }

pin_option: PLUS K_SPECIAL { TUPLE3(STRING("pin_option1"),PLUS,K_SPECIAL) }
	|	extension_stmt { ($1) }
	|	PLUS K_DIRECTION T_STRING { TUPLE4(STRING("pin_option3"),PLUS,K_DIRECTION,T_STRING) }
	|	PLUS K_NETEXPR QSTRING { TUPLE4(STRING("pin_option4"),PLUS,K_NETEXPR,QSTRING $3) }
	|	PLUS K_SUPPLYSENSITIVITY /* 24 */ T_STRING { TUPLE4(STRING("pin_option1"),PLUS,K_SUPPLYSENSITIVITY,T_STRING) }
	|	PLUS K_GROUNDSENSITIVITY /* 25 */ T_STRING { TUPLE4(STRING("pin_option1"),PLUS,K_GROUNDSENSITIVITY,T_STRING) }
	|	PLUS K_USE use_type { TUPLE4(STRING("pin_option2"),PLUS,K_USE,$3) }
	|	PLUS K_PORT { TUPLE3(STRING("pin_option3"),PLUS,K_PORT) }
	|	PLUS K_LAYER /* 26 */ T_STRING /* 27 */ pin_layer_mask_opt pin_layer_spacing_opt pt pt { TUPLE8(STRING("pin_option1"),PLUS,K_LAYER,T_STRING,$4,$5,$6,$7) }
	|	PLUS K_POLYGON /* 28 */ T_STRING /* 29 */ pin_poly_mask_opt pin_poly_spacing_opt firstPt nextPt nextPt otherPts { TUPLE10(STRING("pin_option1"),PLUS,K_POLYGON,T_STRING,$4,$5,$6,$7,$8,$9) }
	|	PLUS K_VIA /* 30 */ T_STRING pin_via_mask_opt LPAREN NUMBER NUMBER RPAREN { TUPLE9(STRING("pin_option1"),PLUS,K_VIA,T_STRING,$4,LPAREN,NUMBER $6,NUMBER $7,RPAREN) }
	|	placement_status pt orient { TUPLE4(STRING("pin_option2"),$1,$2,$3) }
	|	PLUS K_ANTENNAPINPARTIALMETALAREA NUMBER pin_layer_opt { TUPLE5(STRING("pin_option3"),PLUS,K_ANTENNAPINPARTIALMETALAREA,NUMBER $3,$4) }
	|	PLUS K_ANTENNAPINPARTIALMETALSIDEAREA NUMBER pin_layer_opt { TUPLE5(STRING("pin_option4"),PLUS,K_ANTENNAPINPARTIALMETALSIDEAREA,NUMBER $3,$4) }
	|	PLUS K_ANTENNAPINGATEAREA NUMBER pin_layer_opt { TUPLE5(STRING("pin_option5"),PLUS,K_ANTENNAPINGATEAREA,NUMBER $3,$4) }
	|	PLUS K_ANTENNAPINDIFFAREA NUMBER pin_layer_opt { TUPLE5(STRING("pin_option6"),PLUS,K_ANTENNAPINDIFFAREA,NUMBER $3,$4) }
	|	PLUS K_ANTENNAPINMAXAREACAR NUMBER K_LAYER /* 31 */ T_STRING { TUPLE6(STRING("pin_option1"),PLUS,K_ANTENNAPINMAXAREACAR,NUMBER $3,K_LAYER,T_STRING) }
	|	PLUS K_ANTENNAPINMAXSIDEAREACAR NUMBER K_LAYER /* 32 */ T_STRING { TUPLE6(STRING("pin_option1"),PLUS,K_ANTENNAPINMAXSIDEAREACAR,NUMBER $3,K_LAYER,T_STRING) }
	|	PLUS K_ANTENNAPINPARTIALCUTAREA NUMBER pin_layer_opt { TUPLE5(STRING("pin_option2"),PLUS,K_ANTENNAPINPARTIALCUTAREA,NUMBER $3,$4) }
	|	PLUS K_ANTENNAPINMAXCUTCAR NUMBER K_LAYER /* 33 */ T_STRING { TUPLE6(STRING("pin_option1"),PLUS,K_ANTENNAPINMAXCUTCAR,NUMBER $3,K_LAYER,T_STRING) }
	|	PLUS K_ANTENNAMODEL pin_oxide { TUPLE4(STRING("pin_option2"),PLUS,K_ANTENNAMODEL,$3) }

pin_layer_mask_opt: /* empty */ { EMPTY_TOKEN }
	|	K_MASK NUMBER { TUPLE3(STRING("pin_layer_mask_opt2"),K_MASK,NUMBER $2) }

pin_via_mask_opt: /* empty */ { EMPTY_TOKEN }
	|	K_MASK NUMBER { TUPLE3(STRING("pin_via_mask_opt2"),K_MASK,NUMBER $2) }

pin_poly_mask_opt: /* empty */ { EMPTY_TOKEN }
	|	K_MASK NUMBER { TUPLE3(STRING("pin_poly_mask_opt2"),K_MASK,NUMBER $2) }

pin_layer_spacing_opt: /* empty */ { EMPTY_TOKEN }
	|	K_SPACING NUMBER { TUPLE3(STRING("pin_layer_spacing_opt2"),K_SPACING,NUMBER $2) }
	|	K_DESIGNRULEWIDTH NUMBER { TUPLE3(STRING("pin_layer_spacing_opt3"),K_DESIGNRULEWIDTH,NUMBER $2) }

pin_poly_spacing_opt: /* empty */ { EMPTY_TOKEN }
	|	K_SPACING NUMBER { TUPLE3(STRING("pin_poly_spacing_opt2"),K_SPACING,NUMBER $2) }
	|	K_DESIGNRULEWIDTH NUMBER { TUPLE3(STRING("pin_poly_spacing_opt3"),K_DESIGNRULEWIDTH,NUMBER $2) }

pin_oxide: K_OXIDE1 { (K_OXIDE1) }
	|	K_OXIDE2 { (K_OXIDE2) }
	|	K_OXIDE3 { (K_OXIDE3) }
	|	K_OXIDE4 { (K_OXIDE4) }

use_type: K_SIGNAL { (K_SIGNAL) }
	|	K_POWER { (K_POWER) }
	|	K_GROUND { (K_GROUND) }
	|	K_CLOCK { (K_CLOCK) }
	|	K_TIEOFF { (K_TIEOFF) }
	|	K_ANALOG { (K_ANALOG) }
	|	K_SCAN { (K_SCAN) }
	|	K_RESET { (K_RESET) }

pin_layer_opt: /* empty */ { EMPTY_TOKEN }
	|	K_LAYER /* 34 */ T_STRING { TUPLE3(STRING("pin_layer_opt1"),K_LAYER,T_STRING) }

end_pins: K_END K_PINS { TUPLE3(STRING("end_pins1"),K_END,K_PINS) }

row_rule: K_ROW /* 35 */ T_STRING T_STRING NUMBER NUMBER orient /* 36 */ row_do_option row_options SEMICOLON { TUPLE10(STRING("row_rule1"),K_ROW,T_STRING,T_STRING,NUMBER $4,NUMBER $5,$6,$7,$8,SEMICOLON) }

row_do_option: /* empty */ { EMPTY_TOKEN }
	|	K_DO NUMBER K_BY NUMBER row_step_option { TUPLE6(STRING("row_do_option2"),K_DO,NUMBER $2,K_BY,NUMBER $4,$5) }

row_step_option: /* empty */ { EMPTY_TOKEN }
	|	K_STEP NUMBER NUMBER { TUPLE4(STRING("row_step_option2"),K_STEP,NUMBER $2,NUMBER $3) }

row_options: /* empty */ { EMPTY_TOKEN }
	|	row_options row_option { TUPLE3(STRING("row_options2"),$1,$2) }

row_option: PLUS K_PROPERTY /* 37 */ row_prop_list { TUPLE4(STRING("row_option1"),PLUS,K_PROPERTY,$3) }

row_prop_list: /* empty */ { EMPTY_TOKEN }
	|	row_prop_list row_prop { CONS2($1,$2) }

row_prop: T_STRING NUMBER { TUPLE3(STRING("row_prop1"),T_STRING,NUMBER $2) }
	|	T_STRING QSTRING { TUPLE3(STRING("row_prop2"),T_STRING,QSTRING $2) }
	|	T_STRING T_STRING { TUPLE3(STRING("row_prop3"),T_STRING,T_STRING) }

tracks_rule: track_start NUMBER /* 38 */ K_DO NUMBER K_STEP NUMBER track_opts SEMICOLON { TUPLE9(STRING("tracks_rule1"),$1,NUMBER $2,K_DO,NUMBER $4,K_STEP,NUMBER $6,$7,SEMICOLON) }

track_start: K_TRACKS track_type { TUPLE3(STRING("track_start1"),K_TRACKS,$2) }

track_type: K_X { (K_X) }
	|	K_Y { (K_Y) }

track_opts: track_mask_statement track_layer_statement { TUPLE3(STRING("track_opts1"),$1,$2) }

track_mask_statement: /* empty */ { EMPTY_TOKEN }
	|	K_MASK NUMBER same_mask { TUPLE4(STRING("track_mask_statement2"),K_MASK,NUMBER $2,$3) }

same_mask: /* empty */ { EMPTY_TOKEN }
	|	K_SAMEMASK { (K_SAMEMASK) }

track_layer_statement: /* empty */ { EMPTY_TOKEN }
	|	K_LAYER /* 39 */ track_layer track_layers { TUPLE4(STRING("track_layer_statement1"),K_LAYER,$2,$3) }

track_layers: /* empty */ { EMPTY_TOKEN }
	|	track_layer track_layers { TUPLE3(STRING("track_layers2"),$1,$2) }

track_layer: T_STRING { (T_STRING) }

gcellgrid: K_GCELLGRID track_type NUMBER K_DO NUMBER K_STEP NUMBER SEMICOLON { TUPLE9(STRING("gcellgrid1"),K_GCELLGRID,$2,NUMBER $3,K_DO,NUMBER $5,K_STEP,NUMBER $7,SEMICOLON) }

extension_section: K_BEGINEXT { (K_BEGINEXT) }

extension_stmt: PLUS K_BEGINEXT { TUPLE3(STRING("extension_stmt1"),PLUS,K_BEGINEXT) }

via_section: via via_declarations via_end { TUPLE4(STRING("via_section1"),$1,$2,$3) }

via: K_VIAS NUMBER SEMICOLON { TUPLE4(STRING("via1"),K_VIAS,NUMBER $2,SEMICOLON) }

via_declarations: /* empty */ { EMPTY_TOKEN }
	|	via_declarations via_declaration { TUPLE3(STRING("via_declarations2"),$1,$2) }

via_declaration: HYPHEN /* 40 */ T_STRING /* 41 */ layer_stmts SEMICOLON { TUPLE5(STRING("via_declaration1"),HYPHEN,T_STRING,$3,SEMICOLON) }

layer_stmts: /* empty */ { EMPTY_TOKEN }
	|	layer_stmts layer_stmt { TUPLE3(STRING("layer_stmts2"),$1,$2) }

layer_stmt: PLUS K_RECT /* 42 */ T_STRING mask pt pt { TUPLE7(STRING("layer_stmt1"),PLUS,K_RECT,T_STRING,$4,$5,$6) }
	|	PLUS K_POLYGON /* 43 */ T_STRING mask /* 44 */ firstPt nextPt nextPt otherPts { TUPLE9(STRING("layer_stmt1"),PLUS,K_POLYGON,T_STRING,$4,$5,$6,$7,$8) }
	|	PLUS K_PATTERNNAME /* 45 */ T_STRING { TUPLE4(STRING("layer_stmt1"),PLUS,K_PATTERNNAME,T_STRING) }
	|	PLUS K_VIARULE /* 46 */ T_STRING PLUS K_CUTSIZE NUMBER NUMBER PLUS K_LAYERS /* 47 */ T_STRING T_STRING T_STRING PLUS K_CUTSPACING NUMBER NUMBER PLUS K_ENCLOSURE NUMBER NUMBER NUMBER NUMBER { TUPLE23(STRING("layer_stmt1"),PLUS,K_VIARULE,T_STRING,PLUS,K_CUTSIZE,NUMBER $6,NUMBER $7,PLUS,K_LAYERS,T_STRING,T_STRING,T_STRING,PLUS,K_CUTSPACING,NUMBER $15,NUMBER $16,PLUS,K_ENCLOSURE,NUMBER $19,NUMBER $20,NUMBER $21,NUMBER $22) }
	|	layer_viarule_opts { ($1) }
	|	extension_stmt { ($1) }

layer_viarule_opts: PLUS K_ROWCOL NUMBER NUMBER { TUPLE5(STRING("layer_viarule_opts1"),PLUS,K_ROWCOL,NUMBER $3,NUMBER $4) }
	|	PLUS K_ORIGIN NUMBER NUMBER { TUPLE5(STRING("layer_viarule_opts2"),PLUS,K_ORIGIN,NUMBER $3,NUMBER $4) }
	|	PLUS K_OFFSET NUMBER NUMBER NUMBER NUMBER { TUPLE7(STRING("layer_viarule_opts3"),PLUS,K_OFFSET,NUMBER $3,NUMBER $4,NUMBER $5,NUMBER $6) }
	|	PLUS K_PATTERN /* 48 */ T_STRING { TUPLE4(STRING("layer_viarule_opts1"),PLUS,K_PATTERN,T_STRING) }

firstPt: pt { ($1) }

nextPt: pt { ($1) }

otherPts: /* empty */ { EMPTY_TOKEN }
	|	otherPts nextPt { TUPLE3(STRING("otherPts2"),$1,$2) }

pt: LPAREN NUMBER NUMBER RPAREN { TUPLE5(STRING("pt1"),LPAREN,NUMBER $2,NUMBER $3,RPAREN) }
	|	LPAREN STAR NUMBER RPAREN { TUPLE5(STRING("pt2"),LPAREN,STAR,NUMBER $3,RPAREN) }
	|	LPAREN NUMBER STAR RPAREN { TUPLE5(STRING("pt3"),LPAREN,NUMBER $2,STAR,RPAREN) }
	|	LPAREN STAR STAR RPAREN { TUPLE5(STRING("pt4"),LPAREN,STAR,STAR,RPAREN) }

mask: /* empty */ { EMPTY_TOKEN }
	|	PLUS K_MASK NUMBER { TUPLE4(STRING("mask2"),PLUS,K_MASK,NUMBER $3) }

via_end: K_END K_VIAS { TUPLE3(STRING("via_end1"),K_END,K_VIAS) }

regions_section: regions_start regions_stmts K_END K_REGIONS { TUPLE5(STRING("regions_section1"),$1,$2,K_END,K_REGIONS) }

regions_start: K_REGIONS NUMBER SEMICOLON { TUPLE4(STRING("regions_start1"),K_REGIONS,NUMBER $2,SEMICOLON) }

regions_stmts: /* empty */ { EMPTY_TOKEN }
	|	regions_stmts regions_stmt { TUPLE3(STRING("regions_stmts2"),$1,$2) }

regions_stmt: HYPHEN /* 49 */ T_STRING /* 50 */ rect_list region_options SEMICOLON { TUPLE6(STRING("regions_stmt1"),HYPHEN,T_STRING,$3,$4,SEMICOLON) }

rect_list: pt pt { CONS2($1,$2) }
	|	rect_list pt pt { CONS3($1,$2,$3) }

region_options: /* empty */ { EMPTY_TOKEN }
	|	region_options region_option { TUPLE3(STRING("region_options2"),$1,$2) }

region_option: PLUS K_PROPERTY /* 51 */ region_prop_list { TUPLE4(STRING("region_option1"),PLUS,K_PROPERTY,$3) }
	|	PLUS K_TYPE region_type { TUPLE4(STRING("region_option2"),PLUS,K_TYPE,$3) }

region_prop_list: /* empty */ { EMPTY_TOKEN }
	|	region_prop_list region_prop { CONS2($1,$2) }

region_prop: T_STRING NUMBER { TUPLE3(STRING("region_prop1"),T_STRING,NUMBER $2) }
	|	T_STRING QSTRING { TUPLE3(STRING("region_prop2"),T_STRING,QSTRING $2) }
	|	T_STRING T_STRING { TUPLE3(STRING("region_prop3"),T_STRING,T_STRING) }

region_type: K_FENCE { (K_FENCE) }
	|	K_GUIDE { (K_GUIDE) }

comps_maskShift_section: K_COMPSMASKSHIFT layer_statement SEMICOLON { TUPLE4(STRING("comps_maskShift_section1"),K_COMPSMASKSHIFT,$2,SEMICOLON) }

comps_section: start_comps comps_rule end_comps { TUPLE4(STRING("comps_section1"),$1,$2,$3) }

start_comps: K_COMPS NUMBER SEMICOLON { TUPLE4(STRING("start_comps1"),K_COMPS,NUMBER $2,SEMICOLON) }

layer_statement: /* empty */ { EMPTY_TOKEN }
	|	layer_statement maskLayer { TUPLE3(STRING("layer_statement2"),$1,$2) }

maskLayer: T_STRING { (T_STRING) }

comps_rule: /* empty */ { EMPTY_TOKEN }
	|	comps_rule comp { TUPLE3(STRING("comps_rule2"),$1,$2) }

comp: comp_start comp_options SEMICOLON { TUPLE4(STRING("comp1"),$1,$2,SEMICOLON) }

comp_start: comp_id_and_name comp_net_list { TUPLE3(STRING("comp_start1"),$1,$2) }

comp_id_and_name: HYPHEN /* 52 */ T_STRING T_STRING { TUPLE4(STRING("comp_id_and_name1"),HYPHEN,T_STRING,T_STRING) }

comp_net_list: /* empty */ { EMPTY_TOKEN }
	|	comp_net_list STAR { CONS2($1,STAR) }
	|	comp_net_list T_STRING { CONS2($1,T_STRING) }

comp_options: /* empty */ { EMPTY_TOKEN }
	|	comp_options comp_option { TUPLE3(STRING("comp_options2"),$1,$2) }

comp_option: comp_generate { ($1) }
	|	comp_source { ($1) }
	|	comp_type { ($1) }
	|	weight { ($1) }
	|	maskShift { ($1) }
	|	comp_foreign { ($1) }
	|	comp_region { ($1) }
	|	comp_eeq { ($1) }
	|	comp_halo { ($1) }
	|	comp_routehalo { ($1) }
	|	comp_property { ($1) }
	|	comp_extension_stmt { ($1) }

comp_extension_stmt: extension_stmt { ($1) }

comp_eeq: PLUS K_EEQMASTER /* 53 */ T_STRING { TUPLE4(STRING("comp_eeq1"),PLUS,K_EEQMASTER,T_STRING) }

comp_generate: PLUS K_COMP_GEN /* 54 */ T_STRING opt_pattern { TUPLE5(STRING("comp_generate1"),PLUS,K_COMP_GEN,T_STRING,$4) }

opt_pattern: /* empty */ { EMPTY_TOKEN }
	|	T_STRING { (T_STRING) }

comp_source: PLUS K_SOURCE source_type { TUPLE4(STRING("comp_source1"),PLUS,K_SOURCE,$3) }

source_type: K_NETLIST { (K_NETLIST) }
	|	K_DIST { (K_DIST) }
	|	K_USER { (K_USER) }
	|	K_TIMING { (K_TIMING) }

comp_region: comp_region_start comp_pnt_list { TUPLE3(STRING("comp_region1"),$1,$2) }
	|	comp_region_start T_STRING { TUPLE3(STRING("comp_region2"),$1,T_STRING) }

comp_pnt_list: pt pt { CONS2($1,$2) }
	|	comp_pnt_list pt pt { CONS3($1,$2,$3) }

comp_halo: PLUS K_HALO /* 55 */ halo_soft NUMBER NUMBER NUMBER NUMBER { TUPLE8(STRING("comp_halo1"),PLUS,K_HALO,$3,NUMBER $4,NUMBER $5,NUMBER $6,NUMBER $7) }

halo_soft: /* empty */ { EMPTY_TOKEN }
	|	K_SOFT { (K_SOFT) }

comp_routehalo: PLUS K_ROUTEHALO NUMBER /* 56 */ T_STRING T_STRING { TUPLE6(STRING("comp_routehalo1"),PLUS,K_ROUTEHALO,NUMBER $3,T_STRING,T_STRING) }

comp_property: PLUS K_PROPERTY /* 57 */ comp_prop_list { TUPLE4(STRING("comp_property1"),PLUS,K_PROPERTY,$3) }

comp_prop_list: comp_prop { CONS1 ($1) }
	|	comp_prop_list comp_prop { CONS2($1,$2) }

comp_prop: T_STRING NUMBER { TUPLE3(STRING("comp_prop1"),T_STRING,NUMBER $2) }
	|	T_STRING QSTRING { TUPLE3(STRING("comp_prop2"),T_STRING,QSTRING $2) }
	|	T_STRING T_STRING { TUPLE3(STRING("comp_prop3"),T_STRING,T_STRING) }

comp_region_start: PLUS K_REGION { TUPLE3(STRING("comp_region_start1"),PLUS,K_REGION) }

comp_foreign: PLUS K_FOREIGN /* 58 */ T_STRING opt_paren orient { TUPLE6(STRING("comp_foreign1"),PLUS,K_FOREIGN,T_STRING,$4,$5) }

opt_paren: pt { ($1) }
	|	NUMBER NUMBER { TUPLE3(STRING("opt_paren2"),NUMBER $1,NUMBER $2) }

comp_type: placement_status pt orient { TUPLE4(STRING("comp_type1"),$1,$2,$3) }
	|	PLUS K_UNPLACED { TUPLE3(STRING("comp_type2"),PLUS,K_UNPLACED) }
	|	PLUS K_UNPLACED pt orient { TUPLE5(STRING("comp_type3"),PLUS,K_UNPLACED,$3,$4) }

maskShift: PLUS K_MASKSHIFT NUMBER { TUPLE4(STRING("maskShift1"),PLUS,K_MASKSHIFT,NUMBER $3) }

placement_status: PLUS K_FIXED { TUPLE3(STRING("placement_status1"),PLUS,K_FIXED) }
	|	PLUS K_COVER { TUPLE3(STRING("placement_status2"),PLUS,K_COVER) }
	|	PLUS K_PLACED { TUPLE3(STRING("placement_status3"),PLUS,K_PLACED) }

weight: PLUS K_WEIGHT NUMBER { TUPLE4(STRING("weight1"),PLUS,K_WEIGHT,NUMBER $3) }

end_comps: K_END K_COMPS { TUPLE3(STRING("end_comps1"),K_END,K_COMPS) }

nets_section: start_nets net_rules end_nets { TUPLE4(STRING("nets_section1"),$1,$2,$3) }

start_nets: K_NETS NUMBER SEMICOLON { TUPLE4(STRING("start_nets1"),K_NETS,NUMBER $2,SEMICOLON) }

net_rules: /* empty */ { EMPTY_TOKEN }
	|	net_rules one_net { TUPLE3(STRING("net_rules2"),$1,$2) }

one_net: net_and_connections net_options SEMICOLON { TUPLE4(STRING("one_net1"),$1,$2,SEMICOLON) }

net_and_connections: net_start { ($1) }

net_start: HYPHEN /* 59 */ net_name { TUPLE3(STRING("net_start1"),HYPHEN,$2) }

net_name: T_STRING /* 60 */ net_connections { TUPLE3(STRING("net_name1"),T_STRING,$2) }
	|	K_MUSTJOIN LPAREN T_STRING /* 61 */ T_STRING RPAREN { TUPLE6(STRING("net_name1"),K_MUSTJOIN,LPAREN,T_STRING,T_STRING,RPAREN) }

net_connections: /* empty */ { EMPTY_TOKEN }
	|	net_connections net_connection { TUPLE3(STRING("net_connections2"),$1,$2) }

net_connection: LPAREN T_STRING /* 62 */ T_STRING conn_opt RPAREN { TUPLE6(STRING("net_connection1"),LPAREN,T_STRING,T_STRING,$4,RPAREN) }
	|	LPAREN STAR /* 63 */ T_STRING conn_opt RPAREN { TUPLE6(STRING("net_connection1"),LPAREN,STAR,T_STRING,$4,RPAREN) }
	|	LPAREN K_PIN /* 64 */ T_STRING conn_opt RPAREN { TUPLE6(STRING("net_connection1"),LPAREN,K_PIN,T_STRING,$4,RPAREN) }

conn_opt: /* empty */ { EMPTY_TOKEN }
	|	extension_stmt { ($1) }
	|	PLUS K_SYNTHESIZED { TUPLE3(STRING("conn_opt3"),PLUS,K_SYNTHESIZED) }

net_options: /* empty */ { EMPTY_TOKEN }
	|	net_options net_option { TUPLE3(STRING("net_options2"),$1,$2) }

net_option: PLUS net_type /* 65 */ paths { TUPLE4(STRING("net_option1"),PLUS,$2,$3) }
	|	PLUS K_SOURCE netsource_type { TUPLE4(STRING("net_option2"),PLUS,K_SOURCE,$3) }
	|	PLUS K_FIXEDBUMP { TUPLE3(STRING("net_option3"),PLUS,K_FIXEDBUMP) }
	|	PLUS K_FREQUENCY /* 66 */ NUMBER { TUPLE4(STRING("net_option1"),PLUS,K_FREQUENCY,NUMBER $3) }
	|	PLUS K_ORIGINAL /* 67 */ T_STRING { TUPLE4(STRING("net_option1"),PLUS,K_ORIGINAL,T_STRING) }
	|	PLUS K_PATTERN pattern_type { TUPLE4(STRING("net_option2"),PLUS,K_PATTERN,$3) }
	|	PLUS K_WEIGHT NUMBER { TUPLE4(STRING("net_option3"),PLUS,K_WEIGHT,NUMBER $3) }
	|	PLUS K_XTALK NUMBER { TUPLE4(STRING("net_option4"),PLUS,K_XTALK,NUMBER $3) }
	|	PLUS K_ESTCAP NUMBER { TUPLE4(STRING("net_option5"),PLUS,K_ESTCAP,NUMBER $3) }
	|	PLUS K_USE use_type { TUPLE4(STRING("net_option6"),PLUS,K_USE,$3) }
	|	PLUS K_STYLE NUMBER { TUPLE4(STRING("net_option7"),PLUS,K_STYLE,NUMBER $3) }
	|	PLUS K_NONDEFAULTRULE /* 68 */ T_STRING { TUPLE4(STRING("net_option1"),PLUS,K_NONDEFAULTRULE,T_STRING) }
	|	vpin_stmt { ($1) }
	|	PLUS K_SHIELDNET /* 69 */ T_STRING { TUPLE4(STRING("net_option1"),PLUS,K_SHIELDNET,T_STRING) }
	|	PLUS K_NOSHIELD /* 70 */ /* 71 */ paths { TUPLE4(STRING("net_option1"),PLUS,K_NOSHIELD,$3) }
	|	PLUS K_SUBNET /* 72 */ T_STRING /* 73 */ comp_names /* 74 */ subnet_options { TUPLE6(STRING("net_option1"),PLUS,K_SUBNET,T_STRING,$4,$5) }
	|	PLUS K_PROPERTY /* 75 */ net_prop_list { TUPLE4(STRING("net_option1"),PLUS,K_PROPERTY,$3) }
	|	extension_stmt { ($1) }

net_prop_list: net_prop { CONS1 ($1) }
	|	net_prop_list net_prop { CONS2($1,$2) }

net_prop: T_STRING NUMBER { TUPLE3(STRING("net_prop1"),T_STRING,NUMBER $2) }
	|	T_STRING QSTRING { TUPLE3(STRING("net_prop2"),T_STRING,QSTRING $2) }
	|	T_STRING T_STRING { TUPLE3(STRING("net_prop3"),T_STRING,T_STRING) }

netsource_type: K_NETLIST { (K_NETLIST) }
	|	K_DIST { (K_DIST) }
	|	K_USER { (K_USER) }
	|	K_TIMING { (K_TIMING) }
	|	K_TEST { (K_TEST) }

vpin_stmt: vpin_begin vpin_layer_opt pt pt /* 76 */ vpin_options { TUPLE6(STRING("vpin_stmt1"),$1,$2,$3,$4,$5) }

vpin_begin: PLUS K_VPIN /* 77 */ T_STRING { TUPLE4(STRING("vpin_begin1"),PLUS,K_VPIN,T_STRING) }

vpin_layer_opt: /* empty */ { EMPTY_TOKEN }
	|	K_LAYER /* 78 */ T_STRING { TUPLE3(STRING("vpin_layer_opt1"),K_LAYER,T_STRING) }

vpin_options: /* empty */ { EMPTY_TOKEN }
	|	vpin_status pt orient { TUPLE4(STRING("vpin_options2"),$1,$2,$3) }

vpin_status: K_PLACED { (K_PLACED) }
	|	K_FIXED { (K_FIXED) }
	|	K_COVER { (K_COVER) }

net_type: K_FIXED { (K_FIXED) }
	|	K_COVER { (K_COVER) }
	|	K_ROUTED { (K_ROUTED) }

paths: path { ($1) }
	|	paths new_path { TUPLE3(STRING("paths2"),$1,$2) }

new_path: K_NEW /* 79 */ path { TUPLE3(STRING("new_path1"),K_NEW,$2) }

path: T_STRING /* 80 */ opt_taper_style_s path_pt /* 81 */ path_item_list { TUPLE5(STRING("path1"),T_STRING,$2,$3,$4) }

virtual_statement: K_VIRTUAL virtual_pt { TUPLE3(STRING("virtual_statement1"),K_VIRTUAL,$2) }

rect_statement: K_RECT rect_pts { TUPLE3(STRING("rect_statement1"),K_RECT,$2) }

path_item_list: /* empty */ { EMPTY_TOKEN }
	|	path_item_list path_item { CONS2($1,$2) }

path_item: T_STRING { (T_STRING) }
	|	K_MASK NUMBER T_STRING { TUPLE4(STRING("path_item2"),K_MASK,NUMBER $2,T_STRING) }
	|	T_STRING orient { TUPLE3(STRING("path_item3"),T_STRING,$2) }
	|	K_MASK NUMBER T_STRING orient { TUPLE5(STRING("path_item4"),K_MASK,NUMBER $2,T_STRING,$4) }
	|	K_MASK NUMBER T_STRING K_DO NUMBER K_BY NUMBER K_STEP NUMBER NUMBER { TUPLE11(STRING("path_item5"),K_MASK,NUMBER $2,T_STRING,K_DO,NUMBER $5,K_BY,NUMBER $7,K_STEP,NUMBER $9,NUMBER $10) }
	|	T_STRING K_DO NUMBER K_BY NUMBER K_STEP NUMBER NUMBER { TUPLE9(STRING("path_item6"),T_STRING,K_DO,NUMBER $3,K_BY,NUMBER $5,K_STEP,NUMBER $7,NUMBER $8) }
	|	T_STRING orient K_DO NUMBER K_BY NUMBER K_STEP NUMBER NUMBER { TUPLE10(STRING("path_item7"),T_STRING,$2,K_DO,NUMBER $4,K_BY,NUMBER $6,K_STEP,NUMBER $8,NUMBER $9) }
	|	K_MASK NUMBER T_STRING orient K_DO NUMBER K_BY NUMBER K_STEP NUMBER NUMBER { TUPLE12(STRING("path_item8"),K_MASK,NUMBER $2,T_STRING,$4,K_DO,NUMBER $6,K_BY,NUMBER $8,K_STEP,NUMBER $10,NUMBER $11) }
	|	virtual_statement { ($1) }
	|	rect_statement { ($1) }
	|	K_MASK NUMBER K_RECT /* 82 */ LPAREN NUMBER NUMBER NUMBER NUMBER RPAREN { TUPLE10(STRING("path_item1"),K_MASK,NUMBER $2,K_RECT,LPAREN,NUMBER $5,NUMBER $6,NUMBER $7,NUMBER $8,RPAREN) }
	|	K_MASK NUMBER /* 83 */ path_pt { TUPLE4(STRING("path_item1"),K_MASK,NUMBER $2,$3) }
	|	path_pt { ($1) }

path_pt: LPAREN NUMBER NUMBER RPAREN { TUPLE5(STRING("path_pt1"),LPAREN,NUMBER $2,NUMBER $3,RPAREN) }
	|	LPAREN STAR NUMBER RPAREN { TUPLE5(STRING("path_pt2"),LPAREN,STAR,NUMBER $3,RPAREN) }
	|	LPAREN NUMBER STAR RPAREN { TUPLE5(STRING("path_pt3"),LPAREN,NUMBER $2,STAR,RPAREN) }
	|	LPAREN STAR STAR RPAREN { TUPLE5(STRING("path_pt4"),LPAREN,STAR,STAR,RPAREN) }
	|	LPAREN NUMBER NUMBER NUMBER RPAREN { TUPLE6(STRING("path_pt5"),LPAREN,NUMBER $2,NUMBER $3,NUMBER $4,RPAREN) }
	|	LPAREN STAR NUMBER NUMBER RPAREN { TUPLE6(STRING("path_pt6"),LPAREN,STAR,NUMBER $3,NUMBER $4,RPAREN) }
	|	LPAREN NUMBER STAR NUMBER RPAREN { TUPLE6(STRING("path_pt7"),LPAREN,NUMBER $2,STAR,NUMBER $4,RPAREN) }
	|	LPAREN STAR STAR NUMBER RPAREN { TUPLE6(STRING("path_pt8"),LPAREN,STAR,STAR,NUMBER $4,RPAREN) }

virtual_pt: LPAREN NUMBER NUMBER RPAREN { TUPLE5(STRING("virtual_pt1"),LPAREN,NUMBER $2,NUMBER $3,RPAREN) }
	|	LPAREN STAR NUMBER RPAREN { TUPLE5(STRING("virtual_pt2"),LPAREN,STAR,NUMBER $3,RPAREN) }
	|	LPAREN NUMBER STAR RPAREN { TUPLE5(STRING("virtual_pt3"),LPAREN,NUMBER $2,STAR,RPAREN) }

rect_pts: LPAREN NUMBER NUMBER NUMBER NUMBER RPAREN { TUPLE7(STRING("rect_pts1"),LPAREN,NUMBER $2,NUMBER $3,NUMBER $4,NUMBER $5,RPAREN) }

opt_taper_style_s: /* empty */ { EMPTY_TOKEN }
	|	opt_taper_style_s opt_taper_style { TUPLE3(STRING("opt_taper_style_s2"),$1,$2) }

opt_taper_style: opt_style { ($1) }
	|	opt_taper { ($1) }

opt_taper: K_TAPER { (K_TAPER) }
	|	K_TAPERRULE /* 84 */ T_STRING { TUPLE3(STRING("opt_taper1"),K_TAPERRULE,T_STRING) }

opt_style: K_STYLE NUMBER { TUPLE3(STRING("opt_style1"),K_STYLE,NUMBER $2) }

opt_spaths: /* empty */ { EMPTY_TOKEN }
	|	opt_spaths opt_shape_style { TUPLE3(STRING("opt_spaths2"),$1,$2) }

opt_shape_style: PLUS K_SHAPE shape_type { TUPLE4(STRING("opt_shape_style1"),PLUS,K_SHAPE,$3) }
	|	PLUS K_STYLE NUMBER { TUPLE4(STRING("opt_shape_style2"),PLUS,K_STYLE,NUMBER $3) }

end_nets: K_END K_NETS { TUPLE3(STRING("end_nets1"),K_END,K_NETS) }

shape_type: K_RING { (K_RING) }
	|	K_STRIPE { (K_STRIPE) }
	|	K_FOLLOWPIN { (K_FOLLOWPIN) }
	|	K_IOWIRE { (K_IOWIRE) }
	|	K_COREWIRE { (K_COREWIRE) }
	|	K_BLOCKWIRE { (K_BLOCKWIRE) }
	|	K_FILLWIRE { (K_FILLWIRE) }
	|	K_FILLWIREOPC { (K_FILLWIREOPC) }
	|	K_DRCFILL { (K_DRCFILL) }
	|	K_BLOCKAGEWIRE { (K_BLOCKAGEWIRE) }
	|	K_PADRING { (K_PADRING) }
	|	K_BLOCKRING { (K_BLOCKRING) }

snets_section: start_snets snet_rules end_snets { TUPLE4(STRING("snets_section1"),$1,$2,$3) }

snet_rules: /* empty */ { EMPTY_TOKEN }
	|	snet_rules snet_rule { TUPLE3(STRING("snet_rules2"),$1,$2) }

snet_rule: net_and_connections snet_options SEMICOLON { TUPLE4(STRING("snet_rule1"),$1,$2,SEMICOLON) }

snet_options: /* empty */ { EMPTY_TOKEN }
	|	snet_options snet_option { TUPLE3(STRING("snet_options2"),$1,$2) }

snet_option: snet_width { ($1) }
	|	snet_voltage { ($1) }
	|	snet_spacing { ($1) }
	|	snet_other_option { ($1) }

snet_other_option: PLUS net_type { TUPLE3(STRING("snet_other_option1"),PLUS,$2) }
	|	PLUS net_type /* 85 */ spaths { TUPLE4(STRING("snet_other_option1"),PLUS,$2,$3) }
	|	PLUS K_SHIELD /* 86 */ T_STRING /* 87 */ shield_layer { TUPLE5(STRING("snet_other_option1"),PLUS,K_SHIELD,T_STRING,$4) }
	|	PLUS K_SHAPE shape_type { TUPLE4(STRING("snet_other_option2"),PLUS,K_SHAPE,$3) }
	|	PLUS K_MASK NUMBER { TUPLE4(STRING("snet_other_option3"),PLUS,K_MASK,NUMBER $3) }
	|	PLUS K_POLYGON /* 88 */ T_STRING /* 89 */ firstPt nextPt nextPt otherPts { TUPLE8(STRING("snet_other_option1"),PLUS,K_POLYGON,T_STRING,$4,$5,$6,$7) }
	|	PLUS K_RECT /* 90 */ T_STRING pt pt { TUPLE6(STRING("snet_other_option1"),PLUS,K_RECT,T_STRING,$4,$5) }
	|	PLUS K_VIA /* 91 */ T_STRING orient_pt /* 92 */ firstPt otherPts { TUPLE7(STRING("snet_other_option1"),PLUS,K_VIA,T_STRING,$4,$5,$6) }
	|	PLUS K_SOURCE source_type { TUPLE4(STRING("snet_other_option2"),PLUS,K_SOURCE,$3) }
	|	PLUS K_FIXEDBUMP { TUPLE3(STRING("snet_other_option3"),PLUS,K_FIXEDBUMP) }
	|	PLUS K_FREQUENCY NUMBER { TUPLE4(STRING("snet_other_option4"),PLUS,K_FREQUENCY,NUMBER $3) }
	|	PLUS K_ORIGINAL /* 93 */ T_STRING { TUPLE4(STRING("snet_other_option1"),PLUS,K_ORIGINAL,T_STRING) }
	|	PLUS K_PATTERN pattern_type { TUPLE4(STRING("snet_other_option2"),PLUS,K_PATTERN,$3) }
	|	PLUS K_WEIGHT NUMBER { TUPLE4(STRING("snet_other_option3"),PLUS,K_WEIGHT,NUMBER $3) }
	|	PLUS K_ESTCAP NUMBER { TUPLE4(STRING("snet_other_option4"),PLUS,K_ESTCAP,NUMBER $3) }
	|	PLUS K_USE use_type { TUPLE4(STRING("snet_other_option5"),PLUS,K_USE,$3) }
	|	PLUS K_STYLE NUMBER { TUPLE4(STRING("snet_other_option6"),PLUS,K_STYLE,NUMBER $3) }
	|	PLUS K_PROPERTY /* 94 */ snet_prop_list { TUPLE4(STRING("snet_other_option1"),PLUS,K_PROPERTY,$3) }
	|	extension_stmt { ($1) }

orient_pt: /* empty */ { EMPTY_TOKEN }
	|	K_N { (K_N) }
	|	K_W { (K_W) }
	|	K_S { (K_S) }
	|	K_E { (K_E) }
	|	K_FN { (K_FN) }
	|	K_FW { (K_FW) }
	|	K_FS { (K_FS) }
	|	K_FE { (K_FE) }

shield_layer: /* empty */ { EMPTY_TOKEN }
	|	/* 95 */ spaths { ($1) }

snet_width: PLUS K_WIDTH /* 96 */ T_STRING NUMBER { TUPLE5(STRING("snet_width1"),PLUS,K_WIDTH,T_STRING,NUMBER $4) }

snet_voltage: PLUS K_VOLTAGE /* 97 */ T_STRING { TUPLE4(STRING("snet_voltage1"),PLUS,K_VOLTAGE,T_STRING) }

snet_spacing: PLUS K_SPACING /* 98 */ T_STRING NUMBER /* 99 */ opt_snet_range { TUPLE6(STRING("snet_spacing1"),PLUS,K_SPACING,T_STRING,NUMBER $4,$5) }

snet_prop_list: snet_prop { CONS1 ($1) }
	|	snet_prop_list snet_prop { CONS2($1,$2) }

snet_prop: T_STRING NUMBER { TUPLE3(STRING("snet_prop1"),T_STRING,NUMBER $2) }
	|	T_STRING QSTRING { TUPLE3(STRING("snet_prop2"),T_STRING,QSTRING $2) }
	|	T_STRING T_STRING { TUPLE3(STRING("snet_prop3"),T_STRING,T_STRING) }

opt_snet_range: /* empty */ { EMPTY_TOKEN }
	|	K_RANGE NUMBER NUMBER { TUPLE4(STRING("opt_snet_range2"),K_RANGE,NUMBER $2,NUMBER $3) }

opt_range: /* empty */ { EMPTY_TOKEN }
	|	K_RANGE NUMBER NUMBER { TUPLE4(STRING("opt_range2"),K_RANGE,NUMBER $2,NUMBER $3) }

pattern_type: K_BALANCED { (K_BALANCED) }
	|	K_STEINER { (K_STEINER) }
	|	K_TRUNK { (K_TRUNK) }
	|	K_WIREDLOGIC { (K_WIREDLOGIC) }

spaths: spath { ($1) }
	|	spaths snew_path { TUPLE3(STRING("spaths2"),$1,$2) }

snew_path: K_NEW /* 100 */ spath { TUPLE3(STRING("snew_path1"),K_NEW,$2) }

spath: T_STRING /* 101 */ width opt_spaths path_pt /* 102 */ path_item_list { TUPLE6(STRING("spath1"),T_STRING,$2,$3,$4,$5) }

width: NUMBER { (NUMBER $1) }

start_snets: K_SNETS NUMBER SEMICOLON { TUPLE4(STRING("start_snets1"),K_SNETS,NUMBER $2,SEMICOLON) }

end_snets: K_END K_SNETS { TUPLE3(STRING("end_snets1"),K_END,K_SNETS) }

groups_section: groups_start group_rules groups_end { TUPLE4(STRING("groups_section1"),$1,$2,$3) }

groups_start: K_GROUPS NUMBER SEMICOLON { TUPLE4(STRING("groups_start1"),K_GROUPS,NUMBER $2,SEMICOLON) }

group_rules: /* empty */ { EMPTY_TOKEN }
	|	group_rules group_rule { TUPLE3(STRING("group_rules2"),$1,$2) }

group_rule: start_group group_members group_options SEMICOLON { TUPLE5(STRING("group_rule1"),$1,$2,$3,SEMICOLON) }

start_group: HYPHEN /* 103 */ T_STRING { TUPLE3(STRING("start_group1"),HYPHEN,T_STRING) }

group_members: /* empty */ { EMPTY_TOKEN }
	|	group_members group_member { TUPLE3(STRING("group_members2"),$1,$2) }

group_member: T_STRING { (T_STRING) }

group_options: /* empty */ { EMPTY_TOKEN }
	|	group_options group_option { TUPLE3(STRING("group_options2"),$1,$2) }

group_option: PLUS K_SOFT group_soft_options { TUPLE4(STRING("group_option1"),PLUS,K_SOFT,$3) }
	|	PLUS K_PROPERTY /* 104 */ group_prop_list { TUPLE4(STRING("group_option1"),PLUS,K_PROPERTY,$3) }
	|	PLUS K_REGION /* 105 */ group_region { TUPLE4(STRING("group_option1"),PLUS,K_REGION,$3) }
	|	extension_stmt { ($1) }

group_region: pt pt { TUPLE3(STRING("group_region1"),$1,$2) }
	|	T_STRING { (T_STRING) }

group_prop_list: /* empty */ { EMPTY_TOKEN }
	|	group_prop_list group_prop { CONS2($1,$2) }

group_prop: T_STRING NUMBER { TUPLE3(STRING("group_prop1"),T_STRING,NUMBER $2) }
	|	T_STRING QSTRING { TUPLE3(STRING("group_prop2"),T_STRING,QSTRING $2) }
	|	T_STRING T_STRING { TUPLE3(STRING("group_prop3"),T_STRING,T_STRING) }

group_soft_options: /* empty */ { EMPTY_TOKEN }
	|	group_soft_options group_soft_option { TUPLE3(STRING("group_soft_options2"),$1,$2) }

group_soft_option: K_MAXX NUMBER { TUPLE3(STRING("group_soft_option1"),K_MAXX,NUMBER $2) }
	|	K_MAXY NUMBER { TUPLE3(STRING("group_soft_option2"),K_MAXY,NUMBER $2) }
	|	K_MAXHALFPERIMETER NUMBER { TUPLE3(STRING("group_soft_option3"),K_MAXHALFPERIMETER,NUMBER $2) }

groups_end: K_END K_GROUPS { TUPLE3(STRING("groups_end1"),K_END,K_GROUPS) }

assertions_section: assertions_start constraint_rules assertions_end { TUPLE4(STRING("assertions_section1"),$1,$2,$3) }

constraint_section: constraints_start constraint_rules constraints_end { TUPLE4(STRING("constraint_section1"),$1,$2,$3) }

assertions_start: K_ASSERTIONS NUMBER SEMICOLON { TUPLE4(STRING("assertions_start1"),K_ASSERTIONS,NUMBER $2,SEMICOLON) }

constraints_start: K_CONSTRAINTS NUMBER SEMICOLON { TUPLE4(STRING("constraints_start1"),K_CONSTRAINTS,NUMBER $2,SEMICOLON) }

constraint_rules: /* empty */ { EMPTY_TOKEN }
	|	constraint_rules constraint_rule { TUPLE3(STRING("constraint_rules2"),$1,$2) }

constraint_rule: operand_rule { ($1) }
	|	wiredlogic_rule { ($1) }

operand_rule: HYPHEN operand delay_specs SEMICOLON { TUPLE5(STRING("operand_rule1"),HYPHEN,$2,$3,SEMICOLON) }

operand: K_NET /* 106 */ T_STRING { TUPLE3(STRING("operand1"),K_NET,T_STRING) }
	|	K_PATH /* 107 */ T_STRING T_STRING T_STRING T_STRING { TUPLE6(STRING("operand1"),K_PATH,T_STRING,T_STRING,T_STRING,T_STRING) }
	|	K_SUM LPAREN operand_list RPAREN { TUPLE5(STRING("operand2"),K_SUM,LPAREN,$3,RPAREN) }
	|	K_DIFF LPAREN operand_list RPAREN { TUPLE5(STRING("operand3"),K_DIFF,LPAREN,$3,RPAREN) }

operand_list: operand { CONS1 ($1) }
	|	operand_list operand { CONS2($1,$2) }
	|	operand_list COMMA operand { CONS3($1,COMMA,$3) }

wiredlogic_rule: HYPHEN K_WIREDLOGIC /* 108 */ T_STRING opt_plus K_MAXDIST NUMBER SEMICOLON { TUPLE8(STRING("wiredlogic_rule1"),HYPHEN,K_WIREDLOGIC,T_STRING,$4,K_MAXDIST,NUMBER $6,SEMICOLON) }

opt_plus: /* empty */ { EMPTY_TOKEN }
	|	PLUS { (PLUS) }

delay_specs: /* empty */ { EMPTY_TOKEN }
	|	delay_specs delay_spec { TUPLE3(STRING("delay_specs2"),$1,$2) }

delay_spec: PLUS K_RISEMIN NUMBER { TUPLE4(STRING("delay_spec1"),PLUS,K_RISEMIN,NUMBER $3) }
	|	PLUS K_RISEMAX NUMBER { TUPLE4(STRING("delay_spec2"),PLUS,K_RISEMAX,NUMBER $3) }
	|	PLUS K_FALLMIN NUMBER { TUPLE4(STRING("delay_spec3"),PLUS,K_FALLMIN,NUMBER $3) }
	|	PLUS K_FALLMAX NUMBER { TUPLE4(STRING("delay_spec4"),PLUS,K_FALLMAX,NUMBER $3) }

constraints_end: K_END K_CONSTRAINTS { TUPLE3(STRING("constraints_end1"),K_END,K_CONSTRAINTS) }

assertions_end: K_END K_ASSERTIONS { TUPLE3(STRING("assertions_end1"),K_END,K_ASSERTIONS) }

scanchains_section: scanchain_start scanchain_rules scanchain_end { TUPLE4(STRING("scanchains_section1"),$1,$2,$3) }

scanchain_start: K_SCANCHAINS NUMBER SEMICOLON { TUPLE4(STRING("scanchain_start1"),K_SCANCHAINS,NUMBER $2,SEMICOLON) }

scanchain_rules: /* empty */ { EMPTY_TOKEN }
	|	scanchain_rules scan_rule { TUPLE3(STRING("scanchain_rules2"),$1,$2) }

scan_rule: start_scan scan_members SEMICOLON { TUPLE4(STRING("scan_rule1"),$1,$2,SEMICOLON) }

start_scan: HYPHEN /* 109 */ T_STRING { TUPLE3(STRING("start_scan1"),HYPHEN,T_STRING) }

scan_members: /* empty */ { EMPTY_TOKEN }
	|	scan_members scan_member { TUPLE3(STRING("scan_members2"),$1,$2) }

opt_pin: /* empty */ { EMPTY_TOKEN }
	|	T_STRING { (T_STRING) }

scan_member: PLUS K_START /* 110 */ T_STRING opt_pin { TUPLE5(STRING("scan_member1"),PLUS,K_START,T_STRING,$4) }
	|	PLUS K_FLOATING /* 111 */ floating_inst_list { TUPLE4(STRING("scan_member1"),PLUS,K_FLOATING,$3) }
	|	PLUS K_ORDERED /* 112 */ ordered_inst_list { TUPLE4(STRING("scan_member1"),PLUS,K_ORDERED,$3) }
	|	PLUS K_STOP /* 113 */ T_STRING opt_pin { TUPLE5(STRING("scan_member1"),PLUS,K_STOP,T_STRING,$4) }
	|	PLUS K_COMMONSCANPINS /* 114 */ opt_common_pins { TUPLE4(STRING("scan_member1"),PLUS,K_COMMONSCANPINS,$3) }
	|	PLUS K_PARTITION /* 115 */ T_STRING partition_maxbits { TUPLE5(STRING("scan_member1"),PLUS,K_PARTITION,T_STRING,$4) }
	|	extension_stmt { ($1) }

opt_common_pins: /* empty */ { EMPTY_TOKEN }
	|	LPAREN T_STRING T_STRING RPAREN { TUPLE5(STRING("opt_common_pins2"),LPAREN,T_STRING,T_STRING,RPAREN) }
	|	LPAREN T_STRING T_STRING RPAREN LPAREN T_STRING T_STRING RPAREN { TUPLE9(STRING("opt_common_pins3"),LPAREN,T_STRING,T_STRING,RPAREN,LPAREN,T_STRING,T_STRING,RPAREN) }

floating_inst_list: /* empty */ { EMPTY_TOKEN }
	|	floating_inst_list one_floating_inst { CONS2($1,$2) }

one_floating_inst: T_STRING /* 116 */ floating_pins { TUPLE3(STRING("one_floating_inst1"),T_STRING,$2) }

floating_pins: /* empty */ { EMPTY_TOKEN }
	|	LPAREN T_STRING T_STRING RPAREN { TUPLE5(STRING("floating_pins2"),LPAREN,T_STRING,T_STRING,RPAREN) }
	|	LPAREN T_STRING T_STRING RPAREN LPAREN T_STRING T_STRING RPAREN { TUPLE9(STRING("floating_pins3"),LPAREN,T_STRING,T_STRING,RPAREN,LPAREN,T_STRING,T_STRING,RPAREN) }
	|	LPAREN T_STRING T_STRING RPAREN LPAREN T_STRING T_STRING RPAREN LPAREN T_STRING T_STRING RPAREN { TUPLE13(STRING("floating_pins4"),LPAREN,T_STRING,T_STRING,RPAREN,LPAREN,T_STRING,T_STRING,RPAREN,LPAREN,T_STRING,T_STRING,RPAREN) }

ordered_inst_list: /* empty */ { EMPTY_TOKEN }
	|	ordered_inst_list one_ordered_inst { CONS2($1,$2) }

one_ordered_inst: T_STRING /* 117 */ ordered_pins { TUPLE3(STRING("one_ordered_inst1"),T_STRING,$2) }

ordered_pins: /* empty */ { EMPTY_TOKEN }
	|	LPAREN T_STRING T_STRING RPAREN { TUPLE5(STRING("ordered_pins2"),LPAREN,T_STRING,T_STRING,RPAREN) }
	|	LPAREN T_STRING T_STRING RPAREN LPAREN T_STRING T_STRING RPAREN { TUPLE9(STRING("ordered_pins3"),LPAREN,T_STRING,T_STRING,RPAREN,LPAREN,T_STRING,T_STRING,RPAREN) }
	|	LPAREN T_STRING T_STRING RPAREN LPAREN T_STRING T_STRING RPAREN LPAREN T_STRING T_STRING RPAREN { TUPLE13(STRING("ordered_pins4"),LPAREN,T_STRING,T_STRING,RPAREN,LPAREN,T_STRING,T_STRING,RPAREN,LPAREN,T_STRING,T_STRING,RPAREN) }

partition_maxbits: /* empty */ { EMPTY_TOKEN }
	|	K_MAXBITS NUMBER { TUPLE3(STRING("partition_maxbits2"),K_MAXBITS,NUMBER $2) }

scanchain_end: K_END K_SCANCHAINS { TUPLE3(STRING("scanchain_end1"),K_END,K_SCANCHAINS) }

iotiming_section: iotiming_start iotiming_rules iotiming_end { TUPLE4(STRING("iotiming_section1"),$1,$2,$3) }

iotiming_start: K_IOTIMINGS NUMBER SEMICOLON { TUPLE4(STRING("iotiming_start1"),K_IOTIMINGS,NUMBER $2,SEMICOLON) }

iotiming_rules: /* empty */ { EMPTY_TOKEN }
	|	iotiming_rules iotiming_rule { TUPLE3(STRING("iotiming_rules2"),$1,$2) }

iotiming_rule: start_iotiming iotiming_members SEMICOLON { TUPLE4(STRING("iotiming_rule1"),$1,$2,SEMICOLON) }

start_iotiming: HYPHEN LPAREN /* 118 */ T_STRING T_STRING RPAREN { TUPLE6(STRING("start_iotiming1"),HYPHEN,LPAREN,T_STRING,T_STRING,RPAREN) }

iotiming_members: /* empty */ { EMPTY_TOKEN }
	|	iotiming_members iotiming_member { TUPLE3(STRING("iotiming_members2"),$1,$2) }

iotiming_member: PLUS risefall K_VARIABLE NUMBER NUMBER { TUPLE6(STRING("iotiming_member1"),PLUS,$2,K_VARIABLE,NUMBER $4,NUMBER $5) }
	|	PLUS risefall K_SLEWRATE NUMBER NUMBER { TUPLE6(STRING("iotiming_member2"),PLUS,$2,K_SLEWRATE,NUMBER $4,NUMBER $5) }
	|	PLUS K_CAPACITANCE NUMBER { TUPLE4(STRING("iotiming_member3"),PLUS,K_CAPACITANCE,NUMBER $3) }
	|	PLUS K_DRIVECELL /* 119 */ T_STRING /* 120 */ iotiming_drivecell_opt { TUPLE5(STRING("iotiming_member1"),PLUS,K_DRIVECELL,T_STRING,$4) }
	|	extension_stmt { ($1) }

iotiming_drivecell_opt: iotiming_frompin K_TOPIN /* 121 */ T_STRING /* 122 */ iotiming_parallel { TUPLE5(STRING("iotiming_drivecell_opt1"),$1,K_TOPIN,T_STRING,$4) }

iotiming_frompin: /* empty */ { EMPTY_TOKEN }
	|	K_FROMPIN /* 123 */ T_STRING { TUPLE3(STRING("iotiming_frompin1"),K_FROMPIN,T_STRING) }

iotiming_parallel: /* empty */ { EMPTY_TOKEN }
	|	K_PARALLEL NUMBER { TUPLE3(STRING("iotiming_parallel2"),K_PARALLEL,NUMBER $2) }

risefall: K_RISE { (K_RISE) }
	|	K_FALL { (K_FALL) }

iotiming_end: K_END K_IOTIMINGS { TUPLE3(STRING("iotiming_end1"),K_END,K_IOTIMINGS) }

floorplan_contraints_section: fp_start fp_stmts K_END K_FPC { TUPLE5(STRING("floorplan_contraints_section1"),$1,$2,K_END,K_FPC) }

fp_start: K_FPC NUMBER SEMICOLON { TUPLE4(STRING("fp_start1"),K_FPC,NUMBER $2,SEMICOLON) }

fp_stmts: /* empty */ { EMPTY_TOKEN }
	|	fp_stmts fp_stmt { TUPLE3(STRING("fp_stmts2"),$1,$2) }

fp_stmt: HYPHEN /* 124 */ T_STRING h_or_v /* 125 */ constraint_type constrain_what_list SEMICOLON { TUPLE7(STRING("fp_stmt1"),HYPHEN,T_STRING,$3,$4,$5,SEMICOLON) }

h_or_v: K_HORIZONTAL { (K_HORIZONTAL) }
	|	K_VERTICAL { (K_VERTICAL) }

constraint_type: K_ALIGN { (K_ALIGN) }
	|	K_MAX NUMBER { TUPLE3(STRING("constraint_type2"),K_MAX,NUMBER $2) }
	|	K_MIN NUMBER { TUPLE3(STRING("constraint_type3"),K_MIN,NUMBER $2) }
	|	K_EQUAL NUMBER { TUPLE3(STRING("constraint_type4"),K_EQUAL,NUMBER $2) }

constrain_what_list: /* empty */ { EMPTY_TOKEN }
	|	constrain_what_list constrain_what { CONS2($1,$2) }

constrain_what: PLUS K_BOTTOMLEFT /* 126 */ row_or_comp_list { TUPLE4(STRING("constrain_what1"),PLUS,K_BOTTOMLEFT,$3) }
	|	PLUS K_TOPRIGHT /* 127 */ row_or_comp_list { TUPLE4(STRING("constrain_what1"),PLUS,K_TOPRIGHT,$3) }

row_or_comp_list: /* empty */ { EMPTY_TOKEN }
	|	row_or_comp_list row_or_comp { CONS2($1,$2) }

row_or_comp: LPAREN K_ROWS /* 128 */ T_STRING RPAREN { TUPLE5(STRING("row_or_comp1"),LPAREN,K_ROWS,T_STRING,RPAREN) }
	|	LPAREN K_COMPS /* 129 */ T_STRING RPAREN { TUPLE5(STRING("row_or_comp1"),LPAREN,K_COMPS,T_STRING,RPAREN) }

timingdisables_section: timingdisables_start timingdisables_rules timingdisables_end { TUPLE4(STRING("timingdisables_section1"),$1,$2,$3) }

timingdisables_start: K_TIMINGDISABLES NUMBER SEMICOLON { TUPLE4(STRING("timingdisables_start1"),K_TIMINGDISABLES,NUMBER $2,SEMICOLON) }

timingdisables_rules: /* empty */ { EMPTY_TOKEN }
	|	timingdisables_rules timingdisables_rule { TUPLE3(STRING("timingdisables_rules2"),$1,$2) }

timingdisables_rule: HYPHEN K_FROMPIN /* 130 */ T_STRING T_STRING K_TOPIN /* 131 */ T_STRING T_STRING SEMICOLON { TUPLE9(STRING("timingdisables_rule1"),HYPHEN,K_FROMPIN,T_STRING,T_STRING,K_TOPIN,T_STRING,T_STRING,SEMICOLON) }
	|	HYPHEN K_THRUPIN /* 132 */ T_STRING T_STRING SEMICOLON { TUPLE6(STRING("timingdisables_rule1"),HYPHEN,K_THRUPIN,T_STRING,T_STRING,SEMICOLON) }
	|	HYPHEN K_MACRO /* 133 */ T_STRING td_macro_option SEMICOLON { TUPLE6(STRING("timingdisables_rule1"),HYPHEN,K_MACRO,T_STRING,$4,SEMICOLON) }
	|	HYPHEN K_REENTRANTPATHS SEMICOLON { TUPLE4(STRING("timingdisables_rule2"),HYPHEN,K_REENTRANTPATHS,SEMICOLON) }

td_macro_option: K_FROMPIN /* 134 */ T_STRING K_TOPIN /* 135 */ T_STRING { TUPLE5(STRING("td_macro_option1"),K_FROMPIN,T_STRING,K_TOPIN,T_STRING) }
	|	K_THRUPIN /* 136 */ T_STRING { TUPLE3(STRING("td_macro_option1"),K_THRUPIN,T_STRING) }

timingdisables_end: K_END K_TIMINGDISABLES { TUPLE3(STRING("timingdisables_end1"),K_END,K_TIMINGDISABLES) }

partitions_section: partitions_start partition_rules partitions_end { TUPLE4(STRING("partitions_section1"),$1,$2,$3) }

partitions_start: K_PARTITIONS NUMBER SEMICOLON { TUPLE4(STRING("partitions_start1"),K_PARTITIONS,NUMBER $2,SEMICOLON) }

partition_rules: /* empty */ { EMPTY_TOKEN }
	|	partition_rules partition_rule { TUPLE3(STRING("partition_rules2"),$1,$2) }

partition_rule: start_partition partition_members SEMICOLON { TUPLE4(STRING("partition_rule1"),$1,$2,SEMICOLON) }

start_partition: HYPHEN /* 137 */ T_STRING turnoff { TUPLE4(STRING("start_partition1"),HYPHEN,T_STRING,$3) }

turnoff: /* empty */ { EMPTY_TOKEN }
	|	K_TURNOFF turnoff_setup turnoff_hold { TUPLE4(STRING("turnoff2"),K_TURNOFF,$2,$3) }

turnoff_setup: /* empty */ { EMPTY_TOKEN }
	|	K_SETUPRISE { (K_SETUPRISE) }
	|	K_SETUPFALL { (K_SETUPFALL) }

turnoff_hold: /* empty */ { EMPTY_TOKEN }
	|	K_HOLDRISE { (K_HOLDRISE) }
	|	K_HOLDFALL { (K_HOLDFALL) }

partition_members: /* empty */ { EMPTY_TOKEN }
	|	partition_members partition_member { TUPLE3(STRING("partition_members2"),$1,$2) }

partition_member: PLUS K_FROMCLOCKPIN /* 138 */ T_STRING T_STRING risefall minmaxpins { TUPLE7(STRING("partition_member1"),PLUS,K_FROMCLOCKPIN,T_STRING,T_STRING,$5,$6) }
	|	PLUS K_FROMCOMPPIN /* 139 */ T_STRING T_STRING risefallminmax2_list { TUPLE6(STRING("partition_member1"),PLUS,K_FROMCOMPPIN,T_STRING,T_STRING,$5) }
	|	PLUS K_FROMIOPIN /* 140 */ T_STRING risefallminmax1_list { TUPLE5(STRING("partition_member1"),PLUS,K_FROMIOPIN,T_STRING,$4) }
	|	PLUS K_TOCLOCKPIN /* 141 */ T_STRING T_STRING risefall minmaxpins { TUPLE7(STRING("partition_member1"),PLUS,K_TOCLOCKPIN,T_STRING,T_STRING,$5,$6) }
	|	PLUS K_TOCOMPPIN /* 142 */ T_STRING T_STRING risefallminmax2_list { TUPLE6(STRING("partition_member1"),PLUS,K_TOCOMPPIN,T_STRING,T_STRING,$5) }
	|	PLUS K_TOIOPIN /* 143 */ T_STRING risefallminmax1_list { TUPLE5(STRING("partition_member1"),PLUS,K_TOIOPIN,T_STRING,$4) }
	|	extension_stmt { ($1) }

minmaxpins: min_or_max_list K_PINS /* 144 */ pin_list { TUPLE4(STRING("minmaxpins1"),$1,K_PINS,$3) }

min_or_max_list: /* empty */ { EMPTY_TOKEN }
	|	min_or_max_list min_or_max_member { CONS2($1,$2) }

min_or_max_member: K_MIN NUMBER NUMBER { TUPLE4(STRING("min_or_max_member1"),K_MIN,NUMBER $2,NUMBER $3) }
	|	K_MAX NUMBER NUMBER { TUPLE4(STRING("min_or_max_member2"),K_MAX,NUMBER $2,NUMBER $3) }

pin_list: /* empty */ { EMPTY_TOKEN }
	|	pin_list T_STRING { CONS2($1,T_STRING) }

risefallminmax1_list: /* empty */ { EMPTY_TOKEN }
	|	risefallminmax1_list risefallminmax1 { CONS2($1,$2) }

risefallminmax1: K_RISEMIN NUMBER { TUPLE3(STRING("risefallminmax11"),K_RISEMIN,NUMBER $2) }
	|	K_FALLMIN NUMBER { TUPLE3(STRING("risefallminmax12"),K_FALLMIN,NUMBER $2) }
	|	K_RISEMAX NUMBER { TUPLE3(STRING("risefallminmax13"),K_RISEMAX,NUMBER $2) }
	|	K_FALLMAX NUMBER { TUPLE3(STRING("risefallminmax14"),K_FALLMAX,NUMBER $2) }

risefallminmax2_list: risefallminmax2 { CONS1 ($1) }
	|	risefallminmax2_list risefallminmax2 { CONS2($1,$2) }

risefallminmax2: K_RISEMIN NUMBER NUMBER { TUPLE4(STRING("risefallminmax21"),K_RISEMIN,NUMBER $2,NUMBER $3) }
	|	K_FALLMIN NUMBER NUMBER { TUPLE4(STRING("risefallminmax22"),K_FALLMIN,NUMBER $2,NUMBER $3) }
	|	K_RISEMAX NUMBER NUMBER { TUPLE4(STRING("risefallminmax23"),K_RISEMAX,NUMBER $2,NUMBER $3) }
	|	K_FALLMAX NUMBER NUMBER { TUPLE4(STRING("risefallminmax24"),K_FALLMAX,NUMBER $2,NUMBER $3) }

partitions_end: K_END K_PARTITIONS { TUPLE3(STRING("partitions_end1"),K_END,K_PARTITIONS) }

comp_names: /* empty */ { EMPTY_TOKEN }
	|	comp_names comp_name { TUPLE3(STRING("comp_names2"),$1,$2) }

comp_name: LPAREN /* 145 */ T_STRING T_STRING subnet_opt_syn RPAREN { TUPLE6(STRING("comp_name1"),LPAREN,T_STRING,T_STRING,$4,RPAREN) }

subnet_opt_syn: /* empty */ { EMPTY_TOKEN }
	|	PLUS K_SYNTHESIZED { TUPLE3(STRING("subnet_opt_syn2"),PLUS,K_SYNTHESIZED) }

subnet_options: /* empty */ { EMPTY_TOKEN }
	|	subnet_options subnet_option { TUPLE3(STRING("subnet_options2"),$1,$2) }

subnet_option: subnet_type /* 146 */ paths { TUPLE3(STRING("subnet_option1"),$1,$2) }
	|	K_NONDEFAULTRULE /* 147 */ T_STRING { TUPLE3(STRING("subnet_option1"),K_NONDEFAULTRULE,T_STRING) }

subnet_type: K_FIXED { (K_FIXED) }
	|	K_COVER { (K_COVER) }
	|	K_ROUTED { (K_ROUTED) }
	|	K_NOSHIELD { (K_NOSHIELD) }

pin_props_section: begin_pin_props pin_prop_list end_pin_props { TUPLE4(STRING("pin_props_section1"),$1,$2,$3) }

begin_pin_props: K_PINPROPERTIES NUMBER opt_semi { TUPLE4(STRING("begin_pin_props1"),K_PINPROPERTIES,NUMBER $2,$3) }

opt_semi: /* empty */ { EMPTY_TOKEN }
	|	SEMICOLON { (SEMICOLON) }

end_pin_props: K_END K_PINPROPERTIES { TUPLE3(STRING("end_pin_props1"),K_END,K_PINPROPERTIES) }

pin_prop_list: /* empty */ { EMPTY_TOKEN }
	|	pin_prop_list pin_prop_terminal { CONS2($1,$2) }

pin_prop_terminal: HYPHEN /* 148 */ T_STRING T_STRING /* 149 */ pin_prop_options SEMICOLON { TUPLE6(STRING("pin_prop_terminal1"),HYPHEN,T_STRING,T_STRING,$4,SEMICOLON) }

pin_prop_options: /* empty */ { EMPTY_TOKEN }
	|	pin_prop_options pin_prop { TUPLE3(STRING("pin_prop_options2"),$1,$2) }

pin_prop: PLUS K_PROPERTY /* 150 */ pin_prop_name_value_list { TUPLE4(STRING("pin_prop1"),PLUS,K_PROPERTY,$3) }

pin_prop_name_value_list: /* empty */ { EMPTY_TOKEN }
	|	pin_prop_name_value_list pin_prop_name_value { CONS2($1,$2) }

pin_prop_name_value: T_STRING NUMBER { TUPLE3(STRING("pin_prop_name_value1"),T_STRING,NUMBER $2) }
	|	T_STRING QSTRING { TUPLE3(STRING("pin_prop_name_value2"),T_STRING,QSTRING $2) }
	|	T_STRING T_STRING { TUPLE3(STRING("pin_prop_name_value3"),T_STRING,T_STRING) }

blockage_section: blockage_start blockage_defs blockage_end { TUPLE4(STRING("blockage_section1"),$1,$2,$3) }

blockage_start: K_BLOCKAGES NUMBER SEMICOLON { TUPLE4(STRING("blockage_start1"),K_BLOCKAGES,NUMBER $2,SEMICOLON) }

blockage_end: K_END K_BLOCKAGES { TUPLE3(STRING("blockage_end1"),K_END,K_BLOCKAGES) }

blockage_defs: /* empty */ { EMPTY_TOKEN }
	|	blockage_defs blockage_def { TUPLE3(STRING("blockage_defs2"),$1,$2) }

blockage_def: blockage_rule rectPoly_blockage rectPoly_blockage_rules SEMICOLON { TUPLE5(STRING("blockage_def1"),$1,$2,$3,SEMICOLON) }

blockage_rule: HYPHEN K_LAYER /* 151 */ T_STRING /* 152 */ layer_blockage_rules { TUPLE5(STRING("blockage_rule1"),HYPHEN,K_LAYER,T_STRING,$4) }
	|	HYPHEN K_PLACEMENT /* 153 */ placement_comp_rules { TUPLE4(STRING("blockage_rule1"),HYPHEN,K_PLACEMENT,$3) }

layer_blockage_rules: /* empty */ { EMPTY_TOKEN }
	|	layer_blockage_rules layer_blockage_rule { TUPLE3(STRING("layer_blockage_rules2"),$1,$2) }

layer_blockage_rule: PLUS K_SPACING NUMBER { TUPLE4(STRING("layer_blockage_rule1"),PLUS,K_SPACING,NUMBER $3) }
	|	PLUS K_DESIGNRULEWIDTH NUMBER { TUPLE4(STRING("layer_blockage_rule2"),PLUS,K_DESIGNRULEWIDTH,NUMBER $3) }
	|	mask_blockage_rule { ($1) }
	|	comp_blockage_rule { ($1) }

mask_blockage_rule: PLUS K_MASK NUMBER { TUPLE4(STRING("mask_blockage_rule1"),PLUS,K_MASK,NUMBER $3) }

comp_blockage_rule: PLUS K_COMPONENT /* 154 */ T_STRING { TUPLE4(STRING("comp_blockage_rule1"),PLUS,K_COMPONENT,T_STRING) }
	|	PLUS K_SLOTS { TUPLE3(STRING("comp_blockage_rule2"),PLUS,K_SLOTS) }
	|	PLUS K_FILLS { TUPLE3(STRING("comp_blockage_rule3"),PLUS,K_FILLS) }
	|	PLUS K_PUSHDOWN { TUPLE3(STRING("comp_blockage_rule4"),PLUS,K_PUSHDOWN) }
	|	PLUS K_EXCEPTPGNET { TUPLE3(STRING("comp_blockage_rule5"),PLUS,K_EXCEPTPGNET) }

placement_comp_rules: /* empty */ { EMPTY_TOKEN }
	|	placement_comp_rules placement_comp_rule { TUPLE3(STRING("placement_comp_rules2"),$1,$2) }

placement_comp_rule: PLUS K_COMPONENT /* 155 */ T_STRING { TUPLE4(STRING("placement_comp_rule1"),PLUS,K_COMPONENT,T_STRING) }
	|	PLUS K_PUSHDOWN { TUPLE3(STRING("placement_comp_rule2"),PLUS,K_PUSHDOWN) }
	|	PLUS K_SOFT { TUPLE3(STRING("placement_comp_rule3"),PLUS,K_SOFT) }
	|	PLUS K_PARTIAL NUMBER { TUPLE4(STRING("placement_comp_rule4"),PLUS,K_PARTIAL,NUMBER $3) }

rectPoly_blockage_rules: /* empty */ { EMPTY_TOKEN }
	|	rectPoly_blockage_rules rectPoly_blockage { TUPLE3(STRING("rectPoly_blockage_rules2"),$1,$2) }

rectPoly_blockage: K_RECT pt pt { TUPLE4(STRING("rectPoly_blockage1"),K_RECT,$2,$3) }
	|	K_POLYGON /* 156 */ firstPt nextPt nextPt otherPts { TUPLE6(STRING("rectPoly_blockage1"),K_POLYGON,$2,$3,$4,$5) }

slot_section: slot_start slot_defs slot_end { TUPLE4(STRING("slot_section1"),$1,$2,$3) }

slot_start: K_SLOTS NUMBER SEMICOLON { TUPLE4(STRING("slot_start1"),K_SLOTS,NUMBER $2,SEMICOLON) }

slot_end: K_END K_SLOTS { TUPLE3(STRING("slot_end1"),K_END,K_SLOTS) }

slot_defs: /* empty */ { EMPTY_TOKEN }
	|	slot_defs slot_def { TUPLE3(STRING("slot_defs2"),$1,$2) }

slot_def: slot_rule geom_slot_rules SEMICOLON { TUPLE4(STRING("slot_def1"),$1,$2,SEMICOLON) }

slot_rule: HYPHEN K_LAYER /* 157 */ T_STRING /* 158 */ geom_slot { TUPLE5(STRING("slot_rule1"),HYPHEN,K_LAYER,T_STRING,$4) }

geom_slot_rules: /* empty */ { EMPTY_TOKEN }
	|	geom_slot_rules geom_slot { TUPLE3(STRING("geom_slot_rules2"),$1,$2) }

geom_slot: K_RECT pt pt { TUPLE4(STRING("geom_slot1"),K_RECT,$2,$3) }
	|	K_POLYGON /* 159 */ firstPt nextPt nextPt otherPts { TUPLE6(STRING("geom_slot1"),K_POLYGON,$2,$3,$4,$5) }

fill_section: fill_start fill_defs fill_end { TUPLE4(STRING("fill_section1"),$1,$2,$3) }

fill_start: K_FILLS NUMBER SEMICOLON { TUPLE4(STRING("fill_start1"),K_FILLS,NUMBER $2,SEMICOLON) }

fill_end: K_END K_FILLS { TUPLE3(STRING("fill_end1"),K_END,K_FILLS) }

fill_defs: /* empty */ { EMPTY_TOKEN }
	|	fill_defs fill_def { TUPLE3(STRING("fill_defs2"),$1,$2) }

fill_def: fill_rule geom_fill_rules SEMICOLON { TUPLE4(STRING("fill_def1"),$1,$2,SEMICOLON) }
	|	HYPHEN K_VIA /* 160 */ T_STRING /* 161 */ fill_via_mask_opc_opt fill_via_pt SEMICOLON { TUPLE7(STRING("fill_def1"),HYPHEN,K_VIA,T_STRING,$4,$5,SEMICOLON) }

fill_rule: HYPHEN K_LAYER /* 162 */ T_STRING /* 163 */ fill_layer_mask_opc_opt geom_fill { TUPLE6(STRING("fill_rule1"),HYPHEN,K_LAYER,T_STRING,$4,$5) }

geom_fill_rules: /* empty */ { EMPTY_TOKEN }
	|	geom_fill_rules geom_fill { TUPLE3(STRING("geom_fill_rules2"),$1,$2) }

geom_fill: K_RECT pt pt { TUPLE4(STRING("geom_fill1"),K_RECT,$2,$3) }
	|	K_POLYGON /* 164 */ firstPt nextPt nextPt otherPts { TUPLE6(STRING("geom_fill1"),K_POLYGON,$2,$3,$4,$5) }

fill_layer_mask_opc_opt: /* empty */ { EMPTY_TOKEN }
	|	fill_layer_mask_opc_opt opt_mask_opc_l { TUPLE3(STRING("fill_layer_mask_opc_opt2"),$1,$2) }

opt_mask_opc_l: fill_layer_opc { ($1) }
	|	fill_mask { ($1) }

fill_layer_opc: PLUS K_OPC { TUPLE3(STRING("fill_layer_opc1"),PLUS,K_OPC) }

fill_via_pt: firstPt otherPts { TUPLE3(STRING("fill_via_pt1"),$1,$2) }

fill_via_mask_opc_opt: /* empty */ { EMPTY_TOKEN }
	|	fill_via_mask_opc_opt opt_mask_opc { TUPLE3(STRING("fill_via_mask_opc_opt2"),$1,$2) }

opt_mask_opc: fill_via_opc { ($1) }
	|	fill_viaMask { ($1) }

fill_via_opc: PLUS K_OPC { TUPLE3(STRING("fill_via_opc1"),PLUS,K_OPC) }

fill_mask: PLUS K_MASK NUMBER { TUPLE4(STRING("fill_mask1"),PLUS,K_MASK,NUMBER $3) }

fill_viaMask: PLUS K_MASK NUMBER { TUPLE4(STRING("fill_viaMask1"),PLUS,K_MASK,NUMBER $3) }

nondefaultrule_section: nondefault_start nondefault_def nondefault_defs nondefault_end { TUPLE5(STRING("nondefaultrule_section1"),$1,$2,$3,$4) }

nondefault_start: K_NONDEFAULTRULES NUMBER SEMICOLON { TUPLE4(STRING("nondefault_start1"),K_NONDEFAULTRULES,NUMBER $2,SEMICOLON) }

nondefault_end: K_END K_NONDEFAULTRULES { TUPLE3(STRING("nondefault_end1"),K_END,K_NONDEFAULTRULES) }

nondefault_defs: /* empty */ { EMPTY_TOKEN }
	|	nondefault_defs nondefault_def { TUPLE3(STRING("nondefault_defs2"),$1,$2) }

nondefault_def: HYPHEN /* 165 */ T_STRING /* 166 */ nondefault_options SEMICOLON { TUPLE5(STRING("nondefault_def1"),HYPHEN,T_STRING,$3,SEMICOLON) }

nondefault_options: /* empty */ { EMPTY_TOKEN }
	|	nondefault_options nondefault_option { TUPLE3(STRING("nondefault_options2"),$1,$2) }

nondefault_option: PLUS K_HARDSPACING { TUPLE3(STRING("nondefault_option1"),PLUS,K_HARDSPACING) }
	|	PLUS K_LAYER /* 167 */ T_STRING K_WIDTH NUMBER /* 168 */ nondefault_layer_options { TUPLE7(STRING("nondefault_option1"),PLUS,K_LAYER,T_STRING,K_WIDTH,NUMBER $5,$6) }
	|	PLUS K_VIA /* 169 */ T_STRING { TUPLE4(STRING("nondefault_option1"),PLUS,K_VIA,T_STRING) }
	|	PLUS K_VIARULE /* 170 */ T_STRING { TUPLE4(STRING("nondefault_option1"),PLUS,K_VIARULE,T_STRING) }
	|	PLUS K_MINCUTS /* 171 */ T_STRING NUMBER { TUPLE5(STRING("nondefault_option1"),PLUS,K_MINCUTS,T_STRING,NUMBER $4) }
	|	nondefault_prop_opt { ($1) }

nondefault_layer_options: /* empty */ { EMPTY_TOKEN }
	|	nondefault_layer_options nondefault_layer_option { TUPLE3(STRING("nondefault_layer_options2"),$1,$2) }

nondefault_layer_option: K_DIAGWIDTH NUMBER { TUPLE3(STRING("nondefault_layer_option1"),K_DIAGWIDTH,NUMBER $2) }
	|	K_SPACING NUMBER { TUPLE3(STRING("nondefault_layer_option2"),K_SPACING,NUMBER $2) }
	|	K_WIREEXT NUMBER { TUPLE3(STRING("nondefault_layer_option3"),K_WIREEXT,NUMBER $2) }

nondefault_prop_opt: PLUS K_PROPERTY /* 172 */ nondefault_prop_list { TUPLE4(STRING("nondefault_prop_opt1"),PLUS,K_PROPERTY,$3) }

nondefault_prop_list: /* empty */ { EMPTY_TOKEN }
	|	nondefault_prop_list nondefault_prop { CONS2($1,$2) }

nondefault_prop: T_STRING NUMBER { TUPLE3(STRING("nondefault_prop1"),T_STRING,NUMBER $2) }
	|	T_STRING QSTRING { TUPLE3(STRING("nondefault_prop2"),T_STRING,QSTRING $2) }
	|	T_STRING T_STRING { TUPLE3(STRING("nondefault_prop3"),T_STRING,T_STRING) }

styles_section: styles_start styles_rules styles_end { TUPLE4(STRING("styles_section1"),$1,$2,$3) }

styles_start: K_STYLES NUMBER SEMICOLON { TUPLE4(STRING("styles_start1"),K_STYLES,NUMBER $2,SEMICOLON) }

styles_end: K_END K_STYLES { TUPLE3(STRING("styles_end1"),K_END,K_STYLES) }

styles_rules: /* empty */ { EMPTY_TOKEN }
	|	styles_rules styles_rule { TUPLE3(STRING("styles_rules2"),$1,$2) }

styles_rule: HYPHEN K_STYLE NUMBER /* 173 */ firstPt nextPt otherPts SEMICOLON { TUPLE8(STRING("styles_rule1"),HYPHEN,K_STYLE,NUMBER $3,$4,$5,$6,SEMICOLON) }


