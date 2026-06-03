open Eparse
open Edif2
open Printf

let verbose = ref false
let verbose_templates = ref false
let mytop = ref ""
let nomatch = ref Edif
let unrecog = ref []

type conn =
| N of string
| B of string option array

type dir =
| UNKNOWN
| Input
| Output
| Inout

let dirx registered pnam = function
| UNKNOWN -> "UNKNOWN"
| Input -> "input "
| Output -> "output "^(if registered then "reg " else "")
| Inout -> "inout "

let dirx' = function
      | UNKNOWN -> "UNKNOWN"
      | Input -> "wire "
      | Output -> "reg "
      | Inout -> "inout "

type rslt =
| UNKNOWN
| NET of (string*string*rslt list)
| PINS of (string, string*dir) Hashtbl.t
| NETS of (string, unit) Hashtbl.t
| INSTS of (string, string*string*string*string) Hashtbl.t
| ITEMS of rslt list
| LIBRARY of string * rslt list
| DESIGN of string
| COMMENT of string
| CELL of string * rslt list * rslt list * rslt list * rslt list
| STATUS of int * int * int * int * int * int
| PROPERTY of string * rslt list
| PORTREF of string * int option * string option
| INST of string * (string*string*string*rslt list)
| PORT of string * string * int option * dir
| BOOLPROP of bool
| INTPROP of int
| STRPROP of string

let todir = function
| Edif2.Input -> Input
| Output -> Output
| Inout -> Inout
| _ -> UNKNOWN

let validate = function
  | "&_const0_" -> "GND$"
  | "&_const1_" -> "VCC$"
  | oth -> oth

let rec uniq ix nam =
  match nam.[ix] with
    | '0'..'9' ->
        let (trm,rslt) = uniq (ix-1) nam in (trm, Char.code (nam.[ix]) - Char.code ('0') + 10 * rslt)
    | _ -> String.sub nam 0 (ix+1), 0

let uniq nam = uniq (String.length nam - 1) nam

let array1 str =
  Scanf.sscanf str "\"%[a-zA-Z0-9_][%[0-9]]\"" (fun nam wid -> (nam,Some (int_of_string wid))) 

let rec rw = function
| ITEM2 (Edif, TLIST [ID top],
  TLIST
   (ITEM (Edifversion, TLIST [INT 2; INT 0; INT 0]) ::
    ITEM (EdifLevel, TLIST [INT 0]) ::
    ITEM2 (Keywordmap, TLIST [], TLIST [ITEM (Keywordlevel, TLIST [INT 0])]) ::
    items)) ->
   mytop := top;
   let itms = List.map (rw) items in
   ITEMS itms
| ITEM2 (Design, TLIST [ID top],
    TLIST
     [ITEM2
       (Cellref, TLIST [ID top'],
        TLIST [ITEM (Libraryref, TLIST [ID "work"])]);
      ITEM2
       (Property, TLIST [ID "part"],
        TLIST [ITEM (String, TLIST [STRING part])])]) -> DESIGN top
| ITEM (Comment, TLIST [STRING str]) -> COMMENT str
| ITEM2 (Library, TLIST [ID libid],
    TLIST
     (ITEM (EdifLevel, TLIST [INT 0]) ::
      ITEM2 (Technology, TLIST [], TLIST [ITEM (NumberDefinition, TLIST [])]) ::
      cells)) -> LIBRARY (libid, List.map (rw) cells)
| (ITEM2 (Cell, TLIST [ID cellid],
    TLIST
     (ITEM (Celltype, TLIST [ID "GENERIC"]) ::
      ITEM2
       (View, TLIST [ID cellid'],
        TLIST
         (ITEM (Viewtype, TLIST [ID "NETLIST"]) ::
          ITEM2 (Interface, TLIST [], TLIST ports) ::
          ITEM2 (Contents, TLIST [], TLIST (insts_nets)) ::
          properties)) :: []))) ->
let p = List.map (rw) ports in
let (il,nl) = List.partition (function ITEM2 (Instance, _, _) -> true | _ -> false) insts_nets in
let i = List.map (rw) il in
let n = List.map (rw) nl in
let prop = List.map (rw) properties in
CELL (cellid, p, i, n, prop)
| ITEM2 (Cell, TLIST [ID cellid],
    TLIST
     (ITEM (Celltype, TLIST [ID "GENERIC"]) ::
      ITEM2
       (View, TLIST [ID cellid'],
        TLIST
         (ITEM (Viewtype, TLIST [ID "NETLIST"]) ::
          ITEM2 (Interface, TLIST [], TLIST ports) ::
          properties)) :: [])) ->
let p = List.map (rw) ports in
let prop = List.map (rw) properties in
let c = CELL (cellid, p, [], [], prop) in
(match (uniq cellid),p with
  | ("RTL_BSEL",_),PORT ("I", "I", None, Input)::tl ->
     let c = CELL (cellid, PORT ("I", "I", Some 1, Input)::tl, [], [], prop) in c
  | ("RTL_BMERGE",_),data::i::PORT ("O", "O", None, Output)::tl ->
     let c = CELL (cellid, data::i::PORT ("O", "O", Some 1, Output)::tl, [], [], prop) in c
  | _ -> c)
| ITEM2 (Status, TLIST [],
    TLIST
     [ITEM2
       (Written, TLIST [],
        TLIST
         [ITEM (TimeStamp,
            TLIST [INT yr; INT mon; INT day; INT hr; INT min; INT sec]);
          ITEM2 (Program, TLIST [vivado],
            TLIST [ITEM (ID "Version", TLIST [STRING _])]);
          ITEM (Comment,
            TLIST [STRING built_on]);
          ITEM (Comment, TLIST [STRING xbuild])])]) ->
STATUS(yr, mon, day, hr, min, sec)
| ITEM2 (Property, TLIST [ID prop_id], TLIST items) -> PROPERTY(prop_id, List.map (rw) items)
| ITEM2 (Net, TLIST [ID netid],
    TLIST
     (ITEM2
       (Joined, TLIST [],
        TLIST ports) :: properties)) -> NET (validate netid, netid, List.map (rw) ports)
| ITEM2 (Portref, TLIST [],
            TLIST
             [ITEM (Member, TLIST [ID busid; INT n]);
              ITEM (Instanceref, TLIST [ID instid])]) -> PORTREF(busid, Some n, Some instid)
| ITEM (Portref, TLIST [ID portref]) -> PORTREF(portref, None, None)
| ITEM2 (Net, TLIST [],
    TLIST
     (ITEM (Rename, TLIST [ID netid; STRING rnetid]) ::
      ITEM2 (Joined, TLIST [], TLIST ports) :: properties)) ->
    let p = List.map (rw) ports in
    NET (validate netid, rnetid, p)
| ITEM2 (Instance, TLIST [ID instid],
    TLIST
     (ITEM2
       (Viewref, TLIST [ID viewid],
        TLIST
         [ITEM2
           (Cellref, TLIST [ID cellid],
                     TLIST [ITEM (Libraryref, TLIST [ID libid])])]) :: properties)) ->
    let prop = List.map (rw) properties in
    INST (instid, (viewid,cellid,libid,prop))
| ITEM2 (Instance, TLIST [],
    TLIST
     (ITEM
       (Rename,
        TLIST [ID instid; STRING _]) ::
      ITEM2
       (Viewref, TLIST [ID viewid],
        TLIST
         [ITEM2
           (Cellref, TLIST [ID cellid],
            TLIST [ITEM (Libraryref, TLIST [ID libid])])]) :: properties))     ->
    let prop = List.map (rw) properties in
    INST (instid, (viewid,cellid,libid,prop))

| ITEM2 (Portref, TLIST [ID portid],
             TLIST [ITEM (Instanceref, TLIST [ID instid])]) -> PORTREF(portid,None, Some instid)
| ITEM2 (Portref, TLIST [],
                  TLIST [ITEM (Member, TLIST [ID busid; INT bix])]) -> PORTREF(busid, Some bix, None)
| ITEM2 (Port, TLIST [ID portid],
               TLIST (ITEM (Direction, TLIST [(Input|Output|Inout as dir)]) :: prop)) ->
PORT (portid, portid, None, todir dir)
| ITEM2
   (Port, TLIST [],
    TLIST
     [ITEM (Rename, TLIST [ID portid; STRING str]);
      ITEM (Direction, TLIST [(Input|Output|Inout as dir)])]) ->
let (nam,wid) = try array1 str with _ -> (portid,None) in
PORT (nam, str, wid, todir dir)
| ITEM2 (Port, TLIST [],
    TLIST
     [ITEM2
       (Array, ITEM (Rename, TLIST [ID portid; STRING str]), INT wid);
      ITEM (Direction, TLIST [(Input|Output|Inout as dir)])]) ->
PORT (portid, str, Some wid, todir dir)
| ITEM (Integer, TLIST [INT n]) -> INTPROP n
| ITEM (String, TLIST [STRING str]) -> STRPROP str
| ITEM2 (Boolean, TLIST [], TLIST [ITEM (truth, TLIST [])]) ->
    BOOLPROP (match truth with True -> true | False -> false | _ -> false)
| oth -> nomatch := oth; UNKNOWN

let reph = Hashtbl.create 255
let always_oth = ref None

let rec always_kind' = function
| ID id -> id :: []
| TLIST lst -> List.flatten (List.map always_kind' lst)
| TRIPLE (ID id, INT _, INT _) -> id :: []
| DOUBLE (ID id, INT _) -> id :: []
| TRIPLE (INT 1, QUOTE, ID "b0") -> []
| TRIPLE (INT 1, QUOTE, ID "b1") -> []
| oth -> always_oth := Some oth; failwith "always_kind"

let always_kind lst =
let lst' = List.sort compare (List.flatten (List.map always_kind' lst)) in
let lst'' = ref [List.hd lst'] in
List.iter (fun itm -> if itm <> List.hd !lst'' then lst'' := itm :: !lst'') (List.tl lst');
"@("^String.concat " or " (List.rev !lst'')^")"

let rtl_reg str = "parameter ASYNC_LOAD = 1'b0;\n"^str

let mmcm_adv inst ports =
let buf1 = Buffer.create 256 in
bprintf buf1 "  parameter BANDWIDTH = \"OPTIMIZED\";\n";
bprintf buf1 "  parameter real CLKFBOUT_MULT_F = 5.000;\n";
bprintf buf1 "  parameter real CLKFBOUT_PHASE = 0.000;\n";
bprintf buf1 "  parameter CLKFBOUT_USE_FINE_PS = \"FALSE\";\n";
bprintf buf1 "  parameter real CLKIN1_PERIOD = 0.000;\n";
bprintf buf1 "  parameter real CLKIN2_PERIOD = 0.000;\n";
bprintf buf1 "  parameter real CLKOUT0_DIVIDE_F = 1.000;\n";
bprintf buf1 "  parameter real CLKOUT0_DUTY_CYCLE = 0.500;\n";
bprintf buf1 "  parameter real CLKOUT0_PHASE = 0.000;\n";
bprintf buf1 "  parameter CLKOUT0_USE_FINE_PS = \"FALSE\";\n";
bprintf buf1 "  parameter integer CLKOUT1_DIVIDE = 1;\n";
bprintf buf1 "  parameter real CLKOUT1_DUTY_CYCLE = 0.500;\n";
bprintf buf1 "  parameter real CLKOUT1_PHASE = 0.000;\n";
bprintf buf1 "  parameter CLKOUT1_USE_FINE_PS = \"FALSE\";\n";
bprintf buf1 "  parameter integer CLKOUT2_DIVIDE = 1;\n";
bprintf buf1 "  parameter real CLKOUT2_DUTY_CYCLE = 0.500;\n";
bprintf buf1 "  parameter real CLKOUT2_PHASE = 0.000;\n";
bprintf buf1 "  parameter CLKOUT2_USE_FINE_PS = \"FALSE\";\n";
bprintf buf1 "  parameter integer CLKOUT3_DIVIDE = 1;\n";
bprintf buf1 "  parameter real CLKOUT3_DUTY_CYCLE = 0.500;\n";
bprintf buf1 "  parameter real CLKOUT3_PHASE = 0.000;\n";
bprintf buf1 "  parameter CLKOUT3_USE_FINE_PS = \"FALSE\";\n";
bprintf buf1 "  parameter CLKOUT4_CASCADE = \"FALSE\";\n";
bprintf buf1 "  parameter integer CLKOUT4_DIVIDE = 1;\n";
bprintf buf1 "  parameter real CLKOUT4_DUTY_CYCLE = 0.500;\n";
bprintf buf1 "  parameter real CLKOUT4_PHASE = 0.000;\n";
bprintf buf1 "  parameter CLKOUT4_USE_FINE_PS = \"FALSE\";\n";
bprintf buf1 "  parameter integer CLKOUT5_DIVIDE = 1;\n";
bprintf buf1 "  parameter real CLKOUT5_DUTY_CYCLE = 0.500;\n";
bprintf buf1 "  parameter real CLKOUT5_PHASE = 0.000;\n";
bprintf buf1 "  parameter CLKOUT5_USE_FINE_PS = \"FALSE\";\n";
bprintf buf1 "  parameter integer CLKOUT6_DIVIDE = 1;\n";
bprintf buf1 "  parameter real CLKOUT6_DUTY_CYCLE = 0.500;\n";
bprintf buf1 "  parameter real CLKOUT6_PHASE = 0.000;\n";
bprintf buf1 "  parameter CLKOUT6_USE_FINE_PS = \"FALSE\";\n";
bprintf buf1 "  parameter COMPENSATION = \"ZHOLD\";\n";
bprintf buf1 "  parameter integer DIVCLK_DIVIDE = 1;\n";
bprintf buf1 "  parameter [0:0] IS_CLKINSEL_INVERTED = 1'b0;\n";
bprintf buf1 "  parameter [0:0] IS_PSEN_INVERTED = 1'b0;\n";
bprintf buf1 "  parameter [0:0] IS_PSINCDEC_INVERTED = 1'b0;\n";
bprintf buf1 "  parameter [0:0] IS_PWRDWN_INVERTED = 1'b0;\n";
bprintf buf1 "  parameter [0:0] IS_RST_INVERTED = 1'b0;\n";
bprintf buf1 "  parameter real REF_JITTER1 = 0.010;\n";
bprintf buf1 "  parameter real REF_JITTER2 = 0.010;\n";
bprintf buf1 "  parameter SS_EN = \"FALSE\";\n";
bprintf buf1 "  parameter SS_MODE = \"CENTER_HIGH\";\n";
bprintf buf1 "  parameter integer SS_MOD_PERIOD = 10000;\n";
bprintf buf1 "  parameter STARTUP_WAIT = \"FALSE\";\n";
let maplst = List.map (fun s -> "."^s^"("^s^")") ports in
false, (Buffer.contents buf1^inst^" i ("^String.concat ",\n" (maplst)^");\n")

let functionality plist cnt = function
      | "BUFG",["I";"O"] -> false, sprintf "assign O = I; // 0"
      | "GND",["G"] -> false, sprintf "    parameter integer DRIVE = 12;\n  parameter SLEW = \"SLOW\";\nassign G = 1'b0;\n"
      | "IBUFDS",["I";"IB";"O"] -> false, sprintf "parameter CAPACITANCE = \"DONT_CARE\";\n  parameter DIFF_TERM = \"FALSE\";\n  parameter DQS_BIAS = \"FALSE\";\n  parameter IBUF_DELAY_VALUE = \"0\";\n  parameter IBUF_LOW_PWR = \"TRUE\";\n  parameter IFD_DELAY_VALUE = \"AUTO\";\n  parameter IOSTANDARD = \"DEFAULT\";\nassign O = I | !IB; // 0"
      | "IBUF",["I";"O"] -> false, sprintf "    parameter CAPACITANCE = \"DONT_CARE\";\n    parameter IBUF_DELAY_VALUE = \"0\";\n    parameter IBUF_LOW_PWR = \"TRUE\";\n    parameter IFD_DELAY_VALUE = \"AUTO\";\n    parameter IOSTANDARD = \"DEFAULT\";\nassign O=I; // 0"
      | "INV",["I";"O"] -> false, sprintf "assign O=I; // 0"
      | "IOBUF",["I";"IO";"O";"T"] -> false, sprintf "parameter IOSTANDARD=\"DEFAULT\";\nassign IO = !T ? I : 1'bZ; assign O = IO; // 0"
      | ("MMCME2_ADV" as inst),ports -> mmcm_adv inst ports
      | "OBUF",["I";"O"] -> false, sprintf "    parameter CAPACITANCE = \"DONT_CARE\";\n    parameter integer DRIVE = 12;\n    parameter IOSTANDARD = \"DEFAULT\";\nassign O=I; // 0"
      | "RTL_ADD",["I0";"I1";"O"] -> false, sprintf "assign O = I0 + I1;\n"
      | "RTL_AND",["I0";"I1";"O"] -> false, sprintf "assign O = I0 & I1; // 12"
      | "RTL_ALSHIFT",["I0";"I1";"I2";"O"] -> false, sprintf "assign O = $signed(I0) << I1; // 0"
      | "ARSH_TC_UNS_OP",["I0";"I1";"I2";"O"] -> false, sprintf "assign O = $signed(I0) >> I1; // 0"
      | "ARSH_UNS_UNS_OP",["I0";"I1";"I2";"O"] -> false, sprintf "assign O = $unsigned(I0) >> I1; // 0"
      | "RTL_EQ",["I0";"I1";"O"] -> false, sprintf "assign O = I1 == I0; // 28"
      | "RTL_GEQ",["I0";"I1";"O"] -> false, sprintf "assign O = I1 >= I0; // 11"
      | "RTL_GT",["I0";"I1";"O"] -> false, sprintf "assign O = I1 > I0; // 11"
      | "RTL_INV",["I0";"O"] -> false, sprintf "assign O=~I0; // 2"
      | "RTL_LATCH",["D";"G";"Q"] -> false, sprintf "initial $stop; // 317\n"
      | "RTL_LEQ",["I0";"I1";"O"] -> false, sprintf "assign O = I1 <= I0; // 11"
      | "RTL_LSHIFT",["I0";"I1";"I2";"O"] -> false, sprintf "assign O = I0 << I1; // 0"
      | "RTL_LT",["I0";"I1";"O"] -> false, sprintf "assign O = I1 < I0; // 11"
      | "RTL_MULT",["I0";"I1";"O"] -> false, sprintf "assign O = I0 * I1;\n"
      | "RTL_NEQ",["I0";"I1";"O"] -> false, sprintf "assign O = I0 != I1; // 2"
      | "RTL_OR",["I0";"I1";"O"] -> false, sprintf "assign O = I0 | I1; // 0"
      | "RTL_REDUCTION_AND",["I0";"O"] -> false, sprintf "assign O = &I0; // 0"
      | "RTL_REDUCTION_OR",["I0";"O"] -> false, sprintf "assign O = |I0; // 0"
      | "RTL_REDUCTION_XOR",["I0";"O"] -> false, sprintf "assign O = ^I0; // 0"
      | "RTL_REG_ASYNC__BREG_",["C";"CE";"CLR";"D";"PRE";"Q"] -> true, rtl_reg "always @(posedge C or posedge PRE or posedge CLR) if (PRE) Q <= 1'b1; else if (CLR) Q <= 1'b0; else if (CE) Q <= D;"
      | "RTL_REG_ASYNC__BREG_",["C";"CE";"CLR";"D";"Q"] -> true, rtl_reg "always @(posedge C or posedge CLR) if (CLR) Q <= 1'b0; else if (CE) Q <= D;"
      | "RTL_REG_ASYNC__BREG_",["C";"CE";"D";"PRE";"Q"] -> true, rtl_reg "always @(posedge C or posedge PRE) if (PRE) Q <= 1'b1; else if (CE) Q <= D;"
      | "RTL_REG_ASYNC__BREG_",["C";"CLR";"D";"PRE";"Q"] -> true, rtl_reg "always @(posedge C or posedge PRE or posedge CLR) if (PRE) Q <= 1'b1; else if (CLR) Q <= 1'b0; else Q <= D;"
      | "RTL_REG_ASYNC__BREG_",["C";"CLR";"D";"Q"] -> true, rtl_reg "always @(posedge C or posedge CLR) if (CLR) Q <= 1'b0; else Q <= D;"
      | "RTL_REG_ASYNC__BREG_",["C";"D";"PRE";"Q"] -> true, rtl_reg "always @(posedge C or posedge PRE) if (PRE) Q <= 1'b1; else Q <= D;"
      | "RTL_REG__BREG_",["C";"CE";"D";"Q"] -> true, rtl_reg "always @(posedge C) if (CE) Q <= D;\n"
      | "RTL_REG__BREG_",["C";"D";"Q"] -> true, rtl_reg "always @(posedge C) Q <= D;\n"
      | "RTL_REG_SYNC__BREG_",["C";"D";"Q";"RST"] -> true, rtl_reg "always @(posedge C) if (RST) Q <= 1'b0; else Q <= D; // 204"
      | "RTL_RSHIFT",["I0";"I1";"I2";"O"] -> false, sprintf "assign O = I0 >> I1; // 4"
      | "RTL_SUB",["I0";"I1";"O"] -> false, sprintf "assign O = I0 - I1;\n"
      | "RTL_XOR",["I0";"I1";"O"] -> false, sprintf "assign O = I0 ^ I1; // 0"
      | "VCC",["P"] -> false, sprintf "assign P = 1'b1; // 0"
      | "GTECH_BUF",["A";"Z"] -> false, sprintf  "initial $stop; // 340\n"
      | "GTECH_XOR",["A";"B";"Z"] -> false, sprintf  "initial $stop; // 341\n"
      | "GTECH_OR",["A";"B";"Z"] -> false, sprintf  "initial $stop; // 342\n"
      | "GTECH_AND",["A";"B";"Z"] -> false, sprintf  "initial $stop; // 343\n"
      | "GTECH_AND",["A";"B";"C";"Z"] -> false, sprintf  "initial $stop; // 344\n"
      | "SELECT_OP",["CONTROL1";"CONTROL10";"CONTROL11";"CONTROL12";"CONTROL13";"CONTROL14";"CONTROL15";"CONTROL16";"CONTROL17";"CONTROL18";"CONTROL19";"CONTROL2";"CONTROL20";"CONTROL21";"CONTROL22";"CONTROL23";"CONTROL24";"CONTROL25";"CONTROL26";"CONTROL27";"CONTROL28";"CONTROL29";"CONTROL3";"CONTROL30";"CONTROL4";"CONTROL5";"CONTROL6";"CONTROL7";"CONTROL8";"CONTROL9";"DATA1";"DATA10";"DATA11";"DATA12";"DATA13";"DATA14";"DATA15";"DATA16";"DATA17";"DATA18";"DATA19";"DATA2";"DATA20";"DATA21";"DATA22";"DATA23";"DATA24";"DATA25";"DATA26";"DATA27";"DATA28";"DATA29";"DATA3";"DATA30";"DATA4";"DATA5";"DATA6";"DATA7";"DATA8";"DATA9";"Z"] -> false, sprintf  "initial $stop; // 345\n"
      | "GTECH_NOT",["A";"Z"] -> false, sprintf  "initial $stop; // 346\n"
      | "ADD_UNS_OP",["A";"B";"Z"] -> false, sprintf  "initial $stop; // 347\n"
      | "ADD_TC_OP",["A";"B";"Z"] -> false, sprintf  "initial $stop; // 348\n"
      | "EQ_UNS_OP",["A";"B";"Z"] -> false, sprintf  "initial $stop; // 349\n"
      | "MULT_TC_OP",["A";"B";"Z"] -> false, sprintf  "initial $stop; // 350\n"
      | "SUB_TC_OP",["A";"B";"Z"] -> false, sprintf  "initial $stop; // 351\n"
      | "SUB_UNS_OP",["A";"B";"Z"] -> false, sprintf  "initial $stop; // 352\n"

      | (stm,plist) -> let plist' = List.map (fun s -> "\""^s^"\"") plist in
             if not (Hashtbl.mem reph (stm,plist')) && !verbose then
                 begin
                 Hashtbl.add reph (stm,plist') ();
                 (* Add this snippet to the case statement above *)
                 printf "      | \"%s\",[%s] -> false, sprintf  \"initial $stop; // %d\"\n" stm (String.concat ";" plist') cnt;
                 false, sprintf "initial $stop; // %d\"\n" cnt;
                 end
             else false, ""

let cellhash = Hashtbl.create 255
let leaf_lst = ref []

let rec elab modhash path liblst nam =
   let leaf = List.mem nam (fst liblst) || (String.length nam >= 6 && String.sub nam 0 6 = "GTECH_") in
   if leaf then
      begin
      leaf_lst := (if path <> "" then path^"."^nam else nam) :: !leaf_lst;
      ""
      end
   else
      begin
      if Hashtbl.mem modhash nam then
         begin
         let itmlst = Hashtbl.find modhash nam in
         List.iter (function QUADRUPLE(ID kind, _, ID inst, _) -> let _ = elab modhash (path^"."^nam) liblst inst in () | _ -> ()) itmlst;
         end;
      "_elab"
      end

let rec dump liblst fd = function
| ITEMS lst -> List.iter (dump liblst fd) lst
| STATUS _ -> ()
| oth -> ()

let pragma fd = function
| DOUBLE(ID id, STRING s) -> fprintf fd "%s = %s" id s
| DOUBLE(Delay, STRING s) -> fprintf fd "delay = %s" s
| DOUBLE(ID id, INT n) -> fprintf fd "%s = %d" id n
| ID id -> fprintf fd "%s" id
| oth -> failwith "pragma"

let rec expr fd = function
| ID s -> fprintf fd "%s" s
| DOUBLE(ID s, INT n) -> fprintf fd "%s[%d]" s n
| TRIPLE(ID s, INT hi, INT lo)  -> fprintf fd "%s[%d:%d]" s hi lo
| TRIPLE(INT wid, QUOTE, ID cnst) -> fprintf fd "%d'%s" wid cnst
| TLIST lst -> let delim = ref "{" in List.iter (fun itm -> fprintf fd "%s" !delim; expr fd itm; delim := ",") lst; fprintf fd "}"
| DOUBLE(TILDE, a) -> fprintf fd "~(";
    expr fd a; 
    fprintf fd ")\n"
| DOUBLE(a,b) ->
    fprintf fd "DOUBLE(";
    expr fd a;
    fprintf fd ",";
    expr fd b; 
    fprintf fd ")\n";
| TRIPLE(a,VBAR,b) ->
    fprintf fd "(";
    expr fd a;
    fprintf fd "|";
    expr fd b;
    fprintf fd ")\n";
| TRIPLE(a,AMPERSAND,b) ->
    fprintf fd "(";
    expr fd a;
    fprintf fd "&";
    expr fd b;
    fprintf fd ")\n";
| TRIPLE(a,b,c) ->
    fprintf fd "TRIPLE(";
    expr fd a;
    fprintf fd ",";
    expr fd b;
    fprintf fd ",";
    expr fd c; 
    fprintf fd ")\n";
| oth -> fprintf fd "%s" (str_token oth)

let dirx = function
   | Wire -> "wire" 
   | Tri -> "tri" 
   | Input -> "input"
   | Output -> "output" 
   | Inout -> "inout" 
   | oth -> str_token oth

let othitm = ref []
let rom_othitm = ref []
let generic_othitm = ref []
   
let rec todec' err base ix value =
let lsb = match value.[ix] with
| '0'..'9' as dig -> Char.code dig - Char.code '0'
| 'A'..'F' as dig -> Char.code dig - Char.code 'A' + 10
| 'a'..'f' as dig -> Char.code dig - Char.code 'a' + 10
| 'x' | 'X' | _ -> err := true; 0 in
(if ix > 1 then todec' err base (ix-1) value * base else 0) + lsb

let todec err value =
let base = match value.[0] with
| 'b' -> 2;
| 'd' -> 10;
| 'h' -> 16;
| _ -> err := true; 0 in
todec' err base (String.length value - 1) value

let tosdec err value =
if value.[0] = 's' then todec err (String.sub value 1 (String.length value - 1)) else todec err value

let unquote value =
  if value.[0] == '"' then
    String.sub value 1 (String.length value - 2)
  else
    value

let rtl_active pin connlst' =
    List.mem_assoc pin connlst' && (match List.assoc pin connlst' with TRIPLE(INT 1, QUOTE, ID "b0") -> false | _ -> true)

let rtl_inv fd inst pin connlst' =
    if List.mem_assoc pin connlst' then expr fd (List.assoc pin connlst') else fprintf fd "%s_%s" inst pin

let rtl_op' fd inst connlst op z a b sgn =
    let connlst' = List.map (function DOUBLE(ID id, e) -> (id,e) | _ -> ("",INT 0)) connlst in
    fprintf fd "assign ";
    rtl_inv fd inst z connlst';
    fprintf fd " = %s(" sgn;
    rtl_inv fd inst a connlst';
    fprintf fd ") %s %s(" op sgn;
    rtl_inv fd inst b connlst';
    fprintf fd ");\n"

let rtl_op fd inst connlst op = rtl_op' fd inst connlst op "Z" "A" "B" ""
let rtl_op_tc fd inst connlst op = rtl_op' fd inst connlst op "Z" "A" "B" "$signed"
let rtl_op_uns fd inst connlst op = rtl_op' fd inst connlst op "Z" "A" "B" "$unsigned"
let rtl_op_tc_sh fd inst connlst op = rtl_op' fd inst connlst op "Z" "A" "SH" "$signed"
let rtl_op_uns_sh fd inst connlst op = rtl_op' fd inst connlst op "Z" "A" "SH" "$unsigned"

let rtl_bsel fd inst connlst kind =
    let connlst' = List.map (function DOUBLE(ID id, e) -> (id,e) | _ -> ("",INT 0)) connlst in
    let plist = Hashtbl.find cellhash kind in
    fprintf fd "assign ";
    rtl_inv fd inst "Z" connlst';
    fprintf fd " = ";
    rtl_inv fd inst "I" connlst';
    fprintf fd " [ ";
    rtl_inv fd inst "S" connlst';
    let _ = match snd(List.assoc "O" plist) with Some siz -> fprintf fd " +: %d" siz | None -> () in
    fprintf fd " ];\n"

let rtl_unary fd inst connlst op =
    let connlst' = List.map (function DOUBLE(ID id, e) -> (id,e) | _ -> ("",INT 0)) connlst in
    fprintf fd "assign ";
    rtl_inv fd inst "Z" connlst';
    fprintf fd " = ";
    fprintf fd " %s " op;
    rtl_inv fd inst "A" connlst';
    fprintf fd ";\n"

let rtl_pwr fd inst connlst op port =
    let connlst' = List.map (function DOUBLE(ID id, e) -> (id,e) | _ -> ("",INT 0)) connlst in
    fprintf fd "assign ";
    rtl_inv fd inst port connlst';
    fprintf fd " = %s;\n" op

let items modhash path itemhash liblst fd = function
| DOUBLE(dir, TLIST idlst) -> List.iter (function
    | ID id -> fprintf fd "%s %s;\n" (dirx dir) id; Hashtbl.replace itemhash id (0,0)
    | TRIPLE(INT hi, INT lo, ID id) -> fprintf fd "%s [%d:%d] %s;\n" (dirx dir) hi lo id; Hashtbl.replace itemhash id (hi,lo)
    | TRIPLE(INT hi, INT lo, kw) -> let id = str_token kw in fprintf fd "%s [%d:%d] %s;\n" (dirx dir) hi lo id; Hashtbl.replace itemhash id (hi,lo)
    | kw -> let id = str_token kw in fprintf fd "%s %s;\n" (dirx dir) id; Hashtbl.replace itemhash id (0,0)) idlst;
| TRIPLE(Assign, lhs, e) -> fprintf fd "assign "; expr fd lhs; fprintf fd " = "; expr fd e; fprintf fd ";\n"
| QUADRUPLE(ID kind, TLIST actparm', ID inst, TLIST connlst) -> (match uniq kind with
  | "RTL_RAM",_ -> 
    let connlst' = List.map (function DOUBLE(ID id, e) -> (id,e) | _ -> ("",INT 0)) connlst in
    let plist = Hashtbl.find cellhash kind in
    let depth = match List.nth plist 0 with (_, (_, Some wid)) -> wid | _ -> 1 in
    let width = match List.nth plist 2 with (_, (_, Some wid)) -> wid | _ -> 1 in
    fprintf fd "reg [%d:0] %s_mem [0:%d];\n" (width-1) inst ((1 lsl depth)-1);

    let pcnt = ref 0 in List.iter (function
      | (pnam, (Output, None)) ->
        fprintf fd "reg\t%s_%d;\n" inst !pcnt; incr pcnt
      | (pnam, (Output, Some wid)) ->
        fprintf fd "reg\t[%d:0] %s_%d;\n" (wid-1) inst !pcnt; incr pcnt
      | _ -> ()) plist;
    List.iter (fun (id,e) -> print_endline id) connlst';
    for i = 1 to !pcnt do
        fprintf fd "assign "; expr fd (List.assoc ("RO"^string_of_int i) connlst'); fprintf fd " = %s_%d;\n" inst (i-1);
    done;
    fprintf fd "always @(posedge ";
    rtl_inv fd inst "WCLK" connlst';
    fprintf fd ") if (";
    let we = "WE"^string_of_int (1 + !pcnt) in
    rtl_inv fd inst we connlst';
    fprintf fd ") %s_mem[" inst;
    let wa = "WA"^string_of_int (1 + !pcnt) in
    rtl_inv fd inst wa connlst';
    fprintf fd "] <= ";
    let wd = "WD"^string_of_int (1 + !pcnt) in
    rtl_inv fd inst wd connlst';
    fprintf fd ";\n";
    for i = 1 to !pcnt do
        let ra = "RA"^string_of_int i in
        fprintf fd "always %s " (always_kind [List.assoc ra connlst']);
        fprintf fd "%s_%d = mem[" inst (i-1);
        rtl_inv fd inst ra connlst';
        fprintf fd "];\n";
    done

  | "RTL_ROM",_ -> 
    let connlst' = List.map (function DOUBLE(ID id, e) -> (id,e) | _ -> ("",INT 0)) connlst in
    let plist = Hashtbl.find cellhash kind in
    List.iter (function
      | (pnam, (Output, None)) ->
        fprintf fd "reg\t%s;\n" inst
      | (pnam, (Output, Some wid)) ->
        fprintf fd "reg\t[%d:0] %s;\n" (wid-1) inst
      | _ -> ()) plist;
    let conn = List.assoc "O" connlst' in
    fprintf fd "assign "; expr fd conn; fprintf fd " = %s;\n" inst;
    let actparm = actparm' in
    let inputs = List.filter (fun (id,e) -> match List.assoc id plist with (Input,_) -> true | _ -> false) connlst' in
    let inputexp = List.map (fun (id,e) -> e) inputs in
    fprintf fd "always %s casex (" (always_kind inputexp); 
    rtl_inv fd inst "A" connlst';
    fprintf fd ")\n";
    let dflt = ref false and wid = ref 0 in let cases = List.map (function
                   | DOUBLE (ID nam, TRIPLE (INT w, QUOTE, ID value)) ->
                       let nam' = (String.sub nam 5 (String.length nam - 5)) in
                       if nam' = "DEFAULT" then
                           (dflt := true; sprintf "default: %s = %d'%s;\n" inst w value)
                       else
                           (wid := w; sprintf "%7d: %s = %d'%s;\n" (int_of_string nam') inst w value);
                   | oth -> rom_othitm := oth :: !rom_othitm; failwith "actparm") actparm in
    List.iter (fprintf fd "%s") (List.sort compare (if not !dflt then ("default: "^inst^" = "^string_of_int !wid^"'b"^String.make !wid '0'^";\n") :: cases else cases));
    fprintf fd "endcase\n\n";
  | "MUX_OP",_ -> 
    let connlst' = List.map (function DOUBLE(ID id, e) -> (id,e) | _ -> ("",INT 0)) connlst in
    let dpins = Array.make (List.length connlst) None in
    let spins = Array.make (List.length connlst) None in
    List.iter (function
      | DOUBLE (ID "Z", ID id) -> let (hi,lo) = Hashtbl.find itemhash id in fprintf fd "reg\t[%d:%d] %s;\n" hi lo inst
      | DOUBLE (ID "Z", DOUBLE (ID _, INT ix)) -> fprintf fd "reg\t%s;\n" inst
      | DOUBLE (ID "Z", TLIST lst) -> fprintf fd "reg\t[%d:0] %s;\n" ((List.length lst)-1) inst
      | DOUBLE (ID id, exp) as pat -> (match id.[0] with
         | 'D' -> let ix = int_of_string (String.sub id 1 (String.length id - 1)) in dpins.(ix) <- Some exp
         | 'S' -> let ix = int_of_string (String.sub id 1 (String.length id - 1)) in spins.(ix) <- Some exp
         | _ -> unrecog := pat :: !unrecog)
      | oth -> unrecog := oth :: !unrecog) connlst;
    let dpins = List.filter (function Some _ -> true | None -> false) (Array.to_list dpins) in
    let dpins = List.map (function Some itm -> itm | _ -> INT 0) dpins in
    let spins = List.filter (function Some _ -> true | None -> false) (Array.to_list spins) in
    let spins = List.rev (List.map (function Some itm -> itm | _ -> INT 0) spins) in
    let conn = List.assoc "Z" connlst' in
    fprintf fd "assign "; expr fd conn; fprintf fd " = %s;\n" inst;
    let inputexp = spins @ dpins in
    fprintf fd "always %s casex (" (always_kind inputexp); 
    expr fd (TLIST spins);
    fprintf fd ")\n";
    List.iteri (fun ix e -> fprintf fd "'h%x: %s = " ix inst; expr fd e; fprintf fd ";\n") dpins;
    fprintf fd "endcase\n\n";
  | "SELECT_OP",_ -> 
    let connlst' = List.map (function DOUBLE(ID id, e) -> (id,e) | _ -> ("",INT 0)) connlst in
    let dpins = Array.make (List.length connlst) None in
    let cpins = Array.make (List.length connlst) None in
    List.iter (function
      | DOUBLE (ID "Z", ID id) -> let (hi,lo) = Hashtbl.find itemhash id in fprintf fd "reg\t[%d:%d] %s;\n" hi lo inst
      | DOUBLE (ID "Z", DOUBLE (ID _, INT ix)) -> fprintf fd "reg\t%s;\n" inst
      | DOUBLE (ID "Z", TRIPLE (ID _, INT hi, INT lo)) -> fprintf fd "reg\t[%d:0] %s;\n" (hi-lo+1) inst
      | DOUBLE (ID "Z", TLIST lst) -> fprintf fd "reg\t[%d:0] %s;\n" ((List.length lst)-1) inst
      | DOUBLE (ID id, exp) when String.length id > 4 && String.sub id 0 4 = "DATA" ->
         let ix = int_of_string (String.sub id 4 (String.length id - 4)) in dpins.(ix) <- Some exp
      | DOUBLE (ID id, exp) when String.length id > 7 && String.sub id 0 7 = "CONTROL" ->
         let ix = int_of_string (String.sub id 7 (String.length id - 7)) in cpins.(ix) <- Some exp
      | oth -> unrecog := oth :: !unrecog) connlst;
    let dpins = List.filter (function Some _ -> true | None -> false) (Array.to_list dpins) in
    let dpins = List.map (function Some itm -> itm | _ -> INT 0) dpins in
    let cpins = List.filter (function Some _ -> true | None -> false) (Array.to_list cpins) in
    let cpins = List.map (function Some itm -> itm | _ -> INT 0) cpins in
    let conn = List.assoc "Z" connlst' in
    fprintf fd "assign "; expr fd conn; fprintf fd " = %s;\n" inst;
    let inputexp = cpins @ dpins in
    fprintf fd "always %s " (always_kind inputexp); 
    let delim = ref "" in List.iter2 (fun cpin dpin ->
    fprintf fd "%s if (" !delim;
    expr fd cpin;
    fprintf fd ")\n";
    fprintf fd "%s = " inst;
    expr fd dpin;
    fprintf fd ";\n";
    delim := "else") cpins dpins;
    fprintf fd "else %s = 'b0;\n" inst
  | "\\**SEQGEN** ",_ ->
    begin
    let connlst' = List.map (function DOUBLE(ID id, e) -> (id,e) | _ -> ("",INT 0)) connlst in
    fprintf fd "reg\t%s;\n" inst;
    fprintf fd "assign "; rtl_inv fd inst "Q" connlst'; fprintf fd " = %s;\n" inst;
    let flipflop = rtl_active "clocked_on" connlst' in
    let pre = rtl_active "preset" connlst' in
    let clr = rtl_active "clear" connlst' in
    if flipflop then
        begin
        fprintf fd "always @(posedge "; 
        rtl_inv fd inst "clocked_on" connlst';
        if pre then
            begin
            fprintf fd " or posedge ";
            rtl_inv fd inst "preset" connlst';
            end;
        if clr then
            begin
            fprintf fd " or posedge ";
            rtl_inv fd inst "clear" connlst';
            end;
        fprintf fd ")\n    ";
        end
    else
        begin
        fprintf fd "always @("; 
        rtl_inv fd inst "enable" connlst';
        fprintf fd ")\n    ";
        end;
    let delim = ref "" in
    if pre then
        begin
        fprintf fd "%s if (" !delim;
        rtl_inv fd inst "preset" connlst';
        fprintf fd ") %s <= 1'b0;" inst;
        delim := " else";
        end;
    if clr then
        begin
        fprintf fd "%s if (" !delim;
        rtl_inv fd inst "clear" connlst';
        fprintf fd ") %s <= 1'b0;" inst;
        delim := " else";
        end;
    let ce = rtl_active "synch_enable" connlst' in
    if ce then
        begin
        fprintf fd "%s if (" !delim;
        rtl_inv fd inst "synch_enable" connlst';
        fprintf fd ")";
        delim := "";
        end;
    fprintf fd "%s %s <= " !delim inst;
    rtl_inv fd inst "next_state" connlst';
    fprintf fd "; // %B %B %B %B\n" flipflop pre clr ce
    end
  | "GND",_ -> rtl_pwr fd inst connlst "1'b0" "G";
  | "VCC",_ -> rtl_pwr fd inst connlst "1'b1" "P";
  | "RTL_BSEL",_ -> rtl_bsel fd inst connlst kind;
  | "GTECH_AND",_ -> rtl_op fd inst connlst "&";
  | "GTECH_OR",_ -> rtl_op fd inst connlst "|";
  | "GTECH_XOR",_ -> rtl_op fd inst connlst "^";
  | "ADD_UNS_OP",_ -> rtl_op_uns fd inst connlst "+";
  | "SUB_UNS_OP",_ -> rtl_op_uns fd inst connlst "-";
  | "MULT_UNS_OP",_ -> rtl_op_uns fd inst connlst "*";
  | "DIV_UNS_OP",_ -> rtl_op_uns fd inst connlst "/";
  | "REM_UNS_OP",_ -> rtl_op_uns fd inst connlst "%";
  | "ADD_TC_OP",_ -> rtl_op_tc fd inst connlst "+";
  | "SUB_TC_OP",_ -> rtl_op_tc fd inst connlst "-";
  | "MULT_TC_OP",_ -> rtl_op_tc fd inst connlst "*";
  | "DIV_TC_OP",_ -> rtl_op_tc fd inst connlst "/";
  | "REM_TC_OP",_ -> rtl_op_tc fd inst connlst "%";
  | "EQ_UNS_OP",_ -> rtl_op_uns fd inst connlst "==";
  | "NE_UNS_OP",_ -> rtl_op_uns fd inst connlst "!=";
  | "LEQ_UNS_OP",_ -> rtl_op_uns fd inst connlst "<=";
  | "LT_UNS_OP",_ -> rtl_op_uns fd inst connlst "<";
  | "GEQ_UNS_OP",_ -> rtl_op_uns fd inst connlst ">=";
  | "GT_UNS_OP",_ -> rtl_op_uns fd inst connlst ">";
  | "EQ_TC_OP",_ -> rtl_op_tc fd inst connlst "==";
  | "NE_TC_OP",_ -> rtl_op_tc fd inst connlst "!=";
  | "LEQ_TC_OP",_ -> rtl_op_tc fd inst connlst "<=";
  | "LT_TC_OP",_ -> rtl_op_tc fd inst connlst "<";
  | "GEQ_TC_OP",_ -> rtl_op_tc fd inst connlst ">=";
  | "GT_TC_OP",_ -> rtl_op_tc fd inst connlst ">";
  | "ASHR_TC_UNS_OP",_ -> rtl_op_tc_sh fd inst connlst ">>";
  | "ASHR_UNS_UNS_OP",_ -> rtl_op_uns_sh fd inst connlst ">>";
  | "ASH_TC_UNS_OP",_ -> rtl_op_tc_sh fd inst connlst ">>";
  | "ASH_UNS_UNS_OP",_ -> rtl_op_uns_sh fd inst connlst ">>";
  | "GTECH_NOT",_ -> rtl_unary fd inst connlst "~";
  | "GTECH_BUF",_ -> rtl_unary fd inst connlst "";
  | "RTL_REDUCTION_AND",_ -> rtl_unary fd inst connlst "&";
  | "RTL_REDUCTION_OR",_ -> rtl_unary fd inst connlst "|";
  | "RTL_REDUCTION_XOR",_ -> rtl_unary fd inst connlst "^";
  | "INV",_ -> fprintf fd  "initial $stop; // 777\n"
  | uniq_kind,_ -> if !verbose then print_endline uniq_kind;
    let actparm = actparm' in
    fprintf fd "%s%s " kind (elab modhash path liblst kind);
    let delim = ref "#(" in List.iter (function
               | DOUBLE (ID nam, TRIPLE (INT w, QUOTE, ID value)) ->
                   fprintf fd "%s.%s(%d'%s)" !delim nam w value; delim := ",\n"
               | DOUBLE (ID nam, STRING s) ->
                   fprintf fd "%s.%s(\"%s\")" !delim nam (unquote s);
                   delim := ",\n"
               | DOUBLE (ID nam, INT n) ->
                   fprintf fd "%s.%s(%d)" !delim nam n;
                   delim := ",\n"
               | DOUBLE (ID nam, FLT f) ->
                   fprintf fd "%s.%s(%f)" !delim nam f;
                   delim := ",\n"
               | oth -> generic_othitm := oth :: !generic_othitm; failwith "actparm") actparm;
    if actparm <> [] then fprintf fd ")\n";
    fprintf fd "%s\n" inst;
    let connlst' = List.map (function DOUBLE(ID id, e) -> (id,e) | _ -> ("",INT 0)) connlst in
    if Hashtbl.mem cellhash kind then
        begin
        let plist = Hashtbl.find cellhash kind in
        let delim = ref "(" in List.iter (fun (pnam, (dir, wid)) ->
            let connected = List.mem_assoc pnam connlst' in
            if connected then
                begin
                fprintf fd "%s.%s(" !delim pnam; rtl_inv fd inst pnam connlst'; fprintf fd ")";
                end
            else
                begin
                let w = match wid with Some w -> w | None -> 1 in 
                fprintf fd "%s.%s(%s)" !delim pnam (match dir with Input -> string_of_int w^"'b0" | _ -> "");
                end;
            delim := ",\n";
          ) plist;
        end
    else
        begin
        let delim = ref "(" in
        List.iter (function
            | DOUBLE(ID pnam, conn) ->
            fprintf fd "%s.%s(" !delim pnam; expr fd conn; fprintf fd ")";
            delim := ",\n";
            | DOUBLE(kw, conn) ->
            fprintf fd "%s.%s(" !delim (str_token kw); expr fd conn; fprintf fd ")";
            delim := ",\n"
            | oth ->
            fprintf fd "%s.%s(" !delim (str_token oth); expr fd oth; fprintf fd ")";
            delim := ",\n"
          ) connlst;
        if !delim = "(" then fprintf fd "( ";
        end;
    fprintf fd ");\n")
| DOUBLE(ID nam, STRING value') ->
    begin
    let value = unquote value' in
    match nam with
      | "XLNX_LINE_COL" | "XILINX_REPORT_XFORM" | "BOX_TYPE" | "map_to_module" -> ()
      | _ -> fprintf fd "/* %s = \"%s\" */\n" nam value
    end
| DOUBLE(ID nam, INT value) -> fprintf fd "/* %s = %d */\n" nam value
| ID nam -> fprintf fd "/* %s */\n" nam
| DOUBLE(a,b) ->
    fprintf fd "DOUBLE(";
    expr fd a;
    fprintf fd ",";
    expr fd b; 
    fprintf fd ")\n";
| TRIPLE(a,b,c) ->
    fprintf fd "TRIPLE(";
    expr fd a;
    fprintf fd ",";
    expr fd b;
    fprintf fd ",";
    expr fd c; 
    fprintf fd ")\n";
| oth -> othitm := oth :: !othitm; fprintf fd "/* %s */\n" (Eord.getstr oth)

let modules modhash path liblst fd = function
| QUADRUPLE(Module, ID modid, TLIST plst, TLIST itmlst) -> if not (List.mem modid (snd liblst)) then (
    fprintf fd "module %s%s " modid (elab modhash path liblst modid);
    let delim = ref "(" in List.iter (function
               | ID s -> fprintf fd "%s%s" !delim s; delim := ",\n"
               | DOUBLE(ID s, e) -> fprintf fd "%s.%s(" !delim s; expr fd e; fprintf fd ")"; delim := ",\n"
               | oth -> fprintf fd "%s%s" !delim (str_token oth); delim := ",\n") plst;
               fprintf fd ");\n\n";
    let itemhash = Hashtbl.create 255 in
    List.iter (items modhash (path^"."^modid) itemhash liblst fd) itmlst;
    fprintf fd "endmodule // %s%s\n" modid (elab modhash path liblst modid))

| COMMENT c -> fprintf fd "%s\n" c
| MACRO m -> fprintf fd "%s\n" m
| DOUBLE(BEGINPRAGMA, prag) -> fprintf fd "(* "; pragma fd prag; fprintf fd " *)\n"; 
| oth -> failwith "modules"

let nomatch = ref None

let rec is_reg ix inst =
  let len = String.length inst in
  if len >= 4+ix then
      begin
      if String.sub inst ix 4 = "_reg" && ((len = 4+ix) || inst.[ix+4] = '[' || inst.[ix+4] = ' ') then true
      else is_reg (ix+1) inst
      end
   else
      inst = "CDN_flop"

let rec moditer dbglst modhash liblst pathlst path modid itmlst =
   match modid with
     | "CDN_flop" -> pathlst := (path,"q") :: !pathlst
     | "CDN_latch" -> pathlst := (path,"q") :: !pathlst
     | _ ->
   List.iter (function
     | QUADRUPLE(ID kind, _, ID inst, _) -> let path' = (if path <> "" then path^"."^inst else inst) in
        if Hashtbl.mem modhash kind then moditer dbglst modhash liblst pathlst path' kind (Hashtbl.find modhash kind) else
        if is_reg 0 inst then pathlst := (path',"Q") :: !pathlst;
        dbglst := (path',kind) :: !dbglst;
     | DOUBLE((Input|Output|Inout|Wire|Reg), TLIST lst) -> ()
     | TRIPLE(Assign, _, _) -> ()
     | TRIPLE((Nor|Not|Nand|And|Or|Xor|Xnor), _, _) -> ()
     | oth -> nomatch := Some oth; failwith modid) itmlst

let munge_copy str =
  let ix' = ref 0 in
  let str' = Bytes.of_string str in
  String.iter (function '\\' -> () | ' ' -> () | '.' -> Bytes.set str' !ix' '_'; incr ix' | ch -> Bytes.set str' !ix' ch; incr ix') str;
  Bytes.to_string (Bytes.sub str' 0 !ix')

let match_hash = Hashtbl.create 256
let oth_arg = ref None

let eargs args =
  List.iter ( fun arg ->
  match eparse arg with
   | TLIST vlst ->
    let mlst = List.filter (function QUADRUPLE(Module, _, _, _) -> true | _ -> false) vlst in
    print_endline (string_of_int (List.length mlst)^" modules parsed");
    let tmphash = Hashtbl.create 255 in
    let modhash = Hashtbl.create 255 in
    let pathlst = ref [] in
    let dbglst = ref [] in
    List.iter (function QUADRUPLE(Module, ID modid, TLIST plst, TLIST itmlst) -> Hashtbl.add modhash modid itmlst; Hashtbl.add tmphash modid () | _ -> ()) vlst;
    Hashtbl.iter (fun modid itmlst -> List.iter (function QUADRUPLE(ID kind, _, ID _, _) -> Hashtbl.remove tmphash kind | _ -> ()) itmlst) modhash;
    Hashtbl.iter (fun topmodid _ -> let topmod = Hashtbl.find modhash topmodid in moditer dbglst modhash ([],[]) pathlst "" topmodid topmod) tmphash;
    let munged = List.map (fun (n,k) -> (munge_copy n, n, k)) !pathlst in
    let fd = open_out (arg^"_dbg") in
    List.iter (fun (p,k) -> fprintf fd "// \"%s\", %s\n" p k) !dbglst;
    close_out fd;
    let fd = open_out (arg^"_dump.v") in
    List.iter (fun (n,n',k) -> fprintf fd "%s.%s // \"%s\"\n" n k n') (List.sort compare munged);
    List.iter (fun (n,n',k) ->
       if Hashtbl.mem match_hash n then
          Hashtbl.replace match_hash n (Hashtbl.find match_hash n @ [(n',k)])
       else 
          Hashtbl.add match_hash n [(n',k)]) munged;
    if true then
       begin
       let fd = open_out (arg^"_compare.v") in
       List.iter (fun (n,k) -> fprintf fd "\"%s\"\n" n) (List.sort compare !pathlst);
       end;
    close_out fd
   | oth -> oth_arg := Some oth; failwith "eparse"
  ) args;
  let mitre = ref [] in
  Hashtbl.iter (fun k -> function
    | itm :: [] -> ()
    | lft :: rght :: [] -> mitre := (k,lft,rght) :: !mitre
    | _ -> failwith "match_hash"
    ) match_hash;
  let fd = ref stdout in
  List.iteri (fun ix (k,(lft,lpin),(rght,rpin)) ->
     if ix mod 1000 = 0 then
        begin
        if ix > 0 then close_out !fd;
        let fnam = "mitre"^string_of_int (ix/1000)^".v" in
        print_endline ("`include \""^fnam^"\"");
        fd := open_out fnam;
        end;
     fprintf !fd "wire \\%s = `PATH1.%s.%s !== `PATH2.%s.%s;\n" k lft lpin rght rpin) (List.sort compare !mitre);
  close_out !fd
  
