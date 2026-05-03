(*
 MIT License

Copyright (c) 2024 Jonathan Kimmitt

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*)

%{
  
let getstr = function
| FINISHED -> "FINISHED";
| INT -> "INT";
| KILLED -> "KILLED";
| Post_Elab -> "Post-Elab";
| Pre_Elab -> "Pre-Elab";
| RUNNING -> "RUNNING";
| Restored -> "Restored";
| SUSPENDED -> "SUSPENDED";
| UINT -> "UINT";
| WAITING -> "WAITING";
| Always -> "_always";
| Assignment -> "_assignment";
| Begin -> "_begin";
| Class_defn -> "_class_defn";
| Class_typespec -> "_class_typespec";
| Class_var -> "_class_var";
| Constant -> "_constant";
| Design -> "_design";
| Enum_const -> "_enum_const";
| Enum_typespec -> "_enum_typespec";
| Enum_var -> "_enum_var";
| Event_control -> "_event_control";
| Function -> "_function";
| Int_typespec -> "_int_typespec";
| Int_var -> "_int_var";
| Io_decl -> "_io_decl";
| Logic_net -> "_logic_net";
| Logic_typespec -> "_logic_typespec";
| Logic_var -> "_logic_var";
| Module_inst -> "_module_inst";
| Operation -> "_operation";
| Package -> "_package";
| Port -> "_port";
| Range -> "_range";
| Ref_obj -> "_ref_obj";
| Ref_typespec -> "_ref_typespec";
| Task -> "_task";
| Var_select -> "_var_select";
| Weaklyreferenced -> "_weaklyReferenced";
| Any_sverilog_class -> "any_sverilog_class";
| Array -> "array";
| Await -> "await";
| Bound -> "bound";
| Builtin -> "builtin";
| Clk -> "clk";
| DESIGN -> "DESIGN";
| Endln -> "endln";
| File -> "file";
| Get -> "get";
| Keycount -> "keyCount";
| Kill -> "kill";
| Line -> "line";
| Mailbox -> "mailbox";
| Message -> "message";
| New -> "new";
| Num -> "num";
| Peek -> "peek";
| Process -> "process";
| Put -> "put";
| Queue -> "queue";
| Resume -> "resume";
| Self -> "self";
| Semaphore -> "semaphore";
| State -> "state";
| Status -> "status";
| String -> "string";
| Suspend -> "suspend";
| System -> "system";
| Try_get -> "try_get";
| Try_peek -> "try_peek";
| Try_put -> "try_put";
| Uhdmallclasses -> "uhdmallClasses";
| Uhdmallmodules -> "uhdmallModules";
| Uhdmallpackages -> "uhdmallPackages";
| Uhdmtopmodules -> "uhdmtopModules";
| Uhdmtoppackages -> "uhdmtopPackages";
| Vpiactual -> "vpiActual";
| Vpialwaystype -> "vpiAlwaysType";
| Vpiclassdefn -> "vpiClassDefn";
| Vpicondition -> "vpiCondition";
| Vpiconsttype -> "vpiConstType";
| Vpidecompile _ -> "vpiDecompile";
| Vpidefname -> "vpiDefName";
| Vpidirection -> "vpiDirection";
| Vpielaborated -> "vpiElaborated";
| Vpienumconst -> "vpiEnumConst";
| Vpiexpr -> "vpiExpr";
| Vpifullname -> "vpiFullName";
| Vpiiodecl -> "vpiIODecl";
| Vpiindex -> "vpiIndex";
| Vpiinstance -> "vpiInstance";
| Vpileftrange -> "vpiLeftRange";
| Vpilhs -> "vpiLhs";
| Vpilowconn -> "vpiLowConn";
| Vpimethod -> "vpiMethod";
| Vpiname -> "vpiName";
| Vpinet -> "vpiNet";
| Vpinettype -> "vpiNetType";
| Vpioptype -> "vpiOpType";
| Vpioperand -> "vpiOperand";
| Vpiparent -> "vpiParent";
| Vpiport -> "vpiPort";
| Vpiprocess -> "vpiProcess";
| Vpirange -> "vpiRange";
| Vpireturn -> "vpiReturn";
| Vpirhs -> "vpiRhs";
| Vpirightrange -> "vpiRightRange";
| Vpisigned -> "vpiSigned";
| Vpisize -> "vpiSize";
| Vpistmt -> "vpiStmt";
| Vpitop -> "vpiTop";
| Vpitopmodule -> "vpiTopModule";
| Vpitypedef -> "vpiTypedef";
| Vpitypespec -> "vpiTypespec";
| Vpibasetypespec -> "vpiBaseTypespec";
| Vpivariables -> "vpiVariables";
| Vpivisibility -> "vpiVisibility";
| Work -> "work";
| STRING s -> s
| oth -> "UNKNOWN"

(******************************** OBJECT TYPES ********************************)

let vpi_obj = function
| 1 -> Vpialways (* always procedure *)
| 2 -> Vpiassignstmt (* quasi-continuous assignment *)
| 3 -> Vpiassignment (* procedural assignment *)
| 4 -> Vpibegin (* block statement *)
| 5 -> Vpicase (* case statement *)
| 6 -> Vpicaseitem (* case statement item *)
| 7 -> Vpiconstant (* numerical constant or string literal *)
| 8 -> Vpicontassign (* continuous assignment *)
| 9 -> Vpideassign (* deassignment statement *)
| 10 -> Vpidefparam (* defparam *)
| 11 -> Vpidelaycontrol (* delay statement (e.g., #10) *)
| 12 -> Vpidisable (* named block disable statement *)
| 13 -> Vpieventcontrol (* wait on event, e.g., @e *)
| 14 -> Vpieventstmt (* event trigger, e.g., ->e *)
| 15 -> Vpifor (* for statement *)
| 16 -> Vpiforce (* force statement *)
| 17 -> Vpiforever (* forever statement *)
| 18 -> Vpifork (* fork-join block *)
| 19 -> Vpifunccall (* function call *)
| 20 -> Vpifunction (* function *)
| 21 -> Vpigate (* primitive gate *)
| 22 -> Vpiif (* if statement *)
| 23 -> Vpiifelse (* if-else statement *)
| 24 -> Vpiinitial (* initial procedure *)
| 25 -> Vpiintegervar (* integer variable *)
| 26 -> Vpiintermodpath (* intermodule wire delay *)
| 27 -> Vpiiterator (* iterator *)
| 28 -> Vpiiodecl (* input/output declaration *)
| 29 -> Vpimemory (* behavioral memory *)
| 30 -> Vpimemoryword (* single word of memory *)
| 31 -> Vpimodpath (* module path for path delays *)
| 32 -> Vpimodule (* module instance *)
| 33 -> Vpinamedbegin (* named block statement *)
| 34 -> Vpinamedevent (* event variable *)
| 35 -> Vpinamedfork (* named fork-join block *)
| 36 -> Vpinet (* scalar or vector net *)
| 37 -> Vpinetbit (* bit of vector net *)
| 38 -> Vpinullstmt (* a semicolon. Ie. #10 ; *)
| 39 -> Vpioperation (* behavioral operation *)
| 40 -> Vpiparamassign (* module parameter assignment *)
| 41 -> Vpiparameter (* module parameter *)
| 42 -> Vpipartselect (* part-select *)
| 43 -> Vpipathterm (* terminal of module path *)
| 44 -> Vpiport (* module port *)
| 45 -> Vpiportbit (* bit of vector module port *)
| 46 -> Vpiprimterm (* primitive terminal *)
| 47 -> Vpirealvar (* real variable *)
| 48 -> Vpireg (* scalar or vector reg *)
| 49 -> Vpiregbit (* bit of vector reg *)
| 50 -> Vpirelease (* release statement *)
| 51 -> Vpirepeat (* repeat statement *)
| 52 -> Vpirepeatcontrol (* repeat control in an assign stmt *)
| 53 -> Vpischedevent (* vpi_put_value() event *)
| 54 -> Vpispecparam (* specparam *)
| 55 -> Vpiswitch (* transistor switch *)
| 56 -> Vpisysfunccall (* system function call *)
| 57 -> Vpisystaskcall (* system task call *)
| 58 -> Vpitableentry (* UDP state table entry *)
| 59 -> Vpitask (* task *)
| 60 -> Vpitaskcall (* task call *)
| 61 -> Vpitchk (* timing check *)
| 62 -> Vpitchkterm (* terminal of timing check *)
| 63 -> Vpitimevar (* time variable *)
| 64 -> Vpitimequeue (* simulation event queue *)
| 65 -> Vpiudp (* user-defined primitive *)
| 66 -> Vpiudpdefn (* UDP definition *)
| 67 -> Vpiusersystf (* user-defined system task/function *)
| 68 -> Vpivarselect (* variable array selection *)
| 69 -> Vpiwait (* wait statement *)
| 70 -> Vpiwhile (* while statement *)

(********************** object types added with 1364-2001 *********************)

| 105 -> Vpiattribute (* attribute of an object *)
| 106 -> Vpibitselect (* Bit-select of parameter, var select *)
| 107 -> Vpicallback (* callback object *)
| 108 -> Vpidelayterm (* Delay term which is a load or driver *)
| 109 -> Vpidelaydevice (* Delay object within a net *)
| 110 -> Vpiframe (* reentrant task/func frame *)
| 111 -> Vpigatearray (* gate instance array *)
| 112 -> Vpimodulearray (* module instance array *)
| 113 -> Vpiprimitivearray (* vpiprimitiveArray type *)
| 114 -> Vpinetarray (* multidimensional net *)
| 115 -> Vpirange (* range declaration *)
| 116 -> Vpiregarray (* multidimensional reg *)
| 117 -> Vpiswitcharray (* switch instance array *)
| 118 -> Vpiudparray (* UDP instance array *)
| 128 -> Vpicontassignbit (* Bit of a vector continuous assignment *)
| 129 -> Vpinamedeventarray (* multidimensional named event *)

(********************** object types added with 1364-2005 *********************)

| 130 -> Vpiindexedpartselect (* Indexed part-select object *)
| 133 -> Vpigenscopearray (* array of generated scopes *)
| 134 -> Vpigenscope (* A generated scope *)
| 135 -> Vpigenvar (* Object used to instantiate gen scopes *)
| oth -> STRING ("vpi_obj_"^string_of_int oth)

(*********************************** METHODS **********************************)
(**************** methods used to traverse 1 to 1 relationships ***************)

let vpi_meth = function
| 71 -> Vpicondition (* condition expression *)
| 72 -> Vpidelay (* net or gate delay *)
| 73 -> Vpielsestmt (* else statement *)
| 74 -> Vpiforincstmt (* increment statement in for loop *)
| 75 -> Vpiforinitstmt (* initialization statement in for loop *)
| 76 -> Vpihighconn (* higher connection to port *)
| 77 -> Vpilhs (* left-hand side of assignment *)
| 78 -> Vpiindex (* index of var select, bit-select, etc. *)
| 79 -> Vpileftrange (* left range of vector or part-select *)
| 80 -> Vpilowconn (* lower connection to port *)
| 81 -> Vpiparent (* parent object *)
| 82 -> Vpirhs (* right-hand side of assignment *)
| 83 -> Vpirightrange (* right range of vector or part-select *)
| 84 -> Vpiscope (* containing scope object *)
| 85 -> Vpisystfcall (* task function call *)
| 86 -> Vpitchkdataterm (* timing check data term *)
| 87 -> Vpitchknotifier (* timing check notifier *)
| 88 -> Vpitchkrefterm (* timing check reference term *)

(************* methods used to traverse 1 to many relationships ***************)

| 89 -> Vpiargument (* argument to (system) task/function *)
| 90 -> Vpibit (* bit of vector net or port *)
| 91 -> Vpidriver (* driver for a net *)
| 92 -> Vpiinternalscope (* internal scope in module *)
| 93 -> Vpiload (* load on net or reg *)
| 94 -> Vpimoddatapathin (* data terminal of a module path *)
| 95 -> Vpimodpathin (* Input terminal of a module path *)
| 96 -> Vpimodpathout (* output terminal of a module path *)
| 97 -> Vpioperand (* operand of expression *)
| 98 -> Vpiportinst (* connected port instance *)
| 99 -> Vpiprocess (* process in module, program or interface *)
| 100 -> Vpivariables (* variables in module *)
| 101 -> Vpiuse (* usage *)

(******** methods which can traverse 1 to 1, or 1 to many relationships *******)

| 102 -> Vpiexpr (* connected expression *)
| 103 -> Vpiprimitive (* primitive (gate, switch, UDP) *)
| 104 -> Vpistmt (* statement in process or task *)

(************************ methods added with 1364-2001 ************************)

| 119 -> Vpiactivetimeformat (* active $timeformat() system task *)
| 120 -> Vpiinterm (* To get to a delay device's drivers. *)
| 121 -> Vpiinstancearray (* vpiInstance arrays *)
| 122 -> Vpilocaldriver (* local drivers (within a module *)
| 123 -> Vpilocalload (* local loads (within a module *)
| 124 -> Vpioutterm (* To get to a delay device's loads. *)
| 125 -> Vpiports (* Module port *)
| 126 -> Vpisimnet (* simulated net after collapsing *)
| 127 -> Vpitaskfunc (* task/function *)

(************************ methods added with 1364-2005 ************************)

| 131 -> Vpibaseexpr (* Indexed part-select's base expression *)
| 132 -> Vpiwidthexpr (* Indexed part-select's width expression *)

(************************ methods added with 1800-2009 ************************)

| 136 -> Vpiautomatics (* Automatic variables of a frame *)
| oth -> STRING ("vpi_meth_"^string_of_int oth)

(********************************* PROPERTIES *********************************)
(************************** generic object properties *************************)

let vpi_prop = function
| -1 -> Vpiundefined (* undefined property *)
| 1 -> Vpitype (* type of object *)
| 2 -> Vpiname (* local name of object *)
| 3 -> Vpifullname (* full hierarchical name *)
| 4 -> Vpisize (* size of gate, net, port, etc. *)
| 5 -> Vpifile (* File name in which the object is used*)
| 6 -> Vpilineno (* line number where the object is used *)

(***************************** module properties ******************************)

| 7 -> Vpitopmodule (* top-level module (Boolean) *)
| 8 -> Vpicellinstance (* cell (Boolean) *)
| 9 -> Vpidefname (* module definition name *)
| 10 -> Vpiprotected (* source protected module (Boolean) *)
| 11 -> Vpitimeunit (* module time unit *)
| 12 -> Vpitimeprecision (* module time precision *)
| 13 -> Vpidefnettype (* default net type *)
| 14 -> Vpiunconndrive (* unconnected port drive strength *)
| 15 -> Vpideffile (* File name where the module is defined*)
| 16 -> Vpideflineno (* line number for module definition *)
| 47 -> Vpidefdelaymode (* Default delay mode for a module *)
| 48 -> Vpidefdecaytime (* Default decay time for a module *)
| oth -> STRING ("vpi_prop_"^string_of_int oth)

let delay_mode = function
| 1 -> Vpidelaymodenone (* no delay mode specified *)
| 2 -> Vpidelaymodepath (* path delay mode *)
| 3 -> Vpidelaymodedistrib (* distributed delay mode *)
| 4 -> Vpidelaymodeunit (* unit delay mode *)
| 5 -> Vpidelaymodezero (* zero delay mode *)
| 6 -> Vpidelaymodemtm (* min:typ:max delay mode *)
| oth -> STRING ("delay_mode_"^string_of_int oth)

let drive_strength = function
| 1 -> Vpihighz (* No default drive given *)
| 2 -> Vpipull1 (* default pull1 drive *)
| 3 -> Vpipull0 (* default pull0 drive *)
| oth -> STRING ("drive_strength_"^string_of_int oth)

(*************************** port and net properties **************************)

let net_subtype = function
| 1 -> Vpiwire (* wire net *)
| 2 -> Vpiwand (* wire-and net *)
| 3 -> Vpiwor (* wire-or net *)
| 4 -> Vpitri (* tri net *)
| 5 -> Vpitri0 (* pull-down net *)
| 6 -> Vpitri1 (* pull-up net *)
| 7 -> Vpitrireg (* three-state reg net *)
| 8 -> Vpitriand (* three-state wire-and net *)
| 9 -> Vpitrior (* three-state wire-or net *)
| 10 -> Vpisupply1 (* supply-1 net *)
| 11 -> Vpisupply0 (* supply-0 net *)
| 12 -> Vpinone (* no default net type (1364-2001) *)
| 13 -> Vpiuwire (* unresolved wire net (1364-2005) *)
| oth -> STRING ("net_subtype_"^string_of_int oth)

let vpi_port_net = function
| 17 -> Vpiscalar (* scalar (Boolean) *)
| 18 -> Vpivector (* vector (Boolean) *)
| 19 -> Vpiexplicitname (* port is explicitly named *)
| 20 -> Vpidirection (* direction of port: *)
| 1 -> Vpiinput (* input *)
| 2 -> Vpioutput (* output *)
| 3 -> Vpiinout (* inout *)
| 4 -> Vpimixedio (* mixed input-output *)
| 5 -> Vpinodirection (* no direction *)
| 21 -> Vpiconnbyname (* connected by name (Boolean) *)
| 22 -> Vpinettype (* net subtypes: *)
| 23 -> Vpiexplicitscalared (* explicitly scalared (Boolean) *)
| 24 -> Vpiexplicitvectored (* explicitly vectored (Boolean) *)
| 25 -> Vpiexpanded (* expanded vector net (Boolean) *)
| 26 -> Vpiimplicitdecl (* implicitly declared net (Boolean) *)
| 27 -> Vpichargestrength (* charge decay strength of net *)

(*************************** port and net properties **************************)

|  28 -> Vpiarray (* variable array (Boolean) *)
|  29 -> Vpiportindex (* Port index *)
| oth -> STRING ("vpi_port_net_"^string_of_int oth)

let vpi_net_subtype = function
|  1 -> Vpiwire (* wire net *)
|  2 -> Vpiwand (* wire-and net *)
|  3 -> Vpiwor (* wire-or net *)
|  4 -> Vpitri (* tri net *)
|  5 -> Vpitri0 (* pull-down net *)
|  6 -> Vpitri1 (* pull-up net *)
|  7 -> Vpitrireg (* three-state reg net *)
|  8 -> Vpitriand (* three-state wire-and net *)
|  9 -> Vpitrior (* three-state wire-or net *)
|  10 -> Vpisupply1 (* supply-1 net *)
|  11 -> Vpisupply0 (* supply-0 net *)
|  12 -> Vpinone (* no default net type (1364-2001) *)
|  13 -> Vpiuwire (* unresolved wire net (1364-2005) *)
| oth -> STRING ("vpi_net_subtype_"^string_of_int oth)

(************************ gate and terminal properties ************************)

let vpi_gate_term = function
|  30 -> Vpitermindex (* Index of a primitive terminal *)
|  31 -> Vpistrength0 (* 0-strength of net or gate *)
|  32 -> Vpistrength1 (* 1-strength of net or gate *)
|  33 -> Vpiprimtype (* primitive subtypes: *)
|  1 -> Vpiandprim (* and gate *)
|  2 -> Vpinandprim (* nand gate *)
|  3 -> Vpinorprim (* nor gate *)
|  4 -> Vpiorprim (* or gate *)
|  5 -> Vpixorprim (* xor gate *)
|  6 -> Vpixnorprim (* xnor gate *)
|  7 -> Vpibufprim (* buffer *)
|  8 -> Vpinotprim (* not gate *)
|  9 -> Vpibufif0prim (* zero-enabled buffer *)
|  10 -> Vpibufif1prim (* one-enabled buffer *)
|  11 -> Vpinotif0prim (* zero-enabled not gate *)
|  12 -> Vpinotif1prim (* one-enabled not gate *)
|  13 -> Vpinmosprim (* nmos switch *)
|  14 -> Vpipmosprim (* pmos switch *)
|  15 -> Vpicmosprim (* cmos switch *)
|  16 -> Vpirnmosprim (* resistive nmos switch *)
|  17 -> Vpirpmosprim (* resistive pmos switch *)
|  18 -> Vpircmosprim (* resistive cmos switch *)
|  19 -> Vpirtranprim (* resistive bidirectional *)
|  20 -> Vpirtranif0prim (* zero-enable resistive bidirectional *)
|  21 -> Vpirtranif1prim (* one-enable resistive bidirectional *)
|  22 -> Vpitranprim (* bidirectional *)
|  23 -> Vpitranif0prim (* zero-enabled bidirectional *)
|  24 -> Vpitranif1prim (* one-enabled bidirectional *)
|  25 -> Vpipullupprim (* pullup *)
|  26 -> Vpipulldownprim (* pulldown *)
|  27 -> Vpiseqprim (* sequential UDP *)
|  28 -> Vpicombprim (* combinational UDP *)
| oth -> STRING ("vpi_gate_term_"^string_of_int oth)

(**************** path, path terminal, timing check properties ****************)

let vpi_path = function
|  34 -> Vpipolarity (* polarity of module path... *)
|  35 -> Vpidatapolarity (* ...or data path: *)
|  36 -> Vpiedge (* edge type of module path: *)
|  37 -> Vpiposedge (* path delay connection subtypes: *)
|  38 -> Vpitchktype (* timing check subtypes: *)
|  39 -> Vpioptype (* operation subtypes: *)
| oth -> STRING ("vpi_path_"^string_of_int oth)

let vpi_polarity = function
|  1 -> Vpipositive (* positive *)
|  2 -> Vpinegative (* negative *)
|  3 -> Vpiunknown (* unknown (unspecified) *)
| oth -> STRING ("vpi_polarity_"^string_of_int oth)

let vpi_delay = function
|  1 -> Vpipathfull (* ( a *> b ) *)
|  2 -> Vpipathparallel (* ( a => b ) *)
| oth -> STRING ("vpi_delay_"^string_of_int oth)

let vpi_check = function
|  1 -> Vpisetup (* $setup *)
|  2 -> Vpihold (* $hold *)
|  3 -> Vpiperiod (* $period *)
|  4 -> Vpiwidth (* $width *)
|  5 -> Vpiskew (* $skew *)
|  6 -> Vpirecovery (* $recovery *)
|  7 -> Vpinochange (* $nochange *)
|  8 -> Vpisetuphold (* $setuphold *)
|  9 -> Vpifullskew (* $fullskew -- added for 1364-2001 *)
|  10 -> Vpirecrem (* $recrem -- added for 1364-2001 *)
|  11 -> Vpiremoval (* $removal -- added for 1364-2001 *)
|  12 -> Vpitimeskew (* $timeskew -- added for 1364-2001 *)
| oth -> STRING ("vpi_check_"^string_of_int oth)

(**************************** expression properties ***************************)

let vpi_expr = function
|  1 -> Vpiminusop (* unary minus *)
|  2 -> Vpiplusop (* unary plus *)
|  3 -> Vpinotop (* unary not *)
|  4 -> Vpibitnegop (* bitwise negation *)
|  5 -> Vpiunaryandop (* bitwise reduction AND *)
|  6 -> Vpiunarynandop (* bitwise reduction NAND *)
|  7 -> Vpiunaryorop (* bitwise reduction OR *)
|  8 -> Vpiunarynorop (* bitwise reduction NOR *)
|  9 -> Vpiunaryxorop (* bitwise reduction XOR *)
|  10 -> Vpiunaryxnorop (* bitwise reduction XNOR *)
|  11 -> Vpisubop (* binary subtraction *)
|  12 -> Vpidivop (* binary division *)
|  13 -> Vpimodop (* binary modulus *)
|  14 -> Vpieqop (* binary equality *)
|  15 -> Vpineqop (* binary inequality *)
|  16 -> Vpicaseeqop (* case (x and z) equality *)
|  17 -> Vpicaseneqop (* case inequality *)
|  18 -> Vpigtop (* binary greater than *)
|  19 -> Vpigeop (* binary greater than or equal *)
|  20 -> Vpiltop (* binary less than *)
|  21 -> Vpileop (* binary less than or equal *)
|  22 -> Vpilshiftop (* binary left shift *)
|  23 -> Vpirshiftop (* binary right shift *)
|  24 -> Vpiaddop (* binary addition *)
|  25 -> Vpimultop (* binary multiplication *)
|  26 -> Vpilogandop (* binary logical AND *)
|  27 -> Vpilogorop (* binary logical OR *)
|  28 -> Vpibitandop (* binary bitwise AND *)
|  29 -> Vpibitorop (* binary bitwise OR *)
|  30 -> Vpibitxorop (* binary bitwise XOR *)
|  31 -> Vpibitxnorop (* binary bitwise XNOR *)
|  32 -> Vpiconditionop (* ternary conditional *)
|  33 -> Vpiconcatop (* n-ary concatenation *)
|  34 -> Vpimulticoncatop (* repeated concatenation *)
|  35 -> Vpieventorop (* event OR *)
|  36 -> Vpinullop (* null operation *)
|  37 -> Vpilistop (* list of expressions *)
|  38 -> Vpimintypmaxop (* min:typ:max: delay expression *)
|  39 -> Vpiposedgeop (* posedge *)
|  40 -> Vpinegedgeop (* negedge *)
|  41 -> Vpiarithlshiftop (* arithmetic left shift (1364-2001) *)
|  42 -> Vpiarithrshiftop (* arithmetic right shift (1364-2001) *)
|  43 -> Vpipowerop (* arithmetic power op (1364-2001) *)
| oth -> STRING ("vpi_expr_"^string_of_int oth)

let vpi_misc_typ = function
|  40 -> Vpiconsttype (* constant subtypes: *)
|  41 -> Vpiblocking (* blocking assignment (Boolean) *)
|  42 -> Vpicasetype (* case statement subtypes: *)
|  43 -> Vpinetdeclassign (* assign part of decl (Boolean) *)
| oth -> STRING ("vpi_misc_"^string_of_int oth)

let vpi_case_typ = function
|  1 -> Vpicaseexact (* exact match *)
|  2 -> Vpicasex (* ignore X's *)
|  3 -> Vpicasez (* ignore Z's *)
| oth -> STRING ("vpi_case_typ_"^string_of_int oth)

let vpi_const_typ = function
|  1 -> Vpidecconst (* decimal integer *)
|  2 -> Vpirealconst (* real *)
|  3 -> Vpibinaryconst (* binary integer *)
|  4 -> Vpioctconst (* octal integer *)
|  5 -> Vpihexconst (* hexadecimal integer *)
|  6 -> Vpistringconst (* string literal *)
|  7 -> Vpiintconst (* integer constant (1364-2001) *)
|  8 -> Vpitimeconst (* time constant *)
|  9 -> Vpiuintconst (* unsigned integer constant !!! NOT Standard !!! *)
| oth -> STRING ("vpi_const_typ_"^string_of_int oth)

(************************** task/function properties **************************)

let vpi_task_func = function
|  44 -> Vpifunctype (* function & system function type *)
(*
|  1 -> Vpiintfunc (* returns integer *)
|  2 -> Vpirealfunc (* returns real *)
|  3 -> Vpitimefunc (* returns time *)
|  4 -> Vpisizedfunc (* returns an arbitrary size *)
|  5 -> Vpisizedsignedfunc (* returns sized signed value *)
*)
(** alias 1364-1995 system function subtypes to 1364-2001 function subtypes ***)

|  45 -> Vpiuserdefn (*user-defined system task/func(Boolean)*)
|  46 -> Vpischeduled (* object still scheduled (Boolean) *)

(*********************** properties added with 1364-2001 **********************)

|  49 -> Vpiactive (* reentrant task/func frame is active *)
|  50 -> Vpiautomatic (* task/func obj is automatic *)
|  51 -> Vpicell (* configuration cell *)
|  52 -> Vpiconfig (* configuration config file *)
|  53 -> Vpiconstantselect (* (Boolean) bit-select or part-select indices are constant expressions *)
|  54 -> Vpidecompile "" (* decompile the object *)
|  55 -> Vpidefattribute (* Attribute defined for the obj *)
|  56 -> Vpidelaytype (* delay subtype *)
(*
|  1 -> Vpimodpathdelay (* module path delay *)
|  2 -> Vpiintermodpathdelay (* intermodule path delay *)
|  3 -> Vpimipdelay (* module input port delay *)
*)
|  57 -> Vpiiteratortype (* object type of an iterator *)
|  58 -> Vpilibrary (* configuration library *)
|  60 -> Vpioffset (* offset from LSB *)
|  61 -> Vpiresolvednettype (* net subtype after resolution, returns same subtypes as vpiNetType *)
|  62 -> Vpisaverestartid (* unique ID for save/restart data *)
|  63 -> Vpisaverestartlocation (* name of save/restart data file *)
|  64 -> Vpivalid (* reentrant task/func frame or automatic variable is valid *)
|  70 -> Vpilocalparam (* TRUE when a param is declared as a localparam *)
|  71 -> Vpimodpathhasifnone (* Mod path has an ifnone statement *)

(*********************** properties added with 1364-2005 **********************)

|  72 -> Vpiindexedpartselecttype (* Indexed part-select type *)
(*
|  1 -> Vpiposindexed (* +: *)
|  2 -> Vpinegindexed (* -: *)
*)
|  73 -> Vpiismemory (* TRUE for a one-dimensional reg array *)
|  74 -> Vpiisprotected (* TRUE for protected design information *)
| oth -> STRING ("vpi_task_func_"^string_of_int oth)

(*************** vpi_control() constants (added with 1364-2001) ***************)

let vpi_control = function
|  66 -> Vpistop (* execute simulator's $stop *)
|  67 -> Vpifinish (* execute simulator's $finish *)
|  68 -> Vpireset (* execute simulator's $reset *)
|  69 -> Vpisetinteractivescope (* set simulator's interactive scope *)
| oth -> STRING ("vpi_control_"^string_of_int oth)

  let optype x = vpi_expr (x)

  let meth x = vpi_meth (x)

  let cons_typ x = vpi_const_typ (x)

  let direction x = vpi_port_net (x)

  let net_type x = vpi_obj (x)
  
  let rec to_list kind = function
    | Vpiparent :: tl -> to_list kind tl
    | oth -> TUPLE2(kind, TLIST oth)

  let rec to_tuple kind = function
    | [] -> kind
    | Vpiparent :: tl -> to_tuple kind tl
    | arg::[] -> TUPLE2(kind, arg)
    | arg1::arg2::[] -> TUPLE3(kind, arg1, arg2)
    | arg1::arg2::arg3::[] -> TUPLE4(kind, arg1, arg2, arg3)
    | arg1::arg2::arg3::arg4::[] -> TUPLE5(kind, arg1, arg2, arg3, arg4)
    | arg1::arg2::arg3::arg4::arg5::[] -> TUPLE6(kind, arg1, arg2, arg3, arg4, arg5)
    | arg1::arg2::arg3::arg4::arg5::arg6::[] ->
    TUPLE7(kind, arg1, arg2, arg3, arg4, arg5, arg6)
    | arg1::arg2::arg3::arg4::arg5::arg6::arg7::[] ->
    TUPLE8(kind, arg1, arg2, arg3, arg4, arg5, arg6, arg7)
    | arg1::arg2::arg3::arg4::arg5::arg6::arg7::arg8::[] ->
    TUPLE9(kind, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)
    | arg1::arg2::arg3::arg4::arg5::arg6::arg7::arg8::arg9::[] ->
    TUPLE10(kind, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)
    | arg1::arg2::arg3::arg4::arg5::arg6::arg7::arg8::arg9::arg10::[] ->
    TUPLE11(kind, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)
    | arg1::arg2::arg3::arg4::arg5::arg6::arg7::arg8::arg9::arg10::arg11::[] ->
    TUPLE12(kind, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)
    | arg1::arg2::arg3::arg4::arg5::arg6::arg7::arg8::arg9::arg10::arg11::arg12::[] ->
    TUPLE13(kind, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)
    | arg1::arg2::arg3::arg4::arg5::arg6::arg7::arg8::arg9::arg10::arg11::arg12::arg13::[] ->
    TUPLE14(kind, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)
    | arg1::arg2::arg3::arg4::arg5::arg6::arg7::arg8::arg9::arg10::arg11::arg12::arg13::arg14::[] ->
    TUPLE15(kind, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14)
    | arg1::arg2::arg3::arg4::arg5::arg6::arg7::arg8::arg9::arg10::arg11::arg12::arg13::arg14::arg15::[] ->
    TUPLE16(kind, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15)
    | arg1::arg2::arg3::arg4::arg5::arg6::arg7::arg8::arg9::arg10::arg11::arg12::arg13::arg14::arg15::arg16::[] ->
    TUPLE17(kind, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16)
    | oth -> print_endline ("kind:"^getstr kind^" "^string_of_int (List.length oth)); TUPLE2(kind, TLIST oth)

  let cache = Hashtbl.create 257

  let loc_cache kind loc tup =
      Hashtbl.replace cache (kind,loc) tup;
      tup

let rec event_collapse = function
| TUPLE3 (Vpieventorop, ev, ev') -> ev' :: event_collapse ev
| oth -> [oth]

let rec rw' = function
| LOC _ as loc -> loc
| TLIST slst -> TLIST (List.map rw' slst)
| TUPLE2 (kw, (LOC _ as loc)) -> rw' (try Hashtbl.find cache (kw, loc) with _ -> TLIST [])
| TUPLE2(a,b) -> TUPLE2(rw' a, rw' b)
| TUPLE3 (Vpieventorop, TUPLE3 (Vpieventorop, _, _), _) as ev -> TUPLE2 (Vpieventorop, TLIST (List.map rw' (event_collapse ev)))
| TUPLE3(a,b,c) -> TUPLE3(rw' a, rw' b, rw' c)
| TUPLE4(a,b,c,d) -> TUPLE4(rw' a, rw' b, rw' c, rw' d)
| TUPLE5(a,b,c,d,e) -> TUPLE5(rw' a, rw' b, rw' c, rw' d, rw' e)
| TUPLE6(a,b,c,d,e,f) -> TUPLE6(rw' a, rw' b, rw' c, rw' d, rw' e, rw' f)
| TUPLE7(a,b,c,d,e,f,g) -> TUPLE7(rw' a, rw' b, rw' c, rw' d, rw' e, rw' f, rw' g)
| TUPLE8(a,b,c,d,e,f,g,h) -> TUPLE8(rw' a, rw' b, rw' c, rw' d, rw' e, rw' f, rw' g, rw' h)
| TUPLE9(a,b,c,d,e,f,g,h,i) -> TUPLE9(rw' a, rw' b, rw' c, rw' d, rw' e, rw' f, rw' g, rw' h, rw' i)
| TUPLE10(a,b,c,d,e,f,g,h,i,j) -> TUPLE10(rw' a, rw' b, rw' c, rw' d, rw' e, rw' f, rw' g, rw' h, rw' i, rw' j)
| oth -> oth

%}

%token INVALID
%token SP
%token SEMI
%token DQUOTE
%token PLUS
%token MINUS
%token STAR
%token POW
%token NL
%token <string*int> SRC
%token <string> IDENT
%token <Surelog_uhdm_types.cexp> NUM
%token <int * Surelog_uhdm_types.cexp> SIZED
%token <Surelog_uhdm_types.dirop> DIR
%token <string option> BEGIN
%token LCURLY
%token RCURLY
%token LCOMMENT
%token RCOMMENT
%token LSHIFT
%token RSHIFT
%token IFF
%token ELSE
%token LOGIC
%token WIRE
%token REG
%token VSTRING
%token ASSIGN
%token ASSIGNMENT
%token ASSIGNDLY
%token <Surelog_uhdm_types.cmpop>CMPOP
%token CASE
%token ENDCASE
%token WHILE
%token FOR
%token ALWAYS
%token POSEDGE
%token NEGEDGE
%token RETURN
%token FUNCTION
%token ENDFUNCTION
%token TASK
%token ENDTASK
%token MODULE
%token PARAMETER
%token ENDMODULE
%token INITIAL
%token FINAL
%token INTERFACE
%token ENDINTERFACE
%token PACKAGE
%token ENDPACKAGE
%token MODPORT
%token  Indent
%token  Unindent
%token  EQUALS
%token  ACCEPT
%token  AMPERSAND
%token  AT
%token  HYPHEN
%token  BACKQUOTE
%token  BACKSLASH
%token  SLASH
%token  CARET
%token  COLON
%token  COMMA
%token  LPAREN
%token  RPAREN
%token <token> CONS1
%token <token*token> CONS2
%token <token*token*token> CONS3
%token <token*token*token*token> CONS4
%token  DEFAULT
%token  DOLLAR
%token  DOT
%token  DOUBLEQUOTE
%token <token list> ELIST
%token  EMPTY_TOKEN
%token  END
%token  EOF_TOKEN
%token  ERROR
%token  ERROR_TOKEN
%token  GREATER
%token  HASH
%token  LBRACE
%token  LBRACK
%token  LESS
%token  LINEFEED
%token  PERCENT
%token  PLING
%token  QUERY
%token  QUOTE
%token  RBRACE
%token  RBRACK
%token <string list> SLIST
%token <string> STRING
%token <string> STRING_CONST
%token  TILDE
%token <token list> TLIST
%token <token*token> TUPLE2
%token <token*token*token> TUPLE3
%token <token*token*token*token> TUPLE4
%token <token*token*token*token*token> TUPLE5
%token <token*token*token*token*token*token> TUPLE6
%token <token*token*token*token*token*token*token> TUPLE7
%token <token*token*token*token*token*token*token*token> TUPLE8
%token <token*token*token*token*token*token*token*token*token> TUPLE9
%token <token*token*token*token*token*token*token*token*token*token> TUPLE10
%token <token*token*token*token*token*token*token*token*token*token*token> TUPLE11
%token <token*token*token*token*token*token*token*token*token*token*token*token> TUPLE12
%token <token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE13
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE14
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE15
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE16
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE17
%token <int> Vpimethodint
%token <int> Int
%token <int*int*int*int> LOC
%token <int*int*bool> Width
%token  UNDERSCORE
%token  VBAR
%token FINISHED
%token INT
%token <string> HEXS
%token <string> DEC
%token <string> OCT
%token <string> BIN
%token KILLED
%token Post_Elab
%token Pre_Elab
%token RUNNING
%token Restored
%token SUSPENDED
%token UINT
%token WAITING
%token Always
%token Array_net
%token Assignment
%token Attribute
%token Begin
%token Bit_select
%token Case_item
%token Case_stmt
%token Class_defn
%token Class_typespec
%token Class_var
%token Constant
%token Cont_assign
%token DESIGN
%token Design
%token Enum_const
%token Enum_typespec
%token Enum_var
%token Event_control
%token For_stmt
%token Function
%token Gen_if_else
%token Gen_region
%token Gen_scope
%token Gen_scope_array
%token If_else
%token If_stmt
%token Indexed_part_select
%token Initial
%token Int_typespec
%token Int_var
%token Integer_typespec
%token Integer_var
%token Io_decl
%token Logic_net
%token Logic_typespec
%token Logic_var
%token Module_inst
%token Operation
%token Package
%token Param_assign
%token Parameter
%token Part_select
%token Port
%token Range
%token Ref_module
%token Ref_obj
%token Ref_typespec
%token Ref_var
%token Sys_func_call
%token Task
%token Task_call
%token Var_select
%token Weaklyreferenced
%token Any_sverilog_class
%token Array
%token Await
%token Bound
%token Builtin
%token Clk
%token Endln
%token File
%token Get
%token Keycount
%token Kill
%token Line
%token Mailbox
%token Message
%token New
%token Num
%token Peek
%token Process
%token Put
%token Queue
%token Resume
%token Self
%token Semaphore
%token State
%token Status
%token String
%token Suspend
%token System
%token Try_get
%token Try_peek
%token Try_put
%token Uhdmallclasses
%token Uhdmallmodules
%token Uhdmallpackages
%token Uhdmtopmodules
%token Uhdmtoppackages
%token Vpiactual
%token Vpialwaystype
%token Vpiarraynet
%token Vpielaborated
%token Vpimethod
%token Vpiclassdefn
%token Vpigenstmt
%token Vpitop
%token Vpitypedef
%token Vpienumconst
%token Vpivisibility
%token Vpibasetypespec
%token Vpitypespec
%token Vpisigned
%token Vpioverriden
%token Vpirandtype
%token Vpireturn
%token Vpirefmodule
%token Vpiinstance

%token Vpiactive (* reentrant task/func frame is active *)
%token Vpiactivetimeformat (* active $timeformat() system task *)
%token Vpiaddop (* binary addition *)
%token Vpialways (* always procedure *)
%token Vpiandprim (* and gate *)
%token Vpiargument (* argument to (system) task/function *)
%token Vpiarithlshiftop (* arithmetic left shift (1364-2001) *)
%token Vpiarithrshiftop (* arithmetic right shift (1364-2001) *)
%token Vpiarray (* variable array (Boolean) *)
%token Vpiassignment (* procedural assignment *)
%token Vpiassignstmt (* quasi-continuous assignment *)
%token Vpiattribute (* attribute of an object *)
%token Vpiautomatic (* task/func obj is automatic *)
%token Vpiautomatics (* Automatic variables of a frame *)
%token Vpibaseexpr (* Indexed part-select's base expression *)
%token Vpibegin (* block statement *)
%token Vpibinaryconst (* binary integer *)
%token Vpibit (* bit of vector net or port *)
%token Vpibitandop (* binary bitwise AND *)
%token Vpibitnegop (* bitwise negation *)
%token Vpibitorop (* binary bitwise OR *)
%token Vpibitselect (* Bit-select of parameter, var select *)
%token Vpibitxnorop (* binary bitwise XNOR *)
%token Vpibitxorop (* binary bitwise XOR *)
%token Vpiblocking (* blocking assignment (Boolean) *)
%token Vpibufif0prim (* zero-enabled buffer *)
%token Vpibufif1prim (* one-enabled buffer *)
%token Vpibufprim (* buffer *)
%token Vpicallback (* callback object *)
%token Vpicase (* case statement *)
%token Vpicaseeqop (* case (x and z) equality *)
%token Vpicaseexact (* exact match *)
%token Vpicaseitem (* case statement item *)
%token Vpicaseneqop (* case inequality *)
%token Vpicasetype (* case statement subtypes: *)
%token Vpicasex (* ignore X's *)
%token Vpicasez (* ignore Z's *)
%token Vpicell (* configuration cell *)
%token Vpicellinstance (* cell (Boolean) *)
%token Vpichargestrength (* charge decay strength of net *)
%token Vpicmosprim (* cmos switch *)
%token Vpicombprim (* combinational UDP *)
%token Vpiconcatop (* n-ary concatenation *)
%token Vpicondition (* condition expression *)
%token Vpiconditionop (* ternary conditional *)
%token Vpiconfig (* configuration config file *)
%token Vpiconnbyname (* connected by name (Boolean) *)
%token Vpiconstant (* numerical constant or string literal *)
%token Vpiconstantselect (* (Boolean) bit-select or part-select indices are constant expressions *)
%token Vpiconsttype (* constant subtypes: *)
%token Vpicontassign (* continuous assignment *)
%token Vpicontassignbit (* Bit of a vector continuous assignment *)
%token Vpidatapolarity (* ...or data path: *)
%token Vpideassign (* deassignment statement *)
%token Vpidecconst (* decimal integer *)
%token <string> Vpidecompile (* decompile the object *)
%token Vpidefattribute (* Attribute defined for the obj *)
%token Vpidefdecaytime (* Default decay time for a module *)
%token Vpidefdelaymode (* Default delay mode for a module *)
%token Vpideffile (* File name where the module is defined*)
%token Vpideflineno (* line number for module definition *)
%token Vpidefname (* module definition name *)
%token Vpidefnettype (* default net type *)
%token Vpidefparam (* defparam *)
%token Vpidelay (* net or gate delay *)
%token Vpidelaycontrol (* delay statement (e.g., #10) *)
%token Vpidelaydevice (* Delay object within a net *)
%token Vpidelaymodedistrib (* distributed delay mode *)
%token Vpidelaymodemtm (* min:typ:max delay mode *)
%token Vpidelaymodenone (* no delay mode specified *)
%token Vpidelaymodepath (* path delay mode *)
%token Vpidelaymodeunit (* unit delay mode *)
%token Vpidelaymodezero (* zero delay mode *)
%token Vpidelayterm (* Delay term which is a load or driver *)
%token Vpidelaytype (* delay subtype *)
%token Vpidirection (* direction of port: *)
%token Vpidisable (* named block disable statement *)
%token Vpidivop (* binary division *)
%token Vpidriver (* driver for a net *)
%token Vpiedge (* edge type of module path: *)
%token Vpielsestmt (* else statement *)
%token Vpieqop (* binary equality *)
%token Vpieventcontrol (* wait on event, e.g., @e *)
%token Vpieventorop (* event OR *)
%token Vpieventstmt
%token Vpiexpanded (* expanded vector net (Boolean) *)
%token Vpiexplicitname (* port is explicitly named *)
%token Vpiexplicitscalared (* explicitly scalared (Boolean) *)
%token Vpiexplicitvectored (* explicitly vectored (Boolean) *)
%token Vpiexpr (* connected expression *)
%token Vpifile (* File name in which the object is used*)
%token Vpifinish (* execute simulator's $finish *)
%token Vpifor (* for statement *)
%token Vpiforce (* force statement *)
%token Vpiforever (* forever statement *)
%token Vpiforincstmt (* increment statement in for loop *)
%token Vpiforinitstmt (* initialization statement in for loop *)
%token Vpifork (* fork-join block *)
%token Vpiframe (* reentrant task/func frame *)
%token Vpifullname (* full hierarchical name *)
%token Vpifullskew (* $fullskew -- added for 1364-2001 *)
%token Vpifunccall (* function call *)
%token Vpifunction (* function *)
%token Vpifunctype (* function & system function type *)
%token Vpigate (* primitive gate *)
%token Vpigatearray (* gate instance array *)
%token Vpigenscope (* A generated scope *)
%token Vpigenscopearray (* array of generated scopes *)
%token Vpigenvar (* Object used to instantiate gen scopes *)
%token Vpigeop (* binary greater than or equal *)
%token Vpigtop (* binary greater than *)
%token Vpihexconst (* hexadecimal integer *)
%token Vpihighconn (* higher connection to port *)
%token Vpihighz (* No default drive given *)
%token Vpihold (* $hold *)
%token Vpiif (* if statement *)
%token Vpiifelse (* if-else statement *)
%token Vpiimplicitdecl (* implicitly declared net (Boolean) *)
%token Vpiindex (* index of var select, bit-select, etc. *)
%token Vpiindexedpartselect (* Indexed part-select object *)
%token Vpiindexedpartselecttype (* Indexed part-select type *)
%token Vpiinitial (* initial procedure *)
%token Vpiinout (* inout *)
%token Vpiinput (* input *)
%token Vpiinstancearray (* vpiInstance arrays *)
%token Vpiintconst (* integer constant (1364-2001) *)
%token Vpiintegervar (* integer variable *)
%token Vpiinterm (* To get to a delay device's drivers. *)
%token Vpiintermodpath (* intermodule wire delay *)
%token Vpiintermodpathdelay (* intermodule path delay *)
%token Vpiinternalscope (* internal scope in module *)
%token Vpiintfunc (* returns integer *)
%token Vpiiodecl (* input/output declaration *)
%token Vpiismemory (* TRUE for a one-dimensional reg array *)
%token Vpiisprotected (* TRUE for protected design information *)
%token Vpiiterator (* iterator *)
%token Vpiiteratortype (* object type of an iterator *)
%token Vpileftrange (* left range of vector or part-select *)
%token Vpileop (* binary less than or equal *)
%token Vpilhs (* left-hand side of assignment *)
%token Vpilibrary (* configuration library *)
%token Vpilineno (* line number where the object is used *)
%token Vpilistop (* list of expressions *)
%token Vpiload (* load on net or reg *)
%token Vpilocaldriver (* local drivers (within a module *)
%token Vpilocalload (* local loads (within a module *)
%token Vpilocalparam (* TRUE when a param is declared as a localparam *)
%token Vpilogandop (* binary logical AND *)
%token Vpilogorop (* binary logical OR *)
%token Vpilowconn (* lower connection to port *)
%token Vpilshiftop (* binary left shift *)
%token Vpiltop (* binary less than *)
%token Vpimemory (* behavioral memory *)
%token Vpimemoryword (* single word of memory *)
%token Vpimintypmaxop (* min:typ:max: delay expression *)
%token Vpiminusop (* unary minus *)
%token Vpimipdelay (* module input port delay *)
%token Vpimixedio (* mixed input-output *)
%token Vpimoddatapathin (* data terminal of a module path *)
%token Vpimodop (* binary modulus *)
%token Vpimodpath (* module path for path delays *)
%token Vpimodpathdelay (* module path delay *)
%token Vpimodpathhasifnone (* Mod path has an ifnone statement *)
%token Vpimodpathin (* Input terminal of a module path *)
%token Vpimodpathout (* output terminal of a module path *)
%token Vpimodule (* module instance *)
%token Vpimodulearray (* module instance array *)
%token Vpimulticoncatop (* repeated concatenation *)
%token Vpimultop (* binary multiplication *)
%token Vpiname (* local name of object *)
%token Vpinamedbegin (* named block statement *)
%token Vpinamedevent (* event variable *)
%token Vpinamedeventarray (* multidimensional named event *)
%token Vpinamedfork (* named fork-join block *)
%token Vpinandprim (* nand gate *)
%token Vpinegative (* negative *)
%token Vpinegedgeop (* negedge *)
%token Vpinegindexed (* -: *)
%token Vpineqop (* binary inequality *)
%token Vpinet (* scalar or vector net *)
%token Vpinetarray (* multidimensional net *)
%token Vpinetbit (* bit of vector net *)
%token Vpinetdeclassign (* assign part of decl (Boolean) *)
%token Vpinettype (* net subtypes: *)
%token Vpinmosprim (* nmos switch *)
%token Vpinochange (* $nochange *)
%token Vpinodirection (* no direction *)
%token Vpinone (* no default net type (1364-2001) *)
%token Vpinorprim (* nor gate *)
%token Vpinotif0prim (* zero-enabled not gate *)
%token Vpinotif1prim (* one-enabled not gate *)
%token Vpinotop (* unary not *)
%token Vpinotprim (* not gate *)
%token Vpinullop (* null operation *)
%token Vpinullstmt (* a semicolon. Ie. #10 ; *)
%token Vpioctconst (* octal integer *)
%token Vpioffset (* offset from LSB *)
%token Vpioperand (* operand of expression *)
%token Vpioperation (* behavioral operation *)
%token Vpioptype (* operation subtypes: *)
%token Vpiorprim (* or gate *)
%token Vpioutput (* output *)
%token Vpioutterm (* To get to a delay device's loads. *)
%token Vpiparamassign (* module parameter assignment *)
%token Vpiparameter (* module parameter *)
%token Vpiparent (* parent object *)
%token Vpipartselect (* part-select *)
%token Vpipathfull
%token Vpipathparallel
%token Vpipathterm (* terminal of module path *)
%token Vpiperiod (* $period *)
%token Vpiplusop (* unary plus *)
%token Vpipmosprim (* pmos switch *)
%token Vpipolarity (* polarity of module path... *)
%token Vpiport (* module port *)
%token Vpiportbit (* bit of vector module port *)
%token Vpiportindex (* Port index *)
%token Vpiportinst (* connected port instance *)
%token Vpiports (* Module port *)
%token Vpiposedge (* path delay connection subtypes: *)
%token Vpiposedgeop (* posedge *)
%token Vpiposindexed (* +: *)
%token Vpipositive (* positive *)
%token Vpipowerop (* arithmetic power op (1364-2001) *)
%token Vpiprimitive (* primitive (gate, switch, UDP) *)
%token Vpiprimitivearray (* vpiprimitiveArray type *)
%token Vpiprimterm (* primitive terminal *)
%token Vpiprimtype (* primitive subtypes: *)
%token Vpiprocess (* process in module, program or interface *)
%token Vpiprotected (* source protected module (Boolean) *)
%token Vpipull0 (* default pull0 drive *)
%token Vpipull1 (* default pull1 drive *)
%token Vpipulldownprim (* pulldown *)
%token Vpipullupprim (* pullup *)
%token Vpirange (* range declaration *)
%token Vpircmosprim (* resistive cmos switch *)
%token Vpirealconst (* real *)
%token Vpirealfunc (* returns real *)
%token Vpirealvar (* real variable *)
%token Vpirecovery (* $recovery *)
%token Vpirecrem (* $recrem -- added for 1364-2001 *)
%token Vpireg (* scalar or vector reg *)
%token Vpiregarray (* multidimensional reg *)
%token Vpiregbit (* bit of vector reg *)
%token Vpirelease (* release statement *)
%token Vpiremoval (* $removal -- added for 1364-2001 *)
%token Vpirepeat (* repeat statement *)
%token Vpirepeatcontrol (* repeat control in an assign stmt *)
%token Vpireset (* execute simulator's $reset *)
%token Vpiresolvednettype (* net subtype after resolution, returns same subtypes as vpiNetType *)
%token Vpirhs (* right-hand side of assignment *)
%token Vpirightrange (* right range of vector or part-select *)
%token Vpirnmosprim (* resistive nmos switch *)
%token Vpirpmosprim (* resistive pmos switch *)
%token Vpirshiftop (* binary right shift *)
%token Vpirtranif0prim (* zero-enable resistive bidirectional *)
%token Vpirtranif1prim (* one-enable resistive bidirectional *)
%token Vpirtranprim (* resistive bidirectional *)
%token Vpisaverestartid (* unique ID for save/restart data *)
%token Vpisaverestartlocation (* name of save/restart data file *)
%token Vpiscalar (* scalar (Boolean) *)
%token Vpischedevent (* vpi_put_value() event *)
%token Vpischeduled (* object still scheduled (Boolean) *)
%token Vpiscope (* containing scope object *)
%token Vpiseqprim (* sequential UDP *)
%token Vpisetinteractivescope (* set simulator's interactive scope *)
%token Vpisetup (* $setup *)
%token Vpisetuphold (* $setuphold *)
%token Vpisimnet (* simulated net after collapsing *)
%token Vpisize (* size of gate, net, port, etc. *)
%token Vpisizedfunc (* returns an arbitrary size *)
%token Vpisizedsignedfunc (* returns sized signed value *)
%token Vpiskew (* $skew *)
%token Vpispecparam (* specparam *)
%token Vpistmt (* statement in process or task *)
%token Vpistop (* execute simulator's $stop *)
%token Vpistrength0 (* 0-strength of net or gate *)
%token Vpistrength1 (* 1-strength of net or gate *)
%token Vpistringconst (* string literal *)
%token Vpisubop (* binary subtraction *)
%token Vpisupply0 (* supply-0 net *)
%token Vpisupply1 (* supply-1 net *)
%token Vpiswitch (* transistor switch *)
%token Vpiswitcharray (* switch instance array *)
%token Vpisysfunccall (* system function call *)
%token Vpisystaskcall (* system task call *)
%token Vpisystfcall (* task function call *)
%token Vpitableentry (* UDP state table entry *)
%token Vpitask (* task *)
%token Vpitaskcall (* task call *)
%token Vpitaskfunc (* task/function *)
%token Vpitchk (* timing check *)
%token Vpitchkdataterm (* timing check data term *)
%token Vpitchknotifier (* timing check notifier *)
%token Vpitchkrefterm (* timing check reference term *)
%token Vpitchkterm (* terminal of timing check *)
%token Vpitchktype (* timing check subtypes: *)
%token Vpitermindex (* Index of a primitive terminal *)
%token Vpitimeconst (* time constant *)
%token Vpitimefunc (* returns time *)
%token Vpitimeprecision (* module time precision *)
%token Vpitimequeue (* simulation event queue *)
%token Vpitimeskew (* $timeskew -- added for 1364-2001 *)
%token Vpitimeunit (* module time unit *)
%token Vpitimevar (* time variable *)
%token Vpitopmodule (* top-level module (Boolean) *)
%token Vpitranif0prim (* zero-enabled bidirectional *)
%token Vpitranif1prim (* one-enabled bidirectional *)
%token Vpitranprim (* bidirectional *)
%token Vpitri (* tri net *)
%token Vpitri0 (* pull-down net *)
%token Vpitri1 (* pull-up net *)
%token Vpitriand (* three-state wire-and net *)
%token Vpitrior (* three-state wire-or net *)
%token Vpitrireg (* three-state reg net *)
%token Vpitype (* type of object *)
%token Vpiudp (* user-defined primitive *)
%token Vpiudparray (* UDP instance array *)
%token Vpiudpdefn (* UDP definition *)
%token Vpiuintconst (* unsigned integer constant !!! NOT Standard !!! *)
%token Vpiunaryandop (* bitwise reduction AND *)
%token Vpiunarynandop (* bitwise reduction NAND *)
%token Vpiunarynorop (* bitwise reduction NOR *)
%token Vpiunaryorop (* bitwise reduction OR *)
%token Vpiunaryxnorop (* bitwise reduction XNOR *)
%token Vpiunaryxorop (* bitwise reduction XOR *)
%token Vpiunconndrive (* unconnected port drive strength *)
%token Vpiundefined (* undefined property *)
%token Vpiunknown (* unknown (unspecified) *)
%token Vpiuse (* usage *)
%token Vpiuserdefn (*user-defined system task/func(Boolean)*)
%token Vpiusersystf (* user-defined system task/function *)
%token Vpiuwire (* unresolved wire net (1364-2005) *)
%token Vpivalid (* reentrant task/func frame or automatic variable is valid *)
%token Vpivariables (* variables in module *)
%token Vpivarselect (* variable array selection *)
%token Vpivector (* vector (Boolean) *)
%token Vpiwait (* wait statement *)
%token Vpiwand (* wire-and net *)
%token Vpiwhile (* while statement *)
%token Vpiwidth (* $width *)
%token Vpiwidthexpr (* Indexed part-select's width expression *)
%token Vpiwire (* wire net *)
%token Vpiwor (* wire-or net *)
%token Vpixnorprim (* xnor gate *)
%token Vpixorprim (* xor gate *)

%token Vpiarraytype
%token Vpielemtypespec
%token Array_typespec
%token Array_var
%token Gen_case
%token Named_begin

%token Work
%token <string> VpiNum 
%type <(token*token,token)Hashtbl.t * token list> ml_start
%type <token> parameter_def
%type <token list> parameter_lst
%type <token list> always_lst
%type <token list> initial_lst
%type <token> logic_net_def
%type <token> logic_var_def
%type <token> parameter_opt
%type <token> always_typ
%type <token> input
%start ml_start
%%

ml_start: input_lst EOF_TOKEN { (cache, List.map rw' $1) }

package_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpidefname COLON def_name { $3 }
  | Vpitop COLON Int { TUPLE2(Vpitop, Int $3) }
  | Vpiclassdefn COLON class_intro { TUPLE2(Vpiclassdefn, $3) }
  | Design COLON design_nam { $3 }
  
class_intro: Class_defn COLON class_def Indent class_lst Unindent { to_tuple Class_defn $5 }

parent:
  | Package COLON Builtin LPAREN Builtin COLON COLON RPAREN { Package }
  | Design COLON design_nam { Design }
  | Class_defn COLON class_def { Class_defn }
  | Function COLON function_def { Function }
  | Task COLON task_def { Task }
  | Class_var COLON class_var_def { Class_var }
  | Int_var COLON int_var_def { Int_var }
  | Integer_var COLON integer_var_def { Int_var }
  | Logic_var COLON logic_var_def { Logic_var }
  | Array_var COLON array_var_def { Array_var }
  | Io_decl COLON io_decl_def { Io_decl }
  | Enum_var COLON enum_var_def { Enum_var }
  | Module_inst COLON module_inst_def { TUPLE2(Module_inst, $3) }
  | Logic_net COLON logic_net_def { Logic_net }
  | Port COLON port_def { Port }
  | Always COLON loc { Always }
  | Event_control COLON event_control_def { Event_control }
  | Operation COLON operation_def { Operation }
  | Begin COLON begin_def { TUPLE2(Begin, $3) }
  | Named_begin COLON named_begin_def { TUPLE2(Named_begin, $3) }
  | Assignment COLON assignment_def { Assignment }
  | Var_select COLON var_select_def { Var_select }
  | Range COLON range_def { Range }
  | Parameter parameter_def { Parameter }
  | Param_assign COLON loc { Param_assign }
  | Constant COLON constant_def { Constant }
  | Initial COLON loc { Initial }
  | If_stmt COLON loc { If_stmt }
  | If_else COLON loc { If_else }
  | For_stmt COLON for_stmt_def { For_stmt }
  | Bit_select COLON bit_sel_def { Bit_select }
  | Case_stmt COLON loc { Case_stmt }
  | Case_item COLON loc { Case_item }
  | Sys_func_call COLON sys_func_call_def { Sys_func_call }
  | Cont_assign COLON loc { Cont_assign }
  | Gen_region COLON loc { Gen_region }
  | Gen_case COLON loc { Gen_case }
  | Gen_if_else COLON loc { Gen_if_else }
  | Ref_module COLON ref_module_def { Ref_module }
  | Indexed_part_select COLON indexed_part_select_def { Indexed_part_select }
  | Array_net COLON array_net_def { Array_net }
  | Gen_scope_array COLON gen_scope_array_def { Gen_scope_array }
  | Gen_scope COLON gen_scope_def { Gen_scope }
  | Part_select COLON part_select_def { Part_select }
  | Ref_typespec COLON type_spec { Ref_typespec }
  | ref_typespec_actual { $1 }
  
loc: { Work }
  | COMMA Line COLON Int COLON Int COMMA Endln COLON Int COLON Int { LOC($4, $6, $10, $12) }

ref_module_def:
  | Work AT STRING LPAREN name RPAREN loc { TUPLE3(STRING $3, $5, $7) }

class_var_def:
  | LPAREN Work AT name type_lst RPAREN loc { Work }

class_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { TUPLE2(Vpiname, $5) }
  | Vpifullname COLON fullnam { $3 }
  | Vpimethod COLON vpi_method_arg { TUPLE2(Vpimethod, $3) }
  | Vpitypedef COLON typespec { TUPLE2(Vpitypedef, $3) }

typespec:
  | Enum_typespec COLON enum_typespec_decl Indent enum_typespec_lst Unindent { loc_cache Enum_typespec $3 (to_list Enum_typespec $5) }
  | Enum_typespec COLON enum_typespec_decl { TUPLE2 (Enum_typespec, $3) }

enum_typespec_lst: { [] }
  | enum_typespec_opt enum_typespec_lst { $1 :: $2 }

enum_typespec_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpienumconst COLON Enum_const COLON enum_const_decl Indent enum_constant_lst Unindent { loc_cache Enum_const $5 (to_tuple Enum_const $7) }
  | Vpiinstance COLON Module_inst COLON module_inst_def { TUPLE2(Vpiinstance, $5) }
  | Vpibasetypespec COLON ref_typespec { TUPLE2(Vpitypespec, $3) }

parameter_lst: { [] }
  | parameter_opt parameter_lst { $1 :: $2 }

parameter_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpidecompile { Vpidecompile $1 }
  | Vpisize COLON Int { TUPLE2(Vpisize, Int $3) }
  | UINT COLON Int { TUPLE2(UINT, Int $3) }
  | INT COLON Int { TUPLE2(INT, Int $3) }
  | HEXS { HEXS $1 }
  | DEC { DEC $1 }
  | OCT { OCT $1 }
  | BIN { BIN $1 }
  | Vpisigned COLON Int { Vpisigned }
  | Vpilocalparam COLON Int { Vpilocalparam }
  | Vpitypespec COLON { Vpitypespec }
  | ref_typespec { $1 }

enum_constant_lst: { [] }
  | enum_constant_opt enum_constant_lst { $1 :: $2 }

enum_constant_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpidecompile { Vpidecompile $1 }
  | Vpisize COLON Int { TUPLE2(Vpisize, Int $3) }
  | UINT COLON Int { TUPLE2(UINT, Int $3) }
  | INT COLON Int { TUPLE2(INT, Int $3) }

vpi_method_arg:
  | Function COLON function_def Indent function_lst Unindent { Function }
  | Task COLON task_def Indent task_lst Unindent { Task }
  | Function COLON function_def { Function }
  | Task COLON task_def { Task }

class_var_lst: { [] }
  | class_var_opt class_var_lst { $1 :: $2 }

class_var_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpitypespec COLON { Vpitypespec }
  | ref_typespec { $1 }

enum_var_lst: { [] }
  | enum_var_opt enum_var_lst { $1 :: $2 }

enum_var_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpitypespec COLON ref_typespec { TUPLE2(Vpitypespec, $3) }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpivisibility COLON vis { TUPLE2(Vpivisibility, $3) }

int_var_lst: { [] }
  | int_var_opt int_var_lst { $1 :: $2 }

int_var_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpitypespec COLON ref_typespec { TUPLE2(Vpitypespec, $3) }
  | Vpisigned COLON Int { TUPLE2(Vpisigned, Int $3) }

int_typespec_lst: { [] }
  | int_typespec_opt int_typespec_lst { $1 :: $2 }

int_typespec_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpirange COLON Range COLON range_def Indent range_lst Unindent { to_tuple Vpirange $7 }
  | Vpitypespec COLON ref_typespec { TUPLE2(Vpitypespec, $3) }
  | Vpisigned COLON Int { TUPLE2(Vpisigned, Int $3) }

integer_typespec_lst: { [] }
  | integer_typespec_opt integer_typespec_lst { $1 :: $2 }

integer_typespec_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpirange COLON Range COLON range_def Indent range_lst Unindent { to_tuple Vpirange $7 }
  | Vpitypespec COLON ref_typespec { TUPLE2(Vpitypespec, $3) }
  | Vpisigned COLON Int { TUPLE2(Vpisigned, Int $3) }

integer_var_lst: { [] }
  | integer_var_opt integer_var_lst { $1 :: $2 }

integer_var_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpitypespec COLON ref_typespec { TUPLE2(Vpitypespec, $3) }
  | Vpisigned COLON Int { TUPLE2(Vpisigned, Int $3) }
  | Vpivisibility COLON vis { TUPLE2(Vpivisibility, $3) }

logic_var_lst: { [] }
  | logic_var_opt logic_var_lst { $1 :: $2 }

logic_var_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpitypespec COLON { Vpitypespec }
  | ref_typespec { $1 }
  | Vpisigned COLON Int { TUPLE2(Vpisigned, Int $3) }
  | Vpivisibility COLON vis { TUPLE2(Vpivisibility, $3) }

ref_typespec:
  | Ref_typespec COLON type_spec Indent ref_typespec_lst Unindent { loc_cache Ref_typespec $3 (to_tuple Ref_typespec $5) }

ref_typespec_lst: { [] }
  | ref_typespec_opt ref_typespec_lst { $1 :: $2 }

ref_typespec_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpifullname COLON fullnam { $3 }
  | Vpiactual COLON ref_typespec_actual { TUPLE2(Vpiactual, $3) }

ref_typespec_actual:
  | Class_typespec COLON class_typespec { TUPLE2(Class_typespec, $3) }
  | Int_typespec COLON int_typespec_def { TUPLE2(Int_typespec, $3) }
  | Integer_typespec COLON integer_typespec_def { TUPLE2(Integer_typespec, $3) }
  | Enum_typespec COLON enum_typespec_decl { TUPLE2 (Enum_typespec, $3) }
  | Logic_typespec COLON logic_typespec_def { TUPLE2 (Logic_typespec, $3) }
  | Array_typespec COLON loc { TUPLE2 (Array_typespec, $3) }
  | Array_typespec COLON loc Indent array_typespec_lst Unindent { loc_cache Array_typespec $3 (TUPLE2(Array_typespec, TLIST $5)) }

array_typespec_lst: { [] }
  | array_typespec_opt array_typespec_lst { $1 :: $2 }

array_typespec_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpirange COLON Range COLON range_def Indent range_lst Unindent { to_tuple Vpirange $7 }
  | Vpitypespec COLON ref_typespec { TUPLE2(Vpitypespec, $3) }
  | Vpisigned COLON Int { TUPLE2(Vpisigned, Int $3) }
  | Vpielemtypespec COLON ref_typespec { TUPLE2(Vpielemtypespec, $3) }

function_lst: { [] }
  | function_opt function_lst { $1 :: $2 }

function_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpimethodint { Vpimethodint $1 }
  | Vpivisibility COLON vis { TUPLE2(Vpivisibility, $3) }
  | Vpidefname COLON def_name { $3 }
  | Vpitop COLON Int { TUPLE2(Vpitop, Int $3) }
  | Vpiclassdefn COLON class_intro { Vpiclassdefn }
  | Vpireturn COLON ret { TUPLE2(Vpireturn, $3) }
  | Vpiiodecl COLON { Vpiiodecl }
  | Io_decl COLON io_decl_def Indent io_decl_lst Unindent { to_tuple Io_decl $5 }
  
task_lst: { [] }
  | task_opt task_lst { $1 :: $2 }

task_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpimethodint { Vpimethodint $1 }
  | Vpivisibility COLON vis { TUPLE2(Vpivisibility, $3) }
  | Vpistmt COLON stmt { $3 }
  | Vpidefname COLON def_name { $3 }
  | Vpiiodecl COLON { Vpiiodecl }
  | Vpiinstance COLON Module_inst COLON module_inst_def { TUPLE2(Vpiinstance, $5) }
  | Io_decl COLON io_decl_def Indent io_decl_lst Unindent { to_tuple Io_decl $5 }
  
task_call_lst: { [] }
  | task_call_opt task_call_lst { $1 :: $2 }

task_call_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpitask COLON Task COLON task_def { Vpitask }

io_decl_lst: { [] }
  | io_decl_opt io_decl_lst { $1 :: $2 }

io_decl_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { TUPLE2(Vpiname, $3) }
  | Vpidirection COLON Int { TUPLE2(Vpidirection, Int $3) }
  | Vpilowconn COLON oexpr { TUPLE2(Vpilowconn,$3) }
  | Vpihighconn COLON oexpr { TUPLE2(Vpihighconn,$3) }
  | Vpiexpr COLON oexpr { $3 }
  | Vpitypedef COLON ref_typespec { $3 }
  | Vpiinstance COLON Module_inst COLON module_inst_def { TUPLE2(Vpiinstance, $5) }

bit_sel_lst: { [] }
  | bit_sel_opt bit_sel_lst { $1 :: $2 }

bit_sel_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpiindex COLON oexpr { TUPLE2(Vpiindex, $3) }

ref_var_lst: { [] }
  | ref_var_opt ref_var_lst { $1 :: $2 }

ref_var_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }

ref_obj_lst: { [] }
  | ref_obj_opt ref_obj_lst { $1 :: $2 }

ref_obj_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpiactual COLON ref_obj_actual { TUPLE2(Vpiactual, $3) }

ref_obj_actual:
  | Logic_net COLON logic_net_def { TUPLE2 (Logic_net, $3) }
  | Logic_var COLON logic_var_def { TUPLE2 (Logic_var, $3) }
  | Part_select COLON part_select_def { TUPLE2 (Part_select, $3) }
  | Enum_const COLON enum_const_decl { TUPLE2 (Enum_const, $3) }

constant_lst: { [] }
  | constant_opt constant_lst { $1 :: $2 }

constant_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpidecompile { Vpidecompile $1 }
  | Vpisize COLON Int { TUPLE2(Vpisize, Int $3) }
  | UINT COLON Int { TUPLE2(UINT, Int $3) }
  | INT COLON Int { TUPLE2(INT, Int $3) }
  | STRING_CONST { STRING_CONST $1 }
  | HEXS { HEXS $1 }
  | DEC { DEC $1 }
  | OCT { OCT $1 }
  | BIN { BIN $1 }
  | Vpiconsttype COLON Int { cons_typ $3 }
  | Vpitypespec COLON ref_typespec { TUPLE2(Vpitypespec, $3) }

const_decomp:
  | Int { Int $1 }
  | STRING { STRING $1 }

package_lst: { [] }
  | package_opt package_lst { $1 :: $2 }

class_lst: { [] }
  | class_opt class_lst { $1 :: $2 }

packlst: { [] }
  | pack packlst { $1 :: $2 }

pack: Package COLON Builtin LPAREN Builtin COLON COLON RPAREN Indent package_lst Unindent { to_tuple Package $10 }

module_inst:
  | Module_inst COLON module_inst_def Indent module_inst_lst Unindent { TLIST $5 }

modlst: { [] }
  | module_inst modlst { $1 :: $2 }

elab: { TUPLE2(Vpielaborated, Int 0) }
  | Vpielaborated COLON Int { TUPLE2(Vpielaborated, Int $3) }

modnam:
  | Vpiname COLON Work AT name { Vpiname }
  | Vpiname COLON name { Vpiname }

design_nam: LPAREN Work AT STRING RPAREN { STRING $4 }
  | LPAREN STRING RPAREN { STRING $2 }

input:
  | path COLON { Work }
  | DESIGN COLON design_nam elab modnam {  TUPLE4(DESIGN, $3, $4, $5) }
  | Uhdmallclasses COLON class_intro { TUPLE2(Uhdmallclasses, $3) }
  | Uhdmallpackages COLON packlst { to_tuple Uhdmallpackages $3 }
  | Uhdmtopmodules COLON module_inst { TUPLE2 (Uhdmtopmodules, $3) }
  | Uhdmtoppackages COLON packlst { to_tuple Uhdmtoppackages $3 }
  | Uhdmallmodules COLON module_inst { TUPLE2 (Uhdmallmodules, $3) }
  | Weaklyreferenced COLON weak_lst { TUPLE2 (Weaklyreferenced, TLIST $3) }

weak_lst: { [] }
  | weak_opt weak_lst { $1 :: $2 }

weak_opt:
  | Class_typespec COLON class_typespec Indent Vpiclassdefn COLON Class_defn COLON class_def Unindent { Class_typespec }
  | Int_typespec COLON int_typespec_def Indent int_typespec_lst Unindent { to_tuple Int_typespec $5 }
  | Integer_typespec COLON integer_typespec_def Indent integer_typespec_lst Unindent { to_tuple Integer_typespec $5 }
  | Logic_typespec COLON logic_typespec_def { TUPLE2 (Logic_typespec, $3) }
  | Logic_typespec COLON logic_typespec_def Indent misc_lst Unindent { loc_cache Logic_typespec $3 (to_tuple Logic_typespec ($3::$5)) }
  | Function COLON function_def Indent weak_function_lst Unindent { to_tuple Function $5 }
  | Task COLON task_def Indent task_lst Unindent { to_tuple Task $5 }
  | Enum_typespec COLON enum_typespec_decl Indent enum_typespec_lst Unindent { loc_cache Enum_typespec $3 (to_list Enum_typespec $5) }
  | ref_typespec_actual { $1 }
  
weak_function_lst: { [] }
  | weak_function_opt weak_function_lst { $1 :: $2 }

weak_function_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpimethodint { Vpimethodint $1 }
  | Vpivisibility COLON vis { TUPLE2(Vpivisibility, $3) }
  | Vpidefname COLON def_name { $3 }
  | Vpitop COLON Int { TUPLE2(Vpitop, Int $3) }
  | Vpiclassdefn COLON class_intro { Vpiclassdefn }
  | Vpireturn COLON parent { Vpireturn }
  | Vpiiodecl COLON { Vpiiodecl }
  | Io_decl COLON io_decl_def Indent io_decl_lst Unindent { to_tuple Io_decl $5 }
  
misc_lst: { [] }
  | misc misc_lst { $1 :: $2 }

misc:
  | Vpirange COLON Range COLON range_def Indent range_lst Unindent { to_tuple Vpirange $7 }
  | Vpiparent COLON parent { $3 }
  | Vpisigned COLON Int { Vpisigned }
  
range_lst: { [] }
  | range_opt range_lst { $1 :: $2 }

range_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpileftrange COLON oexpr  { TUPLE2(Vpileftrange, $3) }
  | Vpirightrange COLON oexpr { TUPLE2(Vpirightrange, $3) }

module_inst_lst: { [] }
  | module_inst_opt module_inst_lst { $1 :: $2 }

module_inst_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { TUPLE2(Vpiname, $3) }
  | Vpiname COLON Work AT name { TUPLE2(Vpiname, $5) }
  | Vpifullname COLON fullnam { TUPLE2(Vpifullname, $3) }
  | Vpidefname COLON def_name { TUPLE2(Vpidefname, $3) }
  | Vpideffile COLON path { Vpideffile }
  | Vpideflineno COLON Int { TUPLE2(Vpideflineno, Int $3) }
  | Vpivariables COLON ret { TUPLE2(Vpivariables,$3) }  
  | Vpinet COLON nettyp { TUPLE2(Vpinet, $3) }
  | Vpiarraynet COLON arraynettyp { TUPLE2(Vpinet, $3) }
  | Vpiport COLON vport { TUPLE2(Vpiport, $3) }
  | Vpiprocess COLON process { TUPLE2(Vpiprocess, $3) }
  | Vpitop COLON Int { TUPLE2(Vpitop, Int $3) }
  | Vpitopmodule COLON Int { TUPLE2(Vpitopmodule, Int $3) }
  | Vpiparameter COLON Parameter parameter_def Indent parameter_lst Unindent { loc_cache Parameter $4 (to_tuple Parameter $6) }
  | Vpiparamassign COLON Param_assign COLON loc Indent param_assign_lst Unindent { TUPLE2(Vpiparamassign, TLIST $7) }
  | Vpicontassign COLON cont_assign { $3 }
  | Vpitaskfunc COLON vpi_method_arg { TUPLE2(Vpitaskfunc, $3) }
  | Vpigenstmt COLON genstmt { TUPLE2(Vpigenstmt, $3) }
  | Vpirefmodule COLON Ref_module COLON ref_module_def Indent ref_module_lst Unindent { TUPLE3 (Ref_module, $5, TLIST $7) }
  | Vpimodule COLON module_inst { TUPLE2(Vpimodule, $3) }
  | Vpiinstance COLON Module_inst COLON module_inst_def { TUPLE2(Vpiinstance, $5) }
  | Vpigenscopearray COLON gen_scope_array { TUPLE2(Vpigenscopearray, $3) }
  | Vpitypedef COLON typespec { TUPLE2 (Vpitypedef, $3 ) }

genstmt:
  | Gen_region COLON Indent gen_region_lst Unindent { to_tuple Gen_region $4 }
  | Gen_case COLON Indent gen_case_lst Unindent { to_list Gen_case $4 }
  
gen_scope_array:
  | Gen_scope_array COLON gen_scope_array_def Indent gen_scope_array_lst Unindent { to_tuple Gen_scope_array $5 }

gen_scope_array_lst: { [] }
  | gen_scope_array_opt gen_scope_array_lst { $1 :: $2 }

gen_scope_array_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { Vpiname }
  | Vpifullname COLON fullnam { $3 }
  | Vpigenscope COLON gen_scope { $3 }

gen_scope:
  | Gen_scope COLON gen_scope_def Indent gen_scope_lst Unindent { to_list Gen_scope $5 }

gen_scope_lst: { [] }
  | gen_scope_opt gen_scope_lst { $1 :: $2 }

gen_scope_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { Vpiname }
  | Vpifullname COLON fullnam { $3 }
  | Vpivariables COLON ret { TUPLE2(Vpivariables,$3) }  
  | Vpigenscope COLON gen_scope { $3 }
  | Vpisize COLON Int { TUPLE2(Vpisize, Int $3) }
  | Vpirange COLON Range COLON range_def Indent range_lst Unindent { to_tuple Vpirange $7 }
  | Vpinettype COLON Int { TUPLE2(Vpinettype, net_type $3) }
  | Vpitypespec COLON ref_typespec { TUPLE2(Vpitypespec, $3) }
  | Vpicontassign COLON cont_assign { $3 }
  | Vpiprocess COLON process { TUPLE2(Vpiprocess, $3) }
  | Vpiarraynet COLON arraynettyp { TUPLE2(Vpinet, $3) }
  | Vpimodule COLON module_inst { TUPLE2(Vpimodule, $3) }

gen_region_lst: { [] }
  | gen_region_opt gen_region_lst { $1 :: $2 }

gen_region_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpistmt COLON stmt { $3 }

gen_case_lst: { [] }
  | gen_case_opt gen_case_lst { $1 :: $2 }

gen_case_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpicondition COLON oexpr { TUPLE2(Vpicondition, $3) }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { $5 }
  | Vpifullname COLON fullnam { $3 }
  | Vpicondition COLON oexpr { TUPLE2(Vpicondition, $3) }
  | Vpistmt COLON stmt { $3 }
  | Vpicaseitem COLON stmt { TUPLE2(Vpicaseitem, $3) }

parameter_def: COLON LPAREN Work AT pth_lst RPAREN loc { $7 }

process:
  | Always COLON loc Indent always_lst always_typ Unindent { loc_cache Always $3 (to_tuple (TUPLE2(Always,$6)) $5) }
  | Initial COLON loc Indent initial_lst Unindent { loc_cache Initial $3 (to_tuple Initial $5) }
  | Always COLON loc { TUPLE2 (Always, $3) }
  | Initial COLON loc { TUPLE2 (Initial, $3) }

cont_assign:
  | Cont_assign COLON loc Indent cont_assign_lst Unindent { loc_cache Cont_assign $3 (to_tuple Cont_assign $5) }
  | Cont_assign COLON loc { TUPLE2 (Cont_assign, $3) }

always_typ:
  | Vpialwaystype COLON Int { TUPLE2(Vpialwaystype, net_type $3) }

always_lst: { [] }
  | always_opt always_lst { $1 :: $2 }

always_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { Vpiname }
  | Vpifullname COLON fullnam { $3 }
  | Vpistmt COLON stmt { $3 }

initial_typ:
  | Vpialwaystype COLON Int {  TUPLE2(Vpialwaystype, Int $3) }

initial_lst: { [] }
  | initial_opt initial_lst { $1 :: $2 }

initial_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { Vpiname }
  | Vpifullname COLON fullnam { $3 }
  | Vpistmt COLON stmt { $3 }

attr:
  | Attribute COLON attribute_def Indent attribute_lst Unindent { to_tuple Attribute $5 }

attribute_lst: { [] }
  | attribute_opt attribute_lst { $1 :: $2 }

attribute_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | STRING_CONST { STRING_CONST $1 }

stmt:
  | Event_control COLON event_control_def Indent event_control_lst Unindent { to_tuple Event_control $5 }
  | Begin COLON begin_def Indent begin_lst Unindent { TUPLE3 (Begin, $3, TLIST $5) }
  | Named_begin COLON named_begin_def Indent named_begin_lst Unindent { TUPLE3 (Named_begin, $3, TLIST $5) }
  | Assignment COLON assignment_def Indent assignment_lst Unindent { to_tuple Assignment $5 }
  | If_stmt COLON loc Indent if_stmt_opt Unindent { $5 }
  | If_else COLON loc Indent if_else_opt Unindent { $5 }
  | For_stmt COLON for_stmt_def Indent for_stmt_lst Unindent { to_tuple For_stmt $5 }
  | Case_stmt COLON loc Indent case_stmt_lst Unindent { TUPLE2 (Case_stmt, TLIST $5) }
  | Case_item COLON loc Indent case_item_lst Unindent { to_tuple Case_item $5 }
  | Task_call COLON task_call_def Indent task_call_lst Unindent { to_tuple Task_call $5 }
  | Gen_if_else COLON loc Indent gen_if_else_lst Unindent { to_tuple If_else $5 }
  | Ref_module COLON ref_module_def Indent ref_module_lst Unindent { TUPLE3 (Ref_module, $3, TLIST $5) }
  | Always COLON loc Indent always_lst always_typ Unindent { loc_cache Always $3  (to_tuple (TUPLE2(Always,$6)) $5) }
  | cont_assign { $1 }

ref_module_lst: { [] }
  | ref_module_opt ref_module_lst { $1 :: $2 }

ref_module_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { Vpiname }
  | Vpifullname COLON fullnam { $3 }
  | Vpidefname COLON def_name { $3 }
  | Vpiport COLON vport { TUPLE2(Vpiport, $3) }
  | Vpiparameter COLON Parameter parameter_def Indent parameter_lst Unindent { loc_cache Parameter $4 (to_tuple Parameter $6) }
  | Vpiparamassign COLON Param_assign COLON loc Indent param_assign_lst Unindent { TUPLE2(Vpiparamassign, TLIST $7) }
  | Vpiactual COLON Module_inst COLON module_inst_def { TUPLE2(Vpiactual, $5) }

case_item_lst: { [] }
  | case_item_opt case_item_lst { $1 :: $2 }

case_item_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { $5 }
  | Vpifullname COLON fullnam { $3 }
  | Vpiexpr COLON oexpr { $3 }
  | Vpistmt COLON stmt { $3 }

case_stmt_lst: { [] }
  | case_stmt_opt case_stmt_lst { $1 :: $2 }

case_stmt_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { $5 }
  | Vpifullname COLON fullnam { $3 }
  | Vpicondition COLON oexpr { TUPLE2(Vpicondition, $3) }
  | Vpistmt COLON stmt { $3 }
  | Vpicaseitem COLON stmt { $3 }
  | Vpiattribute COLON attr { TUPLE2(Vpiattribute, $3) }
  | Vpicasetype COLON Int { TUPLE2(Vpicasetype, Int $3) }

for_stmt_lst: { [] }
  | for_stmt_opt for_stmt_lst { $1 :: $2 }

for_stmt_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { $5 }
  | Vpifullname COLON fullnam { $3 }
  | Vpicondition COLON oexpr { TUPLE2(Vpicondition, $3) }
  | Vpistmt COLON stmt { $3 }
  | Vpiforinitstmt COLON stmt { TUPLE2(Vpiforinitstmt, $3) }
  | Vpiforincstmt COLON stmt { TUPLE2(Vpiforincstmt, $3) }

if_stmt_opt:
  | Vpiparent COLON parent Vpicondition COLON oexpr Vpistmt COLON stmt { TUPLE3(If_stmt, TUPLE2(Vpicondition, $6), $9 ) }

if_else_opt:
  | Vpiparent COLON parent Vpicondition COLON oexpr Vpistmt COLON stmt Vpielsestmt COLON stmt { TUPLE4(If_else, TUPLE2(Vpicondition, $6), $9, $12) }

gen_if_else_lst: { [] }
  | gen_if_else_opt gen_if_else_lst { $1 :: $2 }

gen_if_else_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { $5 }
  | Vpifullname COLON fullnam { $3 }
  | Vpicondition COLON oexpr { TUPLE2(Vpicondition, $3) }
  | Vpistmt COLON stmt { $3 }
  | Vpielsestmt COLON stmt { TUPLE2(Vpielsestmt, $3) }

begin_lst: { [] }
  | begin_opt begin_lst { $1 :: $2 }

begin_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { Vpiname }
  | Vpifullname COLON fullnam { $3 }
  | Vpistmt COLON stmt { $3 }

named_begin_lst: { [] }
  | named_begin_opt named_begin_lst { $1 :: $2 }

named_begin_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { Vpiname }
  | Vpifullname COLON fullnam { $3 }
  | Vpistmt COLON stmt { $3 }
  | Vpivariables COLON ret { TUPLE2(Vpivariables,$3) }  

assignment_lst: { [] }
  | assignment_opt assignment_lst { $1 :: $2 }

assignment_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | dyadic { $1 }
  | Vpirhs { Vpirhs }
  | Vpiblocking COLON Int { TUPLE2(Vpiblocking, Int $3) }
  | Vpiname COLON Work AT name { Vpiname }
  | Vpifullname COLON fullnam { $3 }
  | Vpirhs COLON rhs { TUPLE2(Vpirhs, $3) }
  | Vpilhs COLON oexpr { TUPLE2(Vpilhs, $3) }

cont_assign_lst: { [] }
  | cont_assign_opt cont_assign_lst { $1 :: $2 }

cont_assign_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpinetdeclassign COLON Int { TUPLE2(Vpinetdeclassign, Int $3) }
  | Vpirhs COLON rhs { TUPLE2(Vpirhs, $3) }
  | Vpilhs COLON oexpr { TUPLE2(Vpilhs, $3) }

param_assign_lst: { [] }
  | param_assign_opt param_assign_lst { $1 :: $2 }

param_assign_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { Vpiname }
  | Vpifullname COLON fullnam { $3 }
  | Vpistmt COLON stmt { $3 }
  | Vpioptype COLON Int { TUPLE2(Vpioptype, optype $3) }
  | Vpirhs COLON rhs { TUPLE2(Vpirhs, $3) }
  | Vpilhs COLON lhs { TUPLE2(Vpilhs, $3) }
  | Vpioverriden COLON Int { TUPLE2(Vpioverriden, Int $3) }

lhs:
  | Parameter parameter_def { TUPLE2 (Parameter, $2) }

rhs:
  | Var_select COLON var_select_def Indent var_select_lst Unindent { to_tuple Var_select $5 }
  | Constant COLON constant_def Indent constant_lst Unindent { loc_cache Constant $3 (to_tuple Constant $5) }
  | Constant COLON constant_def { TUPLE2(Constant, $3) }
  | oexpr { $1 }

var_select_lst: { [] }
  | var_select_opt var_select_lst { $1 :: $2 }

var_select_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { Vpiname }
  | Vpifullname COLON fullnam { $3 }
  | Vpiindex COLON oexpr { TUPLE2(Vpiindex, $3) }
  | Vpiactual COLON parent { TUPLE2(Vpiactual, $3) }

event_control_lst: { [] }
  | event_control_opt event_control_lst { $1 :: $2 }

event_control_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { $5 }
  | Vpifullname COLON fullnam { $3 }
  | Vpicondition COLON oexpr { TUPLE2(Vpicondition, $3) }
  | Vpistmt COLON stmt { $3 }

oexpr:
  | Operation COLON operation_def Indent operation_opt Unindent { $5 }
  | Ref_obj COLON ref_obj_def Indent ref_obj_lst Unindent { to_tuple Ref_obj $5 }
  | Ref_var COLON ref_var_def Indent ref_var_lst Unindent { to_tuple Ref_var $5 }
  | Constant COLON constant_def Indent constant_lst Unindent { to_tuple Constant $5 }
  | Part_select COLON part_select_def Indent part_select_lst Unindent { loc_cache Part_select $3 (to_tuple Part_select $5) }
  | Indexed_part_select COLON indexed_part_select_def Indent indexed_part_select_lst Unindent { to_tuple Indexed_part_select $5 }
  | Bit_select COLON bit_sel_def Indent bit_sel_lst Unindent { to_tuple Bit_select $5 }
  | Sys_func_call COLON sys_func_call_def Indent sys_func_call_lst Unindent { to_tuple Sys_func_call $5 }
  | Logic_net COLON logic_net_def { TUPLE2 (Logic_net, $3) }
  | nettyp { $1 }
  | Array_var COLON array_var_def Indent array_var_lst Unindent { to_tuple Array_var $5 }
  
sys_func_call_lst: { [] }
  | sys_func_call_opt sys_func_call_lst { $1 :: $2 }

sys_func_call_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpidefname COLON def_name { $3 }
  | Vpiargument COLON oexpr { $3 }

part_select_lst: { [] }
  | part_select_opt part_select_lst { $1 :: $2 }

part_select_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { TUPLE2(Vpiname, $3) }
  | Vpifullname COLON fullnam { TUPLE2(Vpifullname, $3) }
  | Vpidefname COLON def_name { TUPLE2(Vpidefname, $3) }
  | Vpiconstantselect COLON Int { TUPLE2(Vpiconstantselect, Int $3) }
  | Vpileftrange COLON oexpr { TUPLE2(Vpileftrange, $3) }
  | Vpirightrange COLON oexpr { TUPLE2(Vpirightrange, $3) }

indexed_part_select_lst: { [] }
  | indexed_part_select_opt indexed_part_select_lst { $1 :: $2 }

indexed_part_select_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpifullname COLON fullnam { $3 }
  | Vpidefname COLON def_name { $3 }
  | Vpiconstantselect COLON Int { Int $3 }
  | Vpiindexedpartselecttype COLON Int { Int $3 }
  | Vpibaseexpr COLON oexpr { TUPLE2(Vpileftrange, $3) }
  | Vpiwidthexpr COLON oexpr { TUPLE2(Vpirightrange, $3) }

operation_opt:
  | Vpiparent COLON parent unary Vpioperand COLON oexpr { TUPLE2($4, $7) }
  | Vpiparent COLON parent dyadic Vpioperand COLON oexpr Vpioperand COLON oexpr attr_opt {
      TUPLE4($4, $11, $7, $10) }
  | Vpiparent COLON parent Vpiconditionop Vpioperand COLON oexpr Vpioperand COLON oexpr Vpioperand COLON oexpr { TUPLE4(Vpiconditionop, $7, $10, $13) }
  | Vpiparent COLON parent concatop concat_lst { TUPLE2($4, TLIST $5 ) }
  | Vpiparent COLON parent Vpinullop { Vpinullop }

attr_opt : { STRING_CONST "" }
  | Vpiattribute COLON attr { match $3 with
          | TUPLE3(Attribute, STRING "mode", (STRING_CONST _ as mode)) -> mode
	  | TUPLE3(Attribute, STRING str, _) -> failwith ("Invalid attribute: "^str)
	  | _ -> STRING_CONST "" }
  
concat_lst: Vpioperand COLON oexpr { [ $3 ] }
  | Vpioperand COLON oexpr concat_lst { $3 :: $4 }

concatop:
  | Vpiconcatop { Vpiconcatop }
  | Vpimulticoncatop { Vpimulticoncatop }

unary:
  | Vpiposedgeop { Vpiposedgeop }
  | Vpinegedgeop { Vpinegedgeop }
  | Vpiunaryandop { Vpiunaryandop }
  | Vpiunarynandop { Vpiunarynandop }
  | Vpiunaryorop { Vpiunaryorop }
  | Vpiunarynorop { Vpiunarynorop }
  | Vpiunaryxorop { Vpiunaryxorop }
  | Vpiunaryxnorop { Vpiunaryxnorop }
  | Vpibitnegop { Vpibitnegop }
  | Vpiplusop { Vpiplusop }
  | Vpiminusop { Vpiminusop }
  | Vpinotop { Vpinotop }

dyadic:
  | Vpieventorop { Vpieventorop }
  | Vpiaddop { Vpiaddop }
  | Vpisubop { Vpisubop }
  | Vpimultop { Vpimultop }
  | Vpidivop { Vpidivop }
  | Vpimodop { Vpimodop }
  | Vpilshiftop { Vpilshiftop }
  | Vpirshiftop { Vpirshiftop }
  | Vpiarithlshiftop { Vpiarithlshiftop }
  | Vpiarithrshiftop { Vpiarithrshiftop }
  | Vpilogandop { Vpilogandop }
  | Vpilogorop { Vpilogorop }
  | Vpibitandop { Vpibitandop }
  | Vpibitorop { Vpibitorop }
  | Vpibitxorop { Vpibitxorop }
  | Vpibitxnorop { Vpibitxnorop }
  | Vpieqop { Vpieqop }
  | Vpineqop { Vpineqop }
  | Vpiltop { Vpiltop }
  | Vpileop { Vpileop }
  | Vpigeop { Vpigeop }
  | Vpigtop { Vpigtop }
  | Vpipowerop { Vpipowerop }
  
vport:
  | Port COLON port_def Indent io_decl_lst Unindent { loc_cache Port $3 (to_tuple Port $5) }
  | Port COLON port_def { TUPLE2 (Port, $3) }

nettyp: 
  | Logic_net COLON logic_net_def { TUPLE2 (Logic_net, $3) }
  | Logic_net COLON logic_net_def Indent logic_net_lst Unindent { loc_cache Logic_net $3 (to_tuple Logic_net $5) }

logic_net_lst: { [] }
  | logic_net_opt logic_net_lst { $1 :: $2 }

logic_net_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { TUPLE2(Vpiname, $3) }
  | Vpiname COLON Work AT name { TUPLE2(Vpiname, $5) }
  | Vpifullname COLON fullnam { TUPLE2(Vpifullname, $3) }
  | Vpinettype COLON Int { TUPLE2(Vpinettype, net_type $3) }
  | Vpitypespec COLON ref_typespec { TUPLE2(Vpitypespec, $3) }
  | Vpisigned COLON Int { Vpisigned }

array_var_lst: { [] }
  | array_var_opt array_var_lst { $1 :: $2 }

array_var_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { Vpiname }
  | Vpifullname COLON fullnam { $3 }
  | Vpinettype COLON Int { TUPLE2(Vpinettype, net_type $3) }
  | Vpitypespec COLON ref_typespec { TUPLE2(Vpitypespec, $3) }
  | Vpisigned COLON Int { Vpisigned }
  | Vpiarraytype COLON Int { Vpiarraytype }
  | Vpireg COLON ret { TUPLE2(Vpireg, $3) }
  | Vpisize COLON Int { TUPLE2(Vpisize, Int $3) }
  | Vpirandtype COLON Int { Vpirandtype }
  | Vpivisibility COLON vis { TUPLE2(Vpivisibility, $3) }
  | Vpirange COLON Range COLON range_def Indent range_lst Unindent { to_tuple Vpirange $7 }

arraynettyp:
  | Array_net COLON array_net_def Indent array_net_lst Unindent { to_tuple Array_net $5 }

array_net_lst: { [] }
  | array_net_opt array_net_lst { $1 :: $2 }

array_net_opt:
  | Vpiparent COLON parent { Vpiparent }
  | Vpiname COLON vnam { $3 }
  | Vpiname COLON Work AT name { Vpiname }
  | Vpifullname COLON fullnam { $3 }
  | Vpisize COLON Int { TUPLE2(Vpisize, Int $3) }
  | Vpirange COLON Range COLON range_def Indent range_lst Unindent { to_tuple Vpirange $7 }
  | Vpinettype COLON Int { TUPLE2(Vpinettype, net_type $3) }
  | Vpitypespec COLON ref_typespec { TUPLE2(Vpitypespec, $3) }
  | Vpinet COLON nettyp { TUPLE2(Vpinet, $3) }

ret:
  | Int_var COLON int_var_def Indent int_var_lst Unindent { to_tuple Int_var $5 }
  | Integer_var COLON integer_var_def Indent integer_var_lst Unindent { to_tuple Int_var $5 }
  | Logic_var COLON logic_var_def Indent logic_var_lst Unindent { loc_cache Logic_var $3 (to_tuple Logic_var $5) }
  | Class_var COLON class_var_def Indent class_var_lst Unindent { to_tuple Class_var $5 }
  | Enum_var COLON enum_var_def Indent enum_var_lst Unindent { to_tuple Enum_var $5 }
  | Array_var COLON array_var_def Indent array_var_lst Unindent { to_tuple Array_var $5 }
  
def_name: Builtin { Builtin }
  | Work AT STRING { STRING $3 }
  | STRING { STRING $1 }
  | Work AT pth_lst type_lst { TLIST (List.rev $3) }

name: STRING { STRING $1 }
  | Mailbox { Mailbox }
  | Process { Process }
  | Semaphore { Semaphore }

gen_scope_def:
  | LPAREN Work AT pth_lst RPAREN loc { Work }

gen_scope_array_def:
  | LPAREN Work AT pth_lst RPAREN loc { Work }

class_def: LPAREN Builtin COLON COLON vnam RPAREN { Work }
  | LPAREN Work AT name RPAREN COMMA File COLON path loc { Work }
  | LPAREN Work AT vnam RPAREN { Work }
  | LPAREN Work AT vnam RPAREN COMMA File COLON path loc { Work }

path_def:
  | Work AT pth_lst type_lst { TLIST $3 }
  
module_inst_def: { Work }
  | path_def LPAREN Work AT pth_lst RPAREN COMMA File COLON path loc { $1 }
  
function_def: LPAREN Builtin COLON COLON vnam RPAREN { Work }
  | LPAREN Work AT name type_lst RPAREN loc { Work }

logic_net_def:
  | LPAREN Work AT pth_lst type_lst RPAREN loc { $7 }

array_net_def: { Work }
  | LPAREN Work AT pth_lst type_lst RPAREN loc { Work }

var_select_def: { Work }
  | LPAREN Work AT pth_lst RPAREN loc { Work }

part_select_def: { Work }
  | STRING LPAREN Work AT pth_lst RPAREN loc { $7 }
  | STRING LPAREN pth_lst RPAREN loc { $5 }

indexed_part_select_def: { Work }
  | STRING LPAREN Work AT pth_lst RPAREN loc { Work }

sys_func_call_def: { Work }
  | LPAREN STRING RPAREN loc { Work }

begin_def: { Work }
  | LPAREN Work AT pth_lst RPAREN loc { TLIST $4 }
  | LPAREN pth_lst RPAREN loc { TLIST $2 }

named_begin_def: { Work }
  | LPAREN Work AT pth_lst RPAREN loc { TLIST $4 }
  | LPAREN STRING RPAREN { STRING $2 }

for_stmt_def: { Work }
  | LPAREN Work AT pth_lst RPAREN loc { Work }

attribute_def: { Work }
  | LPAREN STRING RPAREN loc { Work }

ref_obj_def: LPAREN pth_lst RPAREN loc { TLIST $2 }
  | LPAREN Work AT pth_lst RPAREN loc { Work }

ref_var_def: { Work }
  | LPAREN Work AT pth_lst RPAREN loc { Work }

bit_sel_def: { Work }
  | LPAREN Work AT pth_lst RPAREN loc { Work }
  | LPAREN pth_lst RPAREN loc { Work }

pth_lst: STRING { [STRING $1] }
  | STRING DOT pth_lst { STRING $1 :: $3 }
  
port_def: loc { $1 }
  | LPAREN STRING RPAREN loc { $4 }
  
enum_var_def: LPAREN Builtin COLON COLON vnam RPAREN { Work }
| LPAREN Work AT name type_lst RPAREN loc { Work }
| LPAREN Work AT pth_lst RPAREN loc { Work }

int_var_def: { Work }
  | LPAREN Work AT name type_lst RPAREN loc { Work }

integer_var_def: { Work }
  | LPAREN Work AT name type_lst RPAREN loc { Work }
  | LPAREN Work AT pth_lst RPAREN loc { Work }

logic_var_def:
  | LPAREN Work AT name type_lst RPAREN { Work }
  | LPAREN Work AT pth_lst RPAREN loc { $6 }
  | LPAREN pth_lst RPAREN loc { $4 }

array_var_def: { Work }
  | LPAREN Work AT pth_lst RPAREN loc { $6 }
  | LPAREN pth_lst RPAREN loc { Work }
  
task_def: { Work }
  | LPAREN Work AT name type_lst RPAREN loc { Work }
  | LPAREN Work AT pth_lst RPAREN loc { Work }
  
task_call_def: { Work }
  | LPAREN STRING RPAREN loc { Work }
  
constant_def: { Work }
  | loc { $1 }

operation_def: { Work }
  | loc { Work }

event_control_def: { Work }
  | loc { Work }

assignment_def: { Work }
  | loc { Work }

int_typespec_def: { Work }
  | loc { Work }

integer_typespec_def: { Work }
  | loc { Work }
  
range_def: { Work }
  | loc { Work }
  
io_decl_def:
  | LPAREN vnam RPAREN loc { Work }
  
enum_typespec_decl: { Work }
  | LPAREN vnam RPAREN loc { $4 }
  
enum_const_decl:
  | LPAREN vnam RPAREN loc { $4 }
  
type_spec: { Work }
  | LPAREN Work AT name type_lst RPAREN { $4 }
  | LPAREN Work AT pth_lst RPAREN { TLIST $4 }
  | LPAREN pth_lst RPAREN { TLIST $2 }

type_lst: { [] }
  | COLON COLON vnam type_lst { $3 :: $4 }
  
class_typespec: { Work }
  | loc { Work }
  
logic_typespec_def:
  | loc { $1 }

path: { [] }
  | DOT path { DOT :: $2}
  | HYPHEN path { DOT :: $2}
  | SLASH path { SLASH :: $2}
  | STRING path { STRING $1 :: $2}
  | Int path { Int $1 :: $2 }
  | Builtin path { Builtin :: $2}
  | Restored path { Builtin :: $2}
  | DESIGN path { Builtin :: $2}
  | Pre_Elab path { Builtin :: $2 }
  | Post_Elab path { Builtin :: $2 }
  | Mailbox path { Mailbox :: $2 }
  | Process path { Process :: $2 }
  | Semaphore path { Semaphore :: $2 }
  
vis: { STRING "" }
  | Int { Int $1 }

vnam: { STRING "" }
  | STRING { STRING $1 }
  | New { New }
  | Num { Num }
  | Get { Get }
  | Try_get { Try_get }
  | Put { Put }
  | Peek { Peek }
  | Try_peek { Try_peek }
  | Try_put { Try_put }
  | Bound { Bound }
  | Message { Message }
  | Builtin { Builtin }
  | Array { Array }
  | Queue { Queue }
  | String { String }
  | System { System }
  | Any_sverilog_class { Any_sverilog_class }
  | State { State }
  | Status { Status }
  | Self { Self }
  | Kill { Kill }
  | Await { Await }
  | Suspend { Suspend }
  | Resume { Resume }
  | Keycount { Keycount }
  | FINISHED { FINISHED }
  | RUNNING { RUNNING }
  | WAITING { WAITING }
  | SUSPENDED { SUSPENDED }
  | KILLED { KILLED }
  
fullnam: Builtin COLON COLON vnam { $4 }
  | Work AT name type_lst { TLIST (List.rev $4) }
  | Work AT pth_lst type_lst { TLIST (List.rev $3) }
  | pth_lst { TLIST (List.rev $1) }
  
input_lst: { [] }
  | input_lst input { $2 :: $1 }
  
unreachable:
| ACCEPT { Work }
| AMPERSAND { Work }
| BACKQUOTE { Work }
| BACKSLASH { Work }
| CARET { Work }
| CONS1 { Work }
| CONS2 { Work }
| CONS3 { Work }
| CONS4 { Work }
| Clk { Work }
| DEFAULT { Work }
| DOLLAR { Work }
| DOUBLEQUOTE { Work }
| ELIST { Work }
| EMPTY_TOKEN { Work }
| END { Work }
| ERROR { Work }
| ERROR_TOKEN { Work }
| GREATER { Work }
| HASH { Work }
| LBRACE { Work }
| LBRACK { Work }
| LESS { Work }
| LINEFEED { Work }
| PERCENT { Work }
| PLING { Work }
| Post_Elab { Work }
| Pre_Elab { Work }
| QUERY { Work }
| QUOTE { Work }
| RBRACE { Work }
| RBRACK { Work }
| Restored { Work }
| SLIST { Work }
| TILDE { Work }
| TLIST { Work }
| TUPLE10 { Work }
| TUPLE2 { Work }
| TUPLE3 { Work }
| TUPLE4 { Work }
| TUPLE5 { Work }
| TUPLE6 { Work }
| TUPLE7 { Work }
| TUPLE8 { Work }
| TUPLE9 { Work }
| UNDERSCORE { Work }
| VBAR { Work }

