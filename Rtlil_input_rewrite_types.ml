type ilang =
| Assign_stmt67 of ilang list * ilang list
| Attr_list56 of ilang list * ilang list 
| Attr_stmt of string * ilang list
| Autoidx_stmt26 of int
| Case_body63 of ilang list * ilang list 
| Case_body64 of ilang list * ilang list 
| Case_body65 of ilang list * ilang list 
| Cell_bodyconnect of ilang list * string * ilang list * ilang list 
| Cell_bodyparam of ilang list * string * ilang list * ilang list 
| Cell_bodypreal of ilang list * string * ilang list * ilang list 
| Cell_bodypsigned of ilang list * string * ilang list * ilang list 
| Cell_stmt of string * string * ilang list * ilang list
| Compare_list61 of ilang list * ilang list
| Conn_stmt96 of ilang list * ilang list
| Design6 of ilang list * ilang list 
| Design7 of ilang list * ilang list 
| Design8 of ilang list * ilang list 
| Input2 of ilang list * ilang list 
| Memory_optionsoffset of int
| Memory_optionssize of int
| Memory_optionswidth of int
| Memory_stmt39 of ilang list * string
| Module12 of string * ilang list
| Module_body13 of ilang list * ilang list 
| Param_defval_stmt24 of string * ilang list
| Param_stmt23 of string * ilang list
| Proc_stmt of string * ilang list * ilang list * ilang list
| Signed
| Sigspec90 of string * int
| Sigspec92 of ilang list 
| Sigspec_list_reversed93 of ilang list * ilang list 
| Sigspecrange of string * int * int
| Switch_bodycase of ilang list * ilang list * ilang list 
| Switch_stmt of ilang list * ilang list * ilang list * ilang list
| Sync_list69 of ilang list * ilang list * ilang list * ilang list 
| Sync_listalways of ilang list * ilang list 
| Sync_listglobal of ilang list * ilang list 
| Sync_listinit of ilang list * ilang list 
| Update_list82 of ilang list * ilang list
| Update_listmemwr of string * ilang list * ilang list * ilang list * ilang list
| Upto
| Wire_optionsinout of int
| Wire_optionsinput of int
| Wire_optionsinvalid
| Wire_optionsoffset of int
| Wire_optionsoutput of int
| Wire_optionswidth of int
| Wire_stmt of ilang list * string
| TokCase of ilang list * ilang list
| TokConn of ilang list * ilang list
| TokParam of ilang list * ilang list
| TokUpdate of ilang list * ilang list
| TokInt of int
| TokID of string
| TokVal of string
| TokStr of string
| TokPos
| TokNeg
| TokEdge [@@deriving yojson]
