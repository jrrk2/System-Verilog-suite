open Eparse
open Edif2
open Printf

let dummy = ITEM2
 (Edif, TLIST [ID "DUMMY"],
  TLIST
   [ITEM (Edifversion, TLIST [INT 2; INT 0; INT 0]);
    ITEM (EdifLevel, TLIST [INT 0]);
    ITEM2 (Keywordmap, TLIST [], TLIST [ITEM (Keywordlevel, TLIST [INT 0])]);
    ITEM2
     (Status, TLIST [],
      TLIST
       [ITEM2
         (Written, TLIST [],
          TLIST
           [ITEM
             (TimeStamp,
              TLIST [INT 2020; INT 6; INT 14; INT 14; INT 17; INT 15]);
            ITEM2
             (Program, TLIST [STRING "\"Vivado\""],
              TLIST [ITEM (ID "Version", TLIST [STRING "\"2019.2\""])]);
            ITEM
             (Comment,
              TLIST [STRING "\"Built on 'Wed Nov  6 21:39:14 MST 2019'\""]);
            ITEM (Comment, TLIST [STRING "\"Built by 'xbuild'\""])])]);
    ITEM2
     (Library, TLIST [ID "hdi_primitives"],
      TLIST
       [ITEM (EdifLevel, TLIST [INT 0]);
        ITEM2
         (Technology, TLIST [], TLIST [ITEM (NumberDefinition, TLIST [])]);
        ITEM2
         (Cell, TLIST [ID "GND"],
          TLIST
           [ITEM (Celltype, TLIST [ID "GENERIC"]);
            ITEM2
             (View, TLIST [ID "netlist"],
              TLIST
               [ITEM (Viewtype, TLIST [ID "NETLIST"]);
                ITEM2
                 (Interface, TLIST [],
                  TLIST
                   [ITEM2
                     (Port, TLIST [ID "G"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Output])])])])]);
        ITEM2
         (Cell, TLIST [ID "VCC"],
          TLIST
           [ITEM (Celltype, TLIST [ID "GENERIC"]);
            ITEM2
             (View, TLIST [ID "netlist"],
              TLIST
               [ITEM (Viewtype, TLIST [ID "NETLIST"]);
                ITEM2
                 (Interface, TLIST [],
                  TLIST
                   [ITEM2
                     (Port, TLIST [ID "P"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Output])])])])]);
        ITEM2
         (Cell, TLIST [ID "INV"],
          TLIST
           [ITEM (Celltype, TLIST [ID "GENERIC"]);
            ITEM2
             (View, TLIST [ID "netlist"],
              TLIST
               [ITEM (Viewtype, TLIST [ID "NETLIST"]);
                ITEM2
                 (Interface, TLIST [],
                  TLIST
                   [ITEM2
                     (Port, TLIST [ID "I"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "O"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Output])])])])])]);
    ITEM2
     (Library, TLIST [ID "work"],
      TLIST
       [ITEM (EdifLevel, TLIST [INT 0]);
        ITEM2
         (Technology, TLIST [], TLIST [ITEM (NumberDefinition, TLIST [])]);
        ITEM2
         (Cell, TLIST [ID "GTECH_BUF"],
          TLIST
           [ITEM (Celltype, TLIST [ID "GENERIC"]);
            ITEM2
             (View, TLIST [ID "GTECH_BUF"],
              TLIST
               [ITEM (Viewtype, TLIST [ID "NETLIST"]);
                ITEM2
                 (Interface, TLIST [],
                  TLIST
                   [ITEM2
                     (Port, TLIST [ID "A"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "Z"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Output])])]);
                ITEM2
                 (Property, TLIST [ID "XLNX_LINE_COL"],
                  TLIST [ITEM (Integer, TLIST [INT 461056])])])]);
        ITEM2
         (Cell, TLIST [ID "GTECH_XOR2"],
          TLIST
           [ITEM (Celltype, TLIST [ID "GENERIC"]);
            ITEM2
             (View, TLIST [ID "GTECH_XOR2"],
              TLIST
               [ITEM (Viewtype, TLIST [ID "NETLIST"]);
                ITEM2
                 (Interface, TLIST [],
                  TLIST
                   [ITEM2
                     (Port, TLIST [ID "A"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "B"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "Z"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Output])])]);
                ITEM2
                 (Property, TLIST [ID "XLNX_LINE_COL"],
                  TLIST [ITEM (Integer, TLIST [INT 461312])])])]);
        ITEM2
         (Cell, TLIST [ID "GTECH_OR2"],
          TLIST
           [ITEM (Celltype, TLIST [ID "GENERIC"]);
            ITEM2
             (View, TLIST [ID "GTECH_OR2"],
              TLIST
               [ITEM (Viewtype, TLIST [ID "NETLIST"]);
                ITEM2
                 (Interface, TLIST [],
                  TLIST
                   [ITEM2
                     (Port, TLIST [ID "A"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "B"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "Z"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Output])])]);
                ITEM2
                 (Property, TLIST [ID "XLNX_LINE_COL"],
                  TLIST [ITEM (Integer, TLIST [INT 461056])])])]);
        ITEM2
         (Cell, TLIST [ID "GTECH_AND2"],
          TLIST
           [ITEM (Celltype, TLIST [ID "GENERIC"]);
            ITEM2
             (View, TLIST [ID "GTECH_AND2"],
              TLIST
               [ITEM (Viewtype, TLIST [ID "NETLIST"]);
                ITEM2
                 (Interface, TLIST [],
                  TLIST
                   [ITEM2
                     (Port, TLIST [ID "A"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "B"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "Z"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Output])])]);
                ITEM2
                 (Property, TLIST [ID "XLNX_LINE_COL"],
                  TLIST [ITEM (Integer, TLIST [INT 461312])])])]);
        ITEM2
         (Cell, TLIST [ID "GTECH_AND3"],
          TLIST
           [ITEM (Celltype, TLIST [ID "GENERIC"]);
            ITEM2
             (View, TLIST [ID "GTECH_AND3"],
              TLIST
               [ITEM (Viewtype, TLIST [ID "NETLIST"]);
                ITEM2
                 (Interface, TLIST [],
                  TLIST
                   [ITEM2
                     (Port, TLIST [ID "A"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "B"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "C"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "Z"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Output])])]);
                ITEM2
                 (Property, TLIST [ID "XLNX_LINE_COL"],
                  TLIST [ITEM (Integer, TLIST [INT 461312])])])]);
        ITEM2
         (Cell, TLIST [ID "SELECT_OP"],
          TLIST
           [ITEM (Celltype, TLIST [ID "GENERIC"]);
            ITEM2
             (View, TLIST [ID "SELECT_OP"],
              TLIST
               [ITEM (Viewtype, TLIST [ID "NETLIST"]);
                ITEM2
                 (Interface, TLIST [],
                  TLIST
                   [ITEM2
                     (Port, TLIST [ID "CONTROL1"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "CONTROL10"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "CONTROL11"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "CONTROL12"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "CONTROL13"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "CONTROL14"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "CONTROL15"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "CONTROL16"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "CONTROL17"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "CONTROL18"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "CONTROL19"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "CONTROL2"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "CONTROL20"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "CONTROL21"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "CONTROL22"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "CONTROL23"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "CONTROL24"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "CONTROL25"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "CONTROL26"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "CONTROL27"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "CONTROL28"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "CONTROL29"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "CONTROL3"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "CONTROL30"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "CONTROL4"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "CONTROL5"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "CONTROL6"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "CONTROL7"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "CONTROL8"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "CONTROL9"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename,
                            TLIST [ID "DATA1"; STRING "\"DATA1[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename,
                            TLIST [ID "DATA10"; STRING "\"DATA10[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename,
                            TLIST [ID "DATA11"; STRING "\"DATA11[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename,
                            TLIST [ID "DATA12"; STRING "\"DATA12[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename,
                            TLIST [ID "DATA13"; STRING "\"DATA13[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename,
                            TLIST [ID "DATA14"; STRING "\"DATA14[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename,
                            TLIST [ID "DATA15"; STRING "\"DATA15[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename,
                            TLIST [ID "DATA16"; STRING "\"DATA16[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename,
                            TLIST [ID "DATA17"; STRING "\"DATA17[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename,
                            TLIST [ID "DATA18"; STRING "\"DATA18[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename,
                            TLIST [ID "DATA19"; STRING "\"DATA19[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename,
                            TLIST [ID "DATA2"; STRING "\"DATA2[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename,
                            TLIST [ID "DATA20"; STRING "\"DATA20[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename,
                            TLIST [ID "DATA21"; STRING "\"DATA21[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename,
                            TLIST [ID "DATA22"; STRING "\"DATA22[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename,
                            TLIST [ID "DATA23"; STRING "\"DATA23[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename,
                            TLIST [ID "DATA24"; STRING "\"DATA24[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename,
                            TLIST [ID "DATA25"; STRING "\"DATA25[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename,
                            TLIST [ID "DATA26"; STRING "\"DATA26[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename,
                            TLIST [ID "DATA27"; STRING "\"DATA27[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename,
                            TLIST [ID "DATA28"; STRING "\"DATA28[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename,
                            TLIST [ID "DATA29"; STRING "\"DATA29[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename,
                            TLIST [ID "DATA3"; STRING "\"DATA3[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename,
                            TLIST [ID "DATA30"; STRING "\"DATA30[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename,
                            TLIST [ID "DATA4"; STRING "\"DATA4[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename,
                            TLIST [ID "DATA5"; STRING "\"DATA5[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename,
                            TLIST [ID "DATA6"; STRING "\"DATA6[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename,
                            TLIST [ID "DATA7"; STRING "\"DATA7[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename,
                            TLIST [ID "DATA8"; STRING "\"DATA8[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename,
                            TLIST [ID "DATA9"; STRING "\"DATA9[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename, TLIST [ID "Z"; STRING "\"Z[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Output])])]);
                ITEM2
                 (Property, TLIST [ID "XLNX_LINE_COL"],
                  TLIST [ITEM (Integer, TLIST [INT 461056])])])]);
        ITEM2
         (Cell, TLIST [ID "GTECH_NOT"],
          TLIST
           [ITEM (Celltype, TLIST [ID "GENERIC"]);
            ITEM2
             (View, TLIST [ID "GTECH_NOT"],
              TLIST
               [ITEM (Viewtype, TLIST [ID "NETLIST"]);
                ITEM2
                 (Interface, TLIST [],
                  TLIST
                   [ITEM2
                     (Port, TLIST [ID "A"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "Z"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Output])])]);
                ITEM2
                 (Property, TLIST [ID "XLNX_LINE_COL"],
                  TLIST [ITEM (Integer, TLIST [INT 461056])])])]);
        ITEM2
         (Cell, TLIST [],
          TLIST
           [ITEM (Rename, TLIST [ID "&__SEQGEN__"; STRING "\"**SEQGEN**\""]);
            ITEM (Celltype, TLIST [ID "GENERIC"]);
            ITEM2
             (View, TLIST [],
              TLIST
               [ITEM
                 (Rename, TLIST [ID "&__SEQGEN__"; STRING "\"**SEQGEN**\""]);
                ITEM (Viewtype, TLIST [ID "NETLIST"]);
                ITEM2
                 (Interface, TLIST [],
                  TLIST
                   [ITEM2
                     (Port, TLIST [ID "Q"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Output])]);
                    ITEM2
                     (Port, TLIST [ID "clear"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "clocked_on"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "data_in"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "enable"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "next_state"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "preset"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "synch_clear"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "synch_enable"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "synch_preset"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [ID "synch_toggle"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Input])])]);
                ITEM2
                 (Property, TLIST [ID "XLNX_LINE_COL"],
                  TLIST [ITEM (Integer, TLIST [INT 985344])])])]);
        ITEM2
         (Cell, TLIST [ID "ADD_UNS_OP"],
          TLIST
           [ITEM (Celltype, TLIST [ID "GENERIC"]);
            ITEM2
             (View, TLIST [ID "ADD_UNS_OP"],
              TLIST
               [ITEM (Viewtype, TLIST [ID "NETLIST"]);
                ITEM2
                 (Interface, TLIST [],
                  TLIST
                   [ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename, TLIST [ID "A"; STRING "\"A[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename, TLIST [ID "B"; STRING "\"B[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename, TLIST [ID "Z"; STRING "\"Z[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Output])])]);
                ITEM2
                 (Property, TLIST [ID "XLNX_LINE_COL"],
                  TLIST [ITEM (Integer, TLIST [INT 461312])])])]);
        ITEM2
         (Cell, TLIST [ID "ADD_TC_OP"],
          TLIST
           [ITEM (Celltype, TLIST [ID "GENERIC"]);
            ITEM2
             (View, TLIST [ID "ADD_TC_OP"],
              TLIST
               [ITEM (Viewtype, TLIST [ID "NETLIST"]);
                ITEM2
                 (Interface, TLIST [],
                  TLIST
                   [ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename, TLIST [ID "A"; STRING "\"A[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename, TLIST [ID "B"; STRING "\"B[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename, TLIST [ID "Z"; STRING "\"Z[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Output])])]);
                ITEM2
                 (Property, TLIST [ID "XLNX_LINE_COL"],
                  TLIST [ITEM (Integer, TLIST [INT 461056])])])]);
        ITEM2
         (Cell, TLIST [ID "EQ_UNS_OP"],
          TLIST
           [ITEM (Celltype, TLIST [ID "GENERIC"]);
            ITEM2
             (View, TLIST [ID "EQ_UNS_OP"],
              TLIST
               [ITEM (Viewtype, TLIST [ID "NETLIST"]);
                ITEM2
                 (Interface, TLIST [],
                  TLIST
                   [ITEM2
                     (Port, TLIST [ID "Z"],
                      TLIST [ITEM (Direction, TLIST [Edif2.Output])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename, TLIST [ID "A"; STRING "\"A[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename, TLIST [ID "B"; STRING "\"B[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])])]);
                ITEM2
                 (Property, TLIST [ID "XLNX_LINE_COL"],
                  TLIST [ITEM (Integer, TLIST [INT 461056])])])]);
        ITEM2
         (Cell, TLIST [ID "MULT_TC_OP"],
          TLIST
           [ITEM (Celltype, TLIST [ID "GENERIC"]);
            ITEM2
             (View, TLIST [ID "MULT_TC_OP"],
              TLIST
               [ITEM (Viewtype, TLIST [ID "NETLIST"]);
                ITEM2
                 (Interface, TLIST [],
                  TLIST
                   [ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename, TLIST [ID "A"; STRING "\"A[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename, TLIST [ID "B"; STRING "\"B[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename, TLIST [ID "Z"; STRING "\"Z[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Output])])]);
                ITEM2
                 (Property, TLIST [ID "XLNX_LINE_COL"],
                  TLIST [ITEM (Integer, TLIST [INT 461312])])])]);
        ITEM2
         (Cell, TLIST [ID "SUB_TC_OP"],
          TLIST
           [ITEM (Celltype, TLIST [ID "GENERIC"]);
            ITEM2
             (View, TLIST [ID "SUB_TC_OP"],
              TLIST
               [ITEM (Viewtype, TLIST [ID "NETLIST"]);
                ITEM2
                 (Interface, TLIST [],
                  TLIST
                   [ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename, TLIST [ID "A"; STRING "\"A[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename, TLIST [ID "B"; STRING "\"B[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename, TLIST [ID "Z"; STRING "\"Z[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Output])])]);
                ITEM2
                 (Property, TLIST [ID "XLNX_LINE_COL"],
                  TLIST [ITEM (Integer, TLIST [INT 461056])])])]);
        ITEM2
         (Cell, TLIST [ID "SUB_UNS_OP"],
          TLIST
           [ITEM (Celltype, TLIST [ID "GENERIC"]);
            ITEM2
             (View, TLIST [ID "SUB_UNS_OP"],
              TLIST
               [ITEM (Viewtype, TLIST [ID "NETLIST"]);
                ITEM2
                 (Interface, TLIST [],
                  TLIST
                   [ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename, TLIST [ID "A"; STRING "\"A[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename, TLIST [ID "B"; STRING "\"B[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Input])]);
                    ITEM2
                     (Port, TLIST [],
                      TLIST
                       [ITEM2
                         (Array,
                          ITEM
                           (Rename, TLIST [ID "Z"; STRING "\"Z[207:0]\""]),
                          INT 208);
                        ITEM (Direction, TLIST [Edif2.Output])])]);
                ITEM2
                 (Property, TLIST [ID "XLNX_LINE_COL"],
                  TLIST [ITEM (Integer, TLIST [INT 461312])])])])]);
    ITEM
     (Comment, TLIST [STRING "\"Reference To The Cell Of Highest Level\""]);
    ITEM2
     (Design, TLIST [ID "DUMMY"],
      TLIST
       [ITEM2
         (Cellref, TLIST [ID "DUMMY"],
          TLIST [ITEM (Libraryref, TLIST [ID "work"])]);
        ITEM2
         (Property, TLIST [ID "part"],
          TLIST [ITEM (String, TLIST [STRING "\"xc7vx485tffg1761-2\""])])])])
