/*

    <edif2.mly - Parser for EDIF format.>
    Copyright (C) <2011,2012>  <Jonathan Richard Robert Kimmitt>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/

%token Module
%token Endmodule
%token Begin
%token End
%token Case
%token Default
%token Endcase
%token DEFAULT
%token <string> COMMENT
%token <string> MACRO
%token SEMI
%token HASH_
%token AT
%token AMPERSAND
%token VBAR
%token TILDE
%token QUERY
%token BECOMES
%token LCURLY
%token RCURLY
%token LBRACK
%token RBRACK
%token Reg
%token Wire
%token Tri
%token Input
%token Inout
%token Output
%token BEGINPRAGMA
%token ENDPRAGMA
%token EQUALS
%token Abs
%token Acload
%token After
%token Always
%token And
%token Annotate
%token Apply
%token Arc
%token Array
%token Arraymacro
%token Arrayrelatedinfo
%token Arraysite
%token Assign
%token Atleast
%token Atmost
%token Author
%token Basearray
%token Becomes
%token Between
%token Block
%token Boolean
%token Booleandisplay
%token Booleanmap
%token Booleanvalue
%token Borderpattern
%token Borderwidth
%token Boundingbox
%token Ceiling
%token Cell
%token Cellref
%token Celltype
%token Change
%token Circle
%token Color
%token Comment
%token Commentgraphics
%token Compound
%token Concat
%token Connectlocation
%token Constant
%token Constraint
%token Contents
%token Cornertype
%token Criticality
%token Currentmap
%token Curve
%token Cycle
%token Dataorigin
%token Dcfaninload
%token Dcfanoutload
%token Dcmaxfanin
%token Dcmaxfanout
%token Delay
%token Delta
%token Derivation
%token Design
%token Designator
%token Difference
%token Direction
%token Display
%token Divide
%token Dominates
%token Dot
%token Duration
%token E
%token Edif
%token EdifLevel
%token Edifversion
%token Else
%token Enclosuredistance
%token Endtype
%token Entry
%token Equal
%token Escape
%token Event
%token Exactly
%token External
%token Fabricate
%token False
%token Figure
%token Figurearea
%token Figuregroup
%token Figuregroupobject
%token Figuregroupoverride
%token Figuregroupref
%token Figureperimeter
%token Figurewidth
%token Fillpattern
%token Fix
%token Floor
%token Follow
%token Forbiddenevent
%token Form
%token Globalportref
%token Greaterthan
%token Gridmap
%token If
%token Ignore
%token Includefiguregroup
%token Increasing
%token Initial
%token Instance
%token Instancebackannotate
%token Instancegroup
%token Instancemap
%token Instancenamedef
%token Instanceref
%token Integer
%token Integerdisplay
%token Interface
%token Interfiguregroupspacing
%token Intersection
%token Intrafiguregroupspacing
%token Inverse
%token Isolated
%token Iterate
%token Joined
%token Justify
%token Keyworddisplay
%token Keywordlevel
%token Keywordmap
%token Lessthan
%token Library
%token Libraryref
%token Listofnets
%token Listofports
%token Loaddelay
%token Logicassign
%token Logicinput
%token Logiclist
%token Logicmapinput
%token Logicmapoutput
%token Logiconeof
%token Logicoutput
%token Logicport
%token Logicref
%token Logicvalue
%token Logicwaveform
%token Maintain
%token Match
%token Max
%token Member
%token Min
%token Minomax
%token Minomaxdisplay
%token Mnm
%token Mod
%token Multiplevalueset
%token Mustjoin
%token Name
%token Nand
%token Negate
%token Net
%token Netbackannotate
%token Netbundle
%token Netdelay
%token Netgroup
%token Netmap
%token Netref
%token Nochange
%token Nonpermutable
%token Nor
%token Not
%token Notallowed
%token Notchspacing
%token Number
%token NumberDefinition
%token Numberdisplay
%token Offpageconnector
%token Offsetevent
%token Openshape
%token Or
%token Orientation
%token Origin
%token Overhangdistance
%token Overlapdistance
%token Oversize
%token Owner
%token Page
%token Pagesize
%token Parameter
%token Parameterassign
%token Parameterdisplay
%token Path
%token Pathdelay
%token Pathwidth
%token Permutable
%token Physicaldesignrule
%token Plug
%token Point
%token Pointdisplay
%token Pointlist
%token Pointsubtract
%token Pointsum
%token Polygon
%token Port
%token Portbackannotate
%token Portbundle
%token Portdelay
%token Portgroup
%token Portimplementation
%token Portinstance
%token Portlist
%token Portlistalias
%token Portmap
%token Portref
%token Posedge
%token Product
%token Program
%token Property
%token Propertydisplay
%token Protectionframe
%token Pt
%token Rangevector
%token Rectangle
%token Rectanglesize
%token Rename
%token Resolves
%token Scale
%token Scalex
%token Scaley
%token Section
%token Shape
%token Simulate
%token Simulationinfo
%token Singlevalueset
%token Site
%token Socket
%token Socketset
%token Statement
%token Status
%token Steady
%token Strictlyincreasing
%token String
%token Stringdisplay
%token Strong
%token Subtract
%token Sum
%token Symbol
%token Symmetry
%token Table
%token Tabledefault
%token Technology
%token Textheight
%token Then
%token Timeinterval
%token TimeStamp
%token Timing
%token Transform
%token Transition
%token True
%token Typedvalue
%token Unconstrained
%token Undefined
%token Union
%token Unit
%token Unused
%token Userdata
%token Valuenameref
%token Variable
%token View
%token Viewlist
%token Viewmap
%token Viewref
%token Viewtype
%token Visible
%token Voltagemap
%token Wavevalue
%token Weak
%token Weakjoined
%token When
%token While
%token Written
%token Xcoord
%token Xnor
%token Xor
%token Ycoord

%token <char> ILLEGAL
%token <string>          ID              // "IDENTIFIER"
%token <token list>	TLIST
%token <token list*token list>	TLIST2
%token <token*token>	ITEM
%token <token*token*token>	ITEM2
%token <string>          STRING          // "IDENTIFIER"
%token <int> INT
%token <float> FLT
%token <string> ASCNUM
%token <token*token> DOUBLE
%token <token*token*token> TRIPLE
%token <token*token*token*token> QUADRUPLE
%token <token*token*token*token*token*token> SEXTUPLE
%token <string> IDSTR
%token ENDOFFILE
%token EOL
%token LPAREN
%token RPAREN
%token COMMA
%token COLON
%token QUOTE
%token SLASH
%token DOT
%token EMPTYEDIF

%start start

%type <token> start
%type <token> formItem

%%

identifier:     ID	{ ID $1 }
number:     	INT     { INT $1 }
   | INT QUOTE ID { INT $1 }

string:     	STRING  { STRING $1 }

start:          ENDOFFILE                               { ENDOFFILE }
	|	paranthetical				{ $1 }
	|	propertyList ENDOFFILE				{ TLIST $1 }
   |  modList ENDOFFILE { TLIST $1 }
   |  propstr ENDOFFILE { TLIST $1 }
   |  COMMA propstr ENDOFFILE { TLIST $2 }
   ;

propstr: propstr2 { [ $1 ] }
   | propstr2 COMMA propstr { $1 :: $3 }

propstr2: ID COLON ID EQUALS propvList { TRIPLE ( ID $1, ID $3, TLIST $5 ) }
   | ID COLON INT QUOTE ID { DOUBLE ( ID $1, TRIPLE (INT $3, QUOTE, ID $5)) }
   | ID COLON ID EQUALS DEFAULT { TRIPLE ( ID $1, ID $3, DEFAULT) }
   | ID COLON DEFAULT { DOUBLE ( ID $1, DEFAULT) }
   | INT QUOTE ID { TRIPLE (INT $1, QUOTE, ID $3) }
   
propvList: INT QUOTE ID { [ TRIPLE (INT $1, QUOTE, ID $3) ] }
   | INT QUOTE ID SEMI propvList { TRIPLE (INT $1, QUOTE, ID $3) :: $5 }

modList: { [] }
   | COMMENT modList { COMMENT $1 :: $2 }
   | MACRO modList { MACRO $1 :: $2 }
   | BEGINPRAGMA pragma ENDPRAGMA modList { DOUBLE (BEGINPRAGMA, $2) :: $4 }
   | Module ID LPAREN paramList RPAREN SEMI itmList Endmodule modList { QUADRUPLE (Module, ID $2, TLIST $4, TLIST $7) :: $9}
     
propertyList: { [] }
   |  Property LPAREN idpath propList RPAREN propertyList   { DOUBLE (TLIST $3, TLIST $4) :: $6}

paramList: ID { [ ID $1 ] }
   | formItem { [ $1 ] }
   | DOT ID LPAREN expr RPAREN { [ DOUBLE (ID $2, $4) ] }
   | ID COMMA paramList { ID $1 :: $3 }
   | formItem COMMA paramList { $1 :: $3 }
   | DOT ID LPAREN expr RPAREN COMMA paramList { DOUBLE (ID $2, $4) :: $7 }
   | DOT formItem LPAREN expr RPAREN COMMA paramList { DOUBLE ($2, $4) :: $7 }
      
itmList: { [ ] }
   | dir declList SEMI itmList { DOUBLE ($1, TLIST $2) :: $4 }
   | Assign maybelhs EQUALS expr SEMI itmList { TRIPLE (Assign, $2, $4) :: $6 }
   | ID actParmOpt maybeid LPAREN connList RPAREN SEMI itmList { QUADRUPLE (ID $1, TLIST $2, $3, TLIST $5) :: $8 }
   | ID actParmOpt maybeid LPAREN ordList RPAREN SEMI itmList { QUADRUPLE (ID $1, TLIST $2, $3, TLIST $5) :: $8 }
   | logItem maybeid LPAREN ordList RPAREN SEMI itmList { TRIPLE ($1, $2, TLIST $4) :: $7 }
   | BEGINPRAGMA pragma ENDPRAGMA itmList { $2 :: $4 }
   | Always itemList { Always :: $2 }

maybelhs: lhs { $1 }
   | HASH_ INT lhs { DOUBLE(INT $2, $3) }

maybeid: ID { ID $1 }
   | formItem { $1 }

pragma: ID EQUALS STRING { DOUBLE (ID $1, STRING $3) }
   | Delay EQUALS STRING { DOUBLE (Delay, STRING $3) }
   | ID EQUALS INT { DOUBLE (ID $1, INT $3) }
   | ID { ID $1 }
   
actParmOpt: { [] }
   | HASH_ LPAREN actParmList RPAREN { $3 }

actParmList: DOT ID LPAREN actParm RPAREN { [ DOUBLE ( ID $2, $4 ) ] }
   | DOT formItem LPAREN actParm RPAREN { [ DOUBLE ( $2, $4 ) ] }
   | DOT ID LPAREN actParm RPAREN COMMA actParmList { DOUBLE ( ID $2, $4 ) :: $7 }
   | DOT formItem LPAREN actParm RPAREN COMMA actParmList { DOUBLE ( $2, $4 ) :: $7 }

actParm: STRING { STRING $1 }
   | INT QUOTE ID { TRIPLE(INT $1, QUOTE, ID $3) }
   | INT { INT $1 }
   | FLT { FLT $1 }
   
connList: { [] } // In case of only implicit connections
   | DOT ID LPAREN expr RPAREN { [ DOUBLE(ID $2, $4) ] }
   | DOT formItem LPAREN expr RPAREN { [ DOUBLE($2, $4) ] }
   | DOT ID LPAREN expr RPAREN COMMA connList { DOUBLE(ID $2, $4) :: $7 }
   | DOT formItem LPAREN expr RPAREN COMMA connList { DOUBLE($2, $4) :: $7 }

ordList: expr { [ $1 ] }
   | expr COMMA ordList { $1 :: $3 }

declList: decl { [ $1 ] }
   | decl COMMA declList { $1 :: $3 }

decl: ID { ID $1 }
   | formItem { $1 }
   | LBRACK INT COLON INT RBRACK ID { TRIPLE(INT $2, INT $4, ID $6) }
   | LBRACK INT COLON INT RBRACK formItem { TRIPLE(INT $2, INT $4, $6) }
   
lhs: ID { ID $1 }
   | ID LBRACK INT COLON INT RBRACK { TRIPLE(ID $1, INT $3, INT $5) }
   | ID LBRACK INT RBRACK { DOUBLE(ID $1, INT $3) }

expr: lhs { $1 }
   | LCURLY exprList RCURLY { TLIST $2 }
   | ID LBRACK INT RBRACK { DOUBLE (ID $1, INT $3) }
   | ID QUERY expr COLON expr { QUADRUPLE(QUERY, ID $1, $3, $5) }
   | expr logop expr { TRIPLE($1, $2, $3) }
   | unaryop expr { DOUBLE($1, $2) }
   | LPAREN expr RPAREN { $2 }
   | actParm { $1 }
   | formItem { $1 }

logop: VBAR { VBAR }
   | AMPERSAND { AMPERSAND }

unaryop: TILDE { TILDE }

exprList: expr { [ $1 ] }
   | expr COMMA exprList { $1 :: $3 }
   
dir: Input  { Input }
   | Output { Output }
   | Inout  { Inout }
   | Reg    { Reg }
   | Wire   { Wire }
   | Tri    { Tri }
   
idpath: ID { [ ID $1 ] }
   | ID SLASH idpath { ID $1 :: $3 }
   | ID DOT idpath { ID $1 :: $3 }
      
propList:	{ [] }
	|	COMMA ID COLON INT QUOTE ID propList	{ DOUBLE (ID $2, STRING (string_of_int $4^"'"^$6)) :: $7 }
;

paranthetical:
                LPAREN Array paranthetical number RPAREN { ITEM2(Array, $3, $4) }
        |       LPAREN formItem itemList RPAREN          { ITEM($2, TLIST $3) }
        |       LPAREN formItem itemList subList RPAREN  { ITEM2($2, TLIST $3, TLIST $4) }
        |       LPAREN expr itemList RPAREN              { ITEM($2, TLIST $3) }
        |       Begin itemList End                       { ITEM(Begin, TLIST $2) }
        |       Case paranthetical caseList              { ITEM(Case, TLIST ($2 :: $3)) }
        ;

caseList: Endcase { [] }
	|	number COLON caseItem caseList { $1 :: $3 @ $4 }
	|	Default COLON caseItem caseList { Default :: $3 @ $4 }
;

caseItem: 
      ID BECOMES expr SEMI { ID $1 :: [ $3 ] }
   |  ID EQUALS expr SEMI { ID $1 :: [ $3 ] }
   |  If paranthetical itemList Else itemList { If :: $2 :: $3 @ Else :: $5 }
   |  If paranthetical itemList { If :: $2 :: $3 }

subList:	{ [] }
	|	paranthetical subList	{ $1 :: $2 }
;

itemList:	{ [] }
	|	number itemList		{ $1 :: $2 }
	|	number COLON itemList		{ $1 :: $3 }
	|	identifier itemList	{ $1 :: $2 }
	|	string itemList		{ $1 :: $2 }
	|	formItem itemList	{ $1 :: $2 }
	|	logItem itemList	{ $1 :: $2 }
	|  paranthetical itemList	{ $1 :: $2 }
   |  caseItem { $1 }
;

logItem:
|  And  { (   And  ) }
|  Not  { (   Not  ) }
|  Nor  { (   Nor  ) }
|  Nand { (  Nand  ) }
|  Or   { (    Or  ) }
|  Xnor { (  Xnor  ) }
|  Xor  { (   Xor  ) }

formItem:
|  dir { $1 } 
|  Abs { (   Abs  ) }
|  Acload { (   Acload  ) }
|  After { (   After  ) }
|  Annotate { (   Annotate  ) }
|  Apply { (   Apply  ) }
|  Arc { (   Arc  ) }
|  Array { (   Array  ) }
|  Arraymacro { (   Arraymacro  ) }
|  Arrayrelatedinfo { (   Arrayrelatedinfo  ) }
|  Arraysite { (   Arraysite  ) }
|  Assign { (   Assign  ) }
|  AT { (   AT  ) }
|  Atleast { (   Atleast  ) }
|  Atmost { (   Atmost  ) }
|  Author { (   Author  ) }
|  Basearray { (   Basearray  ) }
|  Becomes { (   Becomes  ) }
|  Between { (   Between  ) }
|  Block { (   Block  ) }
|  Boolean { (   Boolean  ) }
|  Booleandisplay { (   Booleandisplay  ) }
|  Booleanmap { (   Booleanmap  ) }
|  Borderpattern { (   Borderpattern  ) }
|  Borderwidth { (   Borderwidth  ) }
|  Boundingbox { (   Boundingbox  ) }
|  Ceiling { (   Ceiling  ) }
|  Cell { (   Cell  ) }
|  Cellref { (   Cellref  ) }
|  Celltype { (   Celltype  ) }
|  Change { (   Change  ) }
|  Circle { (   Circle  ) }
|  Color { (   Color  ) }
|  Comment { (   Comment  ) }
|  Commentgraphics { (   Commentgraphics  ) }
|  Compound { (   Compound  ) }
|  Concat { (   Concat  ) }
|  Connectlocation { (   Connectlocation  ) }
|  Constant { (   Constant  ) }
|  Constraint { (   Constraint  ) }
|  Contents { (   Contents  ) }
|  Cornertype { (   Cornertype  ) }
|  Criticality { (   Criticality  ) }
|  Currentmap { (   Currentmap  ) }
|  Curve { (   Curve  ) }
|  Cycle { (   Cycle  ) }
|  Dataorigin { (   Dataorigin  ) }
|  Dcfaninload { (   Dcfaninload  ) }
|  Dcfanoutload { (   Dcfanoutload  ) }
|  Dcmaxfanin { (   Dcmaxfanin  ) }
|  Dcmaxfanout { (   Dcmaxfanout  ) }
|  Delay { (   Delay  ) }
|  Delta { (   Delta  ) }
|  Derivation { (   Derivation  ) }
|  Design { (   Design  ) }
|  Designator { (   Designator  ) }
|  Difference { (   Difference  ) }
|  Direction { (   Direction  ) }
|  Display { (   Display  ) }
|  Divide { (   Divide  ) }
|  Dominates { (   Dominates  ) }
|  Dot { (   Dot  ) }
|  Duration { (   Duration  ) }
|  E { (   E  ) }
|  Edif { (   Edif  ) }
|  EdifLevel { (   EdifLevel  ) }
|  Edifversion { (   Edifversion  ) }
|  Else { (   Else  ) }
|  Enclosuredistance { (   Enclosuredistance  ) }
|  Endtype { (   Endtype  ) }
|  Entry { (   Entry  ) }
|  Equal { (   Equal  ) }
|  Escape { (   Escape  ) }
|  Event { (   Event  ) }
|  Exactly { (   Exactly  ) }
|  External { (   External  ) }
|  Fabricate { (   Fabricate  ) }
|  False { (   False  ) }
|  Figure { (   Figure  ) }
|  Figurearea { (   Figurearea  ) }
|  Figuregroup { (   Figuregroup  ) }
|  Figuregroupobject { (   Figuregroupobject  ) }
|  Figuregroupoverride { (   Figuregroupoverride  ) }
|  Figuregroupref { (   Figuregroupref  ) }
|  Figureperimeter { (   Figureperimeter  ) }
|  Figurewidth { (   Figurewidth  ) }
|  Fillpattern { (   Fillpattern  ) }
|  Fix { (   Fix  ) }
|  Floor { (   Floor  ) }
|  Follow { (   Follow  ) }
|  Forbiddenevent { (   Forbiddenevent  ) }
|  Globalportref { (   Globalportref  ) }
|  Greaterthan { (   Greaterthan  ) }
|  Gridmap { (   Gridmap  ) }
|  Ignore { (   Ignore  ) }
|  Includefiguregroup { (   Includefiguregroup  ) }
|  Increasing { (   Increasing  ) }
|  Initial { (   Initial  ) }
|  Instance { (   Instance  ) }
|  Instancebackannotate { (   Instancebackannotate  ) }
|  Instancegroup { (   Instancegroup  ) }
|  Instancemap { (   Instancemap  ) }
|  Instanceref { (   Instanceref  ) }
|  Integer { (   Integer  ) }
|  Integerdisplay { (   Integerdisplay  ) }
|  Interface { (   Interface  ) }
|  Interfiguregroupspacing { (   Interfiguregroupspacing  ) }
|  Intersection { (   Intersection  ) }
|  Intrafiguregroupspacing { (   Intrafiguregroupspacing  ) }
|  Inverse { (   Inverse  ) }
|  Isolated { (   Isolated  ) }
|  Iterate { (   Iterate  ) }
|  Joined { (   Joined  ) }
|  Justify { (   Justify  ) }
|  Keyworddisplay { (   Keyworddisplay  ) }
|  Keywordlevel { (   Keywordlevel  ) }
|  Keywordmap { (   Keywordmap  ) }
|  Lessthan { (   Lessthan  ) }
|  Library { (   Library  ) }
|  Libraryref { (   Libraryref  ) }
|  Listofnets { (   Listofnets  ) }
|  Listofports { (   Listofports  ) }
|  Loaddelay { (   Loaddelay  ) }
|  Logicassign { (   Logicassign  ) }
|  Logicinput { (   Logicinput  ) }
|  Logiclist { (   Logiclist  ) }
|  Logicmapinput { (   Logicmapinput  ) }
|  Logicmapoutput { (   Logicmapoutput  ) }
|  Logiconeof { (   Logiconeof  ) }
|  Logicoutput { (   Logicoutput  ) }
|  Logicport { (   Logicport  ) }
|  Logicref { (   Logicref  ) }
|  Logicvalue { (   Logicvalue  ) }
|  Logicwaveform { (   Logicwaveform  ) }
|  Maintain { (   Maintain  ) }
|  Match { (   Match  ) }
|  Max { (   Max  ) }
|  Member { (   Member  ) }
|  Min { (   Min  ) }
|  Minomax { (   Minomax  ) }
|  Minomaxdisplay { (   Minomaxdisplay  ) }
|  Mnm { (   Mnm  ) }
|  Mod { (   Mod  ) }
|  Multiplevalueset { (   Multiplevalueset  ) }
|  Mustjoin { (   Mustjoin  ) }
|  Name { (   Name  ) }
|  Negate { (   Negate  ) }
|  Net { (   Net  ) }
|  Netbackannotate { (   Netbackannotate  ) }
|  Netbundle { (   Netbundle  ) }
|  Netdelay { (   Netdelay  ) }
|  Netgroup { (   Netgroup  ) }
|  Netmap { (   Netmap  ) }
|  Netref { (   Netref  ) }
|  Nochange { (   Nochange  ) }
|  Nonpermutable { (   Nonpermutable  ) }
|  Notallowed { (   Notallowed  ) }
|  Notchspacing { (   Notchspacing  ) }
|  Number { (   Number  ) }
|  NumberDefinition { (   NumberDefinition  ) }
|  Numberdisplay { (   Numberdisplay  ) }
|  Offpageconnector { (   Offpageconnector  ) }
|  Offsetevent { (   Offsetevent  ) }
|  Openshape { (   Openshape  ) }
|  Or { (   Or  ) }
|  Orientation { (   Orientation  ) }
|  Origin { (   Origin  ) }
|  Overhangdistance { (   Overhangdistance  ) }
|  Overlapdistance { (   Overlapdistance  ) }
|  Oversize { (   Oversize  ) }
|  Owner { (   Owner  ) }
|  Page { (   Page  ) }
|  Pagesize { (   Pagesize  ) }
|  Parameter { (   Parameter  ) }
|  Parameterassign { (   Parameterassign  ) }
|  Parameterdisplay { (   Parameterdisplay  ) }
|  Path { (   Path  ) }
|  Pathdelay { (   Pathdelay  ) }
|  Pathwidth { (   Pathwidth  ) }
|  Permutable { (   Permutable  ) }
|  Physicaldesignrule { (   Physicaldesignrule  ) }
|  Plug { (   Plug  ) }
|  Point { (   Point  ) }
|  Pointdisplay { (   Pointdisplay  ) }
|  Pointlist { (   Pointlist  ) }
|  Pointsubtract { (   Pointsubtract  ) }
|  Pointsum { (   Pointsum  ) }
|  Polygon { (   Polygon  ) }
|  Port { (   Port  ) }
|  Portbackannotate { (   Portbackannotate  ) }
|  Portbundle { (   Portbundle  ) }
|  Portdelay { (   Portdelay  ) }
|  Portgroup { (   Portgroup  ) }
|  Portimplementation { (   Portimplementation  ) }
|  Portinstance { (   Portinstance  ) }
|  Portlist { (   Portlist  ) }
|  Portlistalias { (   Portlistalias  ) }
|  Portmap { (   Portmap  ) }
|  Portref { (   Portref  ) }
|  Posedge { (   Posedge ) }
|  Product { (   Product  ) }
|  Program { (   Program  ) }
|  Property { (   Property  ) }
|  Propertydisplay { (   Propertydisplay  ) }
|  Protectionframe { (   Protectionframe  ) }
|  Pt { (   Pt  ) }
|  Rangevector { (   Rangevector  ) }
|  Rectangle { (   Rectangle  ) }
|  Rectanglesize { (   Rectanglesize  ) }
|  Rename { (   Rename  ) }
|  Resolves { (   Resolves  ) }
|  Scale { (   Scale  ) }
|  Scalex { (   Scalex  ) }
|  Scaley { (   Scaley  ) }
|  Section { (   Section  ) }
|  Shape { (   Shape  ) }
|  Simulate { (   Simulate  ) }
|  Simulationinfo { (   Simulationinfo  ) }
|  Singlevalueset { (   Singlevalueset  ) }
|  Site { (   Site  ) }
|  Socket { (   Socket  ) }
|  Socketset { (   Socketset  ) }
|  Status { (   Status  ) }
|  Steady { (   Steady  ) }
|  Strictlyincreasing { (   Strictlyincreasing  ) }
|  String { (   String  ) }
|  Stringdisplay { (   Stringdisplay  ) }
|  Strong { (   Strong  ) }
|  Subtract { (   Subtract  ) }
|  Sum { (   Sum  ) }
|  Symbol { (   Symbol  ) }
|  Symmetry { (   Symmetry  ) }
|  Table { (   Table  ) }
|  Tabledefault { (   Tabledefault  ) }
|  Technology { (   Technology  ) }
|  Textheight { (   Textheight  ) }
|  Then { (   Then  ) }
|  Timeinterval { (   Timeinterval  ) }
|  TimeStamp { (   TimeStamp  ) }
|  Timing { (   Timing  ) }
|  Transform { (   Transform  ) }
|  Transition { (   Transition  ) }
|  True { (   True  ) }
|  Unconstrained { (   Unconstrained  ) }
|  Undefined { (   Undefined  ) }
|  Union { (   Union  ) }
|  Unit { (   Unit  ) }
|  Unused { (   Unused  ) }
|  Userdata { (   Userdata  ) }
|  Variable { (   Variable  ) }
|  View { (   View  ) }
|  Viewlist { (   Viewlist  ) }
|  Viewmap { (   Viewmap  ) }
|  Viewref { (   Viewref  ) }
|  Viewtype { (   Viewtype  ) }
|  Visible { (   Visible  ) }
|  Voltagemap { (   Voltagemap  ) }
|  Wavevalue { (   Wavevalue  ) }
|  Weak { (   Weak  ) }
|  Weakjoined { (   Weakjoined  ) }
|  When { (   When  ) }
|  While { (   While  ) }
|  Written { (   Written  ) }
|  Xcoord { (   Xcoord  ) }
|  Ycoord { (   Ycoord  ) }
;
