\ REQUIRE <SIGN>	lib/include/float0.f
\ REQUIRE FIRSTFILE ~mak\lib\VDOS\FindFile.4
fload MetaCortex/mhead0.f

\ fload _mak/lib/THERE/bsthere.f

REQUIRE TC_?LIMIT _mak/lib/THERE/STAT.F

fload MetaCortex/srcxx.f 

0 VALUE  TH_H-


fload MetaCortex/mhead.f

 CREATE RMARGIN $30 ,

VECT INCLUDED_X
' INCLUDED TO INCLUDED_X

: mgetxy  getxy ;

: ?CR
        RMARGIN M@ mgetxy DROP ( 2DUP . . ) U<
        IF      CR
        THEN    ;  

MEM_MODE
\ : KEY_ABORT_OFF ['] DROP TO  KEY_ABORT ;

\- T_C@? : T_C@? ABORT ;
\- T_C@> : T_C@> ABORT ;

[IFDEF] T-C@

: P_F_WAIT

	BEGIN  KEY?
		IF KEY 1BH = IF ABORT THEN
		THEN  T_C@?
	UNTIL #WAIT WAIT_COUNT !
;

' P_F_WAIT  EXE_TAB #F_WAIT CELLS + !

: P_MAX
   CR ." P_MAX"
;

' P_F_WAIT  EXE_TAB #MAX CELLS + !

[IFDEF] TC_GR
ALSO TC_GR
\ [ IFDEF ] TO-G
0 [IF]
: P_TO-G  T-@ TO-G ;

' P_TO-G  EXE_TAB #TO-G CELLS + !

: P_TO-R  T-@ TO-R ;

' P_TO-R  EXE_TAB #TO-R CELLS + !

: P_TO-B  T-@ TO-B ;

' P_TO-B  EXE_TAB #TO-B CELLS + !
[THEN]
[THEN]

  PREVIOUS
[ELSE] 
: MAIN_S ;
[THEN]

[IFDEF] T-@
: P_.  T-@ . ;

' P_.  EXE_TAB #. CELLS + !

: P_U.  T-@ U. ;

' P_U.  EXE_TAB #U. CELLS + !

: P_F.  T-@ F. ;

' P_F.  EXE_TAB #F. CELLS + !

: P_.R   T-@ T-@ SWAP .R ;

' P_.R  EXE_TAB #.R CELLS + !

\ : P_EXEC  ." EXEC" CR ; ' P_EXEC  EXE_TAB #EXEC CELLS + !

: P_SETBASE T-C@ BASE M! ;

' P_SETBASE EXE_TAB #SETBASE CELLS + !

WIN\ : P_?CR ?CR ; ' P_?CR EXE_TAB #?CR CELLS + !

[THEN]

: MMSS BEGIN  MAIN_S  KEY? UNTIL KEY DROP ;

: PF>_  ( -- c )
	BEGIN T_C@?
	UNTIL T_C@> ;

: PF>  PF>_ \ ." I=" DUP H. FJB
;

\- UEMIT : UEMIT ABORT ;

: >PF \ 100 PAUSE
\ ." O=" DUP H. FJB
  UEMIT ;

: N>FF         ( N -- )        
    0 ?DO 0xFF C, LOOP 
;


\ REQUIRE CS-! _mak/ext.f

[IFNDEF] W>S
 : W>S ( w -- n )  \ 
  0xFFFF AND    \ 
 0x8000 XOR 0x8000 - ;
[THEN]

[IFNDEF] H.R
: H.R           ( n1 n2 -- )    \ display n1 as a hex number right
                                \ justified in a field of n2 characters
                BASE @ >R HEX >R
                0 <# #S #> R> OVER - 0 MAX SPACES TYPE
                R> BASE ! ;
[THEN]

: HH.+  DUP C@ 2 H.R ."  " 1+ ;
: INST.+
 HH.+ ;

VECT MINST
: ?.NAME>S      ( CFA -- )
\ ELIMINATE " 0x"
	DUP H.  \ 1 H.R>S SSPACE
	DUP 0xFFFF0000 AND 0x10000000 <> IF DROP BREAK
	NEAR_NFA 
	>R DUP
	IF ."  ( " DUP COUNT TYPE 
	     NAME> R> - DUP
	     IF   DUP ." +" NEGATE H. \ >S
	     THEN DROP        ."  ) "
	ELSE RDROP DROP
	THEN ;

REQUIRE M_WL _mak/ext.f

REQUIRE 'NOOP	MetaCortex/SRC/forward.f
REQUIRE DISARM		MetaCortex/SRC/dist2.f 
' AAMINST TO MINST

REQUIRE INCLUDED_AL	_mak/lib/THERE/mlist.f 

REQUIRE THUMB2_MOD MetaCortex/SRC/thumb2.f

FLOAD MetaCortex/SRC/LEX.F
FLOAD MetaCortex/SRC/prefix.f

\ EOF
\ REQUIRE DISARM		MetaCortex/SRC/DISARM.f
: M\ ( POSTPONE \) ; IMMEDIATE

128         CONSTANT    T-ITABLE-SIZE               \ 
[IFDEF] FILE-SIZE
:  T_FILE,
CR  ." T_FI=<" 
   R/W OPEN-FILE THROW
    DUP H.
      >R
   R@  FILE-SIZE THROW D>S  
\ DROP $10000
  DUP H.
\   DUP ALLOCATE DROP DUP TO OF_ADDR TO OF_BUFF
   HERE DUP H.
    T>  DUP H.
 SWAP R@ READ-FILE
         DUP H.
 THROW 
         DUP H. ." >"
   ALLOT
   R> CLOSE-FILE THROW
;
[THEN]

REQUIRE T-START	MetaCortex/SRC/tc.f 

REQUIRE #THEADER MetaCortex/PROTOOS/tc.f

\ : MEHO HERE S" MEHO_" EVALUATE DP ! ;
: OSSS HERE S" MEHP_" EVALUATE DP ! ;

: |  2SWAP  SWAP C, C,  SWAP C, C,  ;



0 VALUE ROM-HERE

\+ CASE-INS CASE-INS ON

\ T-ROM-SIZE
0x30000 TO CODE-SIZE

[IFDEF] SPF-INI
 HERE  3 + -4 AND  $1000 + CONSTANT FW_TADR1
[ELSE]
CODE-SIZE 2* ALLOCATE THROW CONSTANT FW_TADR1
\ CODE-SIZE ALLOCATE THROW CONSTANT FW_TADR2
[THEN]

0x08000000
 TO T-ORG                \ 

T-ORG VALUE MT-ORG
0x20000800  TO D-ORG                \ 

T-ORG TO INCL-HH

\+ <PRE>  ' NOOP TO <PRE>

CR .( T-ORG=) T-ORG H.
CR .( here0=) here H.

\ T-START
\ T-init
\- SPF-INI  ' NOOP TO YALIGN  

\+ SPF-INI  1 ALIGN-BYTES M!


 FW_TADR1 CODE-SIZE 2- + TO MAX_HERE
 FW_TADR1 T-DP M!

 D-ORG D-DP M!
 T-GO

CR .( here=) here H.
