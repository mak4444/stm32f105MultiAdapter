
[IFNDEF]  REPLACE

: REPLACE ( by-xt what-xt -- )
\ by Day (Yakimov D.A.)
\ Example: ' NewSPACES ' SPACES REPLACE
  0xE9 OVER C!  \ JMP ...
  1+ DUP >R
  CELL+ -
  R> !
;
[THEN]

[IFNDEF] OR!	:  OR! ( N ADDR -- )	DUP @ ROT OR	SWAP ! ; [THEN]
[IFNDEF] AND!	: AND! ( N ADDR -- )	DUP @ ROT AND	SWAP ! ; [THEN]

C" H." FIND NIP 0=
[IF] : H.  BASE M@ HEX SWAP U. BASE !  ;
[THEN]

C" BOUNDS" FIND NIP 0=
[IF] : BOUNDS OVER + SWAP ;
[THEN]

[IFNDEF] #HEADER
: #HEADER ( cfa -- )
  HERE >R
  DP M! HEADER
 R> DP M! ;
[THEN]


: [THUMB2] ALSO THUMB2_MOD ; IMMEDIATE


REQUIRE TH_H-	_mak/lib/THERE/RECOM.F
\ REQUIRE <SIGN>	lib/include/float.f
\ REQUIRE MFFFFF	_mak/lib/THERE/tcfloat.f
\ REQUIRE NUMBER? _mak/lib/fpcnum.f 
REQUIRE 'NOOP	MetaCortex/SRC/forward.f
[IFNDEF] #define
: #define
  HEADER CONSTANT-CODE COMPILE, 0 PARSE EVALUATE ,
  LAST @ CURRENT @ ! ;
[THEN]

: CMOVE ( c-addr1 c-addr2 u --- )
\ Copy u bytes starting at c-addr1 to c-addr2, proceeding in ascending
\ order.

   DUP IF  >R
   BEGIN
    OVER 
\ ." C=" DUP H.
 C@
 SWAP DUP >R
 C!
 R> 1+ SWAP 1+ SWAP
    R>  1- DUP  >R 0=
   UNTIL
   R>
   THEN
   2DROP DROP
;

: M_CMOVE ( c-addr1 c-addr2 u --- )
\ Copy u bytes starting at c-addr1 to c-addr2, proceeding in ascending
\ order.

   DUP IF  >R
   BEGIN
    OVER 
 MC@
 SWAP DUP >R
 C!
 R> 1+ SWAP 1+ SWAP
    R>  1- DUP  >R 0=
   UNTIL
   R>
   THEN
   2DROP DROP
;


 REQUIRE TINLINE?	MetaCortex/SRC/macroopt.f
 REQUIRE DoTDTST	MetaCortex/SRC/OptTr.f 
 REQUIRE .OP0		MetaCortex/SRC/DOP.4


MODULE: TC


: ><DP DP M@ T-DP M@
       DP M! T-DP M! ;

: I, , ;

: T_COMPILE,
   TC_?LIMIT
 DUP HERE 4 + - ABS 0x7FFFFF INVERT AND
 IF
  DUP 0xFFFF AND
 [THUMB2]
  DUP 0x100 U<
	IF 0x2000 ( movs R0,) OR W,
	ELSE    R0 SWAP ## MOVW,
	THEN \  TOPT
  16 RSHIFT
  ?DUP IF  R0  SWAP ## MOVT, THEN
  R0 BLX,
  [P]
 ELSE
   '':: THUMB2_MOD BL,
 THEN
;

: THUMBEXP? ( n -- n flg )
  DUP 0= IF -1 BREAK
  DUP
  BEGIN DUP 0>
  WHILE  2*
  REPEAT
  0x00FFFFFF AND 0= ;

: TC-LIT, ( n -- )
\ compile code for a literal
\ DUP 0xFFFFFF00 AND
   TOPT_INIT
\ 0x5D04F844 ,  \ STR.W   R5, [R4, #-4]!
 0x6D04F847 ,  \ STR.W	R6, [R7, #-4]!
       TOPT
  DUP 0x100 U< IF 0x2600 ( movs R6,) OR
 W,
 TOPT;
 BREAK

  THUMBEXP? IF [THUMB2]  R6  SWAP ## MOV.W, [P] TOPT; BREAK
  INVERT 
  THUMBEXP? IF [THUMB2]  R6  SWAP ## MVN.W, [P] TOPT; BREAK
  INVERT
  
  DUP 0xFFFF AND
 [THUMB2]
  DUP 0x100 U< DROP 0 IF 0x2600 ( movs R6,) OR W,
	ELSE    R6 SWAP ## MOVW, THEN  TOPT
  16 RSHIFT
  ?DUP IF  R6  SWAP ## MOVT, THEN
 [P]   TOPT;
;

: ?CONST ( cfa -- cfa flag )
\- REL@  DUP @
\+ REL@  DUP 1+  REL@ CELL+
 CONSTANT-CODE  = ;


: TCOMPILE, ( tcfa -- )  
  DUP THERE? 0=
  IF  ?CONST IF EXECUTE TC-LIT, EXIT THEN
      -9 THROW
  THEN

 	TINLINE? IF  TINLINE, BREAK

	T_COMPILE, ;


: TCONSTANT
 ><DP CONSTANT   ><DP ;

0 VALUE ZZZZ

: COMPILE,  
\ ZZZZ IF  THEN
 DP @
 THERE?
 IF
 TCOMPILE,
 EXIT THEN
 COMPILE, ;

: CONSTANT HERE THERE? IF TCONSTANT BREAK CONSTANT ;

0 VALUE 'DO-CONST

: ICONSTANT
\ abort
 T-ALIGN
 HEADER
    'DO-CONST ,                                                    \ 
    ,                                                             \ 
 ;

: #define  HERE THERE? IF ><DP #define ><DP BREAK #define ;

: DLIT, ( D -> )
  SWAP LIT, LIT,
;


: TLITERAL
  STATE @
 IF
 cr ." TLITERAL=" dup h.
 LIT,
 THEN
; IMMEDIATE

[IFDEF] PLUG

: TLIT,
 DP @
 THERE?
 IF  TC-LIT, BREAK
 MLIT, ;

: PTLIT, RDROP TLIT, ;

' PTLIT,  PLUG LIT,

: PHERE  RDROP
 DP @ DUP TO :-SET ;

' PHERE  PLUG HERE

[ELSE]

: MLIT, ( W -> )
  ['] DUP  INLINE,
  OPT_INIT
  SetOP 0xB8 C,  , OPT  \ MOV EAX, #
  OPT_CLOSE
;

: TLIT,
 DP @
 THERE?
 IF  TC-LIT, BREAK
 MLIT, ;

' TLIT,  ' LIT, REPLACE

[THEN]

: MCREATE CREATE  ;

: TCREATE 
   D-DP M@ 3 + 3 INVERT AND DUP D-DP M!
  TCONSTANT ;

: LABEL        ( | name --- )  \ 
   HERE TCONSTANT ;

: IALLOT ALLOT ;

: ICREATE
   DP M@ 3 + 3 INVERT AND DUP DP M!
  TCONSTANT ;

: CCREATE
  2ALIGN 
  HERE 3 AND 0= if 0xFFFF W, then

  1 IALLOT  HEADER -1 IALLOT  LAST @ CURRENT @ !
  0xb500  W,  \           push    {lr}
  ] S" DOCREATE" EVALUATE POSTPONE [
;

: CREATE
\ CCREATE
 TCREATE
 ;

: ALLOT  D-DP +! ;
: IHERE  HERE ;
: DHERE  D-DP @ ;

: DALIGN  D-DP @ 3 + 3 ANDC D-DP ! ;
: D2ALIGN  D-DP @ 7 + 7 ANDC D-DP ! ;

: MVARIABLE VARIABLE ;
: buffer:  CREATE  ALLOT ;
: VARIABLE    DALIGN CREATE 1 CELLS  ALLOT ;
: 2VARIABLE  D2ALIGN CREATE 2 CELLS  ALLOT ;

: bssvar  VARIABLE postpone \ ;
: bssvarz  VARIABLE postpone \  16 cells allot ;

: ?OLD
  S"  DP @ THERE? 0= IF" EVALUATE
  POSTPONE POSTPONE
  S" EXIT THEN" EVALUATE ; IMMEDIATE

0 VALUE 'EXIT

: EXIT,
	0xbd00  W,  \      pop     {pc}	
;

: EXIT
	?OLD EXIT	EXIT,
; IMMEDIATE
0 VALUE ;OPT?
: ; 
	?OLD ;
	?TSET
	:-SET LAST-CFA M@ M@ 1+ =
	:-SET 2- W@ $B500 = AND ;OPT? AND 
	IF	:-SET :-SET 2-	DP M@ :-SET - CMOVE
		-2 IALLOT NEXT
	ELSE
	EXIT,
	THEN
\+ SMUDGE SMUDGE
\- SMUDGE  LAST @ CURRENT @ !
	[COMPILE] [
; IMMEDIATE

: BRANCH, ( A -- ) \ ZZZZ IF  THEN
\+ BRANCH,	?OLD    BRANCH,
	'':: THUMB2_MOD B.N,
 ;

: ?BRANCH, ( A -- )
\+ ?BRANCH,	?OLD ?BRANCH,	
[ ALSO	THUMB2_MOD ]		TOPT_INIT
	r6 r6 TST,		TOPT
    	r7 !! {{ r6 }}  LDMIA,	TBOPT \ r6 [[ r7 ]] ## 4  ldr.w,
  	BEQ,                  TOPT_CLOSE
[P]
;

: ?BRANCH.F, ( A -- )
\+ ?BRANCH,	?OLD ?BRANCH,
 :-SET
 DROP
\	TOPT_INIT
[ ALSO	THUMB2_MOD ]		TOPT_INIT  [ here drop ]
	r6 r6 TST,		TOPT
    	r7 !! {{ r6 }}  LDMIA,	TBOPT \ r6 [[ r7 ]] ## 4  ldr.w,
  	BEQ.W,                  TOPT_CLOSE
[P]
  ;

1	CONSTANT IF_FLAG	\ 
11	CONSTANT IF.F_FLAG	\ 
13	CONSTANT HEAD_FLAG	\ 
3	CONSTANT BEGIN_FLAG	\ 
7	CONSTANT DO_FLAG1	\ 

: T>ORESOLVE ( A, N -- )
	
  DUP IF_FLAG	= IF DROP 2-
	HERE OVER - 4 - 2/

 DUP  $80 + $FF ANDC IF -314 THROW THEN

 SWAP C! BREAK


 DUP  IF.F_FLAG	= IF DROP 4 - HERE >R DP ! 
[ ALSO	THUMB2_MOD ]		TOPT_INIT
  	R@ BEQ.W,                  TOPT_CLOSE
[P] R> DP !
BREAK

  HEAD_FLAG	= IF	2-	HERE OVER - 4 - 2/ 0x7FF AND 0xE000 OR SWAP  W! BREAK

	-2007 THROW  \ ABORT" Conditionals not paired"
;

\ 
: FORWARD_RESOLVE  ( adr_after_?BRANCH_BRANCH target_adr -- )
    SWAP !
;

: AHEAD ?OLD AHEAD	?COMP DP M@  BRANCH,	HERE HEAD_FLAG	; IMMEDIATE
: IF.F	?OLD IF		?COMP
 DP M@
\ ." <?BRANCH.F," CR
 ?BRANCH.F,
\ ." ?BRANCH.F,>" CR
	HERE IF.F_FLAG	; IMMEDIATE
: IF.N	?OLD IF		?COMP DP M@ ?BRANCH,	HERE IF_FLAG		; IMMEDIATE
: THEN	?OLD THEN	?COMP   T>ORESOLVE ; IMMEDIATE
: BREAK ?OLD BREAK	POSTPONE EXIT POSTPONE THEN ; IMMEDIATE

VECT IF  IMMEDIATE
' IF.F TO IF

: ?OF POSTPONE IF.N  'DROP COMPILE, ; IMMEDIATE

: TCS-SWAP 2SWAP ;

: ELSE   ( BO BI ADDR ? -- 0 0 ADDR1 ?1 )
  HERE THERE? 0= IF POSTPONE ELSE BREAK
  POSTPONE  AHEAD TCS-SWAP
  POSTPONE  THEN
;  IMMEDIATE

S" MetaCortex/SRC/case.4" INCLUDED


: BEGIN
  HERE THERE? 0= IF POSTPONE BEGIN EXIT THEN  ?COMP HERE 3 ; IMMEDIATE

: UNTIL \ 94
  DP @ THERE? 0= IF POSTPONE UNTIL EXIT THEN
  3 <> IF -2004 THROW THEN \ ABORT" UNTIL 
  ?COMP
  ?BRANCH,

; IMMEDIATE

: WHILE \ 94
  DP @ THERE? 0= IF POSTPONE WHILE EXIT THEN
  ?COMP [COMPILE] IF
  TCS-SWAP
; IMMEDIATE

: WHILE.F \ 94
  DP @ THERE? 0= IF POSTPONE WHILE EXIT THEN
  ?COMP [COMPILE] IF.F
  TCS-SWAP
; IMMEDIATE

: AGAIN  HERE THERE? 0= IF  POSTPONE AGAIN EXIT THEN
  ?COMP 3 <> IF -2006 THROW THEN \ ABORT" AGAIN 
  BRANCH,
; IMMEDIATE

: REPEAT HERE THERE? 0= IF  POSTPONE REPEAT EXIT THEN
  POSTPONE AGAIN
  POSTPONE THEN 
; IMMEDIATE

: RECURSE 
  ?COMP
\	
 LATEST NAME>

 HERE  THERE?
 IF
  T_COMPILE,
 BREAK
   COMPILE,
; IMMEDIATE

: [']  \ 94
  ?COMP
	?OLD [']
  ' 
\ LIT, \ '(<'>)  T_COMPILE,  HERE - 4 - ,
\  0x5D04F844 ,  \ STR.W   R5, [R4, #-4]!
  0x6D04F847 ,  \ STR.W	R6, [R7, #-4]!
  0x7ffff6cf I, \ movt	pc, 0xffff - bad code
 I, 
; IMMEDIATE

: E#
  ' 
\ 0x5D04F844 ,  \ STR.W   R5, [R4, #-4]!
  0x6D04F847 ,  \ STR.W	R6, [R7, #-4]!
  0x7ffff6cf I, \ movt	pc, 0xffff - bad code
  1- I, 
; IMMEDIATE

: [CHAR]
  ?COMP
  PARSE-NAME  DROP MC@  LIT,
; IMMEDIATE


: S", ( addr u -- ) \ 
  DUP  C, 
 DP M@ SWAP DUP IALLOT
\  TT_? IF  THEN
   M_CMOVE ;

: H, W, ;

: ALIGNED  1+ -2 AND ;

: SLITERAL
  HERE THERE? STATE
 M@ AND
  IF '(S") COMPILE, S", 
	 HERE 1 AND
	IF 0xFF C,
	THEN
  ELSE  POSTPONE SLITERAL
  THEN   ; IMMEDIATE
1 [IF]
: S"  ( "ccc<quote>" --- )
\ Parse a string delimited by " and compile the following runtime semantics.
\ Runtime: ( --- c-addr u) Return start address and length of that string.
  [CHAR] " PARSE
  POSTPONE SLITERAL ; IMMEDIATE
[THEN]

: FS"  ( "ccc<quote>" --- )
\ Parse a string delimited by " and compile the following runtime semantics.
\ Runtime: ( --- c-addr u) Return start address and length of that string.
  [CHAR] " PARSE 
  POSTPONE SLITERAL
 ; IMMEDIATE

0 VALUE '(.")
: ."
	?COMP
	HERE THERE? 0= IF  POSTPONE ." EXIT THEN
	POSTPONE S"
	S" TYPE" EVALUATE
; IMMEDIATE

: NEXT
  0x4770 W, \  THUMB2_MOD::LR  THUMB2_MOD::BX,
 ;

MVARIABLE 'LEAVE ( --- a-addr)
\ This variable is used for LEAVE address resolution.

\ \

: LEAVE ( --- )
\ Runtime: leave the matching DO LOOP immediately.
\ All places where a leave address for the loop is needed are in a linked
\ list, starting with 'LEAVE variable, the other links in the cells where
\ the leave addresses will come.
   ?OLD LEAVE
  S" (LEAVE)" EVALUATE
   'LEAVE @
   IF   HERE DUP 'LEAVE @ -
   ELSE HERE 0
   THEN W, 'LEAVE !
; IMMEDIATE

: RESOLVE-LEAVE
\ Resolve the references to the leave addresses of the loop.
\ EXIT
          'LEAVE @
        BEGIN DUP
	WHILE
    DUP DUP
    W@ DUP
    IF -  OVER
    ELSE  SWAP
    THEN  HERE - NEGATE  ROT  W!

	REPEAT DROP ;

: DO 
  ?OLD DO
  'LEAVE @
  S" (DO)" EVALUATE

  HERE   DO_FLAG1
 ; IMMEDIATE


: ?DO
  ?OLD DO
  'LEAVE @
  S" (?DO)" EVALUATE

\   'LEAVE @
\   IF   HERE DUP 'LEAVE @ -
\   ELSE HERE 0
\   THEN
 HERE 0
  W, 'LEAVE !

   HERE DO_FLAG1

 ; IMMEDIATE

: LOOP  ( x flg --- )
\ End a DO LOOP.
\ Runtime: Add 1 to the count and if it is equal to the limit leave the loop.
  ?OLD LOOP
 DO_FLAG1 <> IF -2007 THROW THEN \ Conditionals not paired
\	    HERE 3 AND IF 0xBF00 W, THEN
[ ALSO	THUMB2_MOD ]		TOPT_INIT
	{{ R0 }} pop,	TOPT
	R0 ## 1 adds,	TOPT
	{{ R0 } push,	TOPT
	BVC,		TOPT
	SP ## 8 ADD,	TOPT_CLOSE
[P]

	  RESOLVE-LEAVE 
	 'LEAVE !

 ; IMMEDIATE

0 VALUE '+LOOP_ASM

: +LOOP ( x flg --- )
\ End a DO LOOP.
\ Runtime: Add 1 to the count and if it is equal to the limit leave the loop.
  ?OLD LOOP
 DO_FLAG1 <> IF -2007 THROW THEN \ Conditionals not paired
\	    HERE 3 AND IF 0xBF00 W, THEN
[ ALSO	THUMB2_MOD ]		TOPT_INIT
	{{ R0 }} pop,	TOPT
	R0 r6 adds,	TOPT
	{{ R0 } push,	TOPT
	r7 !!  {{ r6 }} LDMIA,  TOPT \ DROP
	BVC,		TOPT
	SP ## 8 ADD,	TOPT_CLOSE
[P]
	  RESOLVE-LEAVE 
	 'LEAVE !

 ; IMMEDIATE

: ]L  LIT, ] ;

: :
    HERE THERE? 0= IF  :  BREAK
\ [DEFINED] DBG_STOP [IF]   DBG_STOP [THEN]
 t-align
  1 IALLOT  : -1 IALLOT
  0xb500  W,  \           push    {lr}
;

: DECIMAL
 STATE M@ 0= IF DECIMAL BREAK
 S" $A BASE !" EVALUATE    ; IMMEDIATE

: HEX
 STATE M@ 0= IF HEX BREAK
 S" $10 BASE !" EVALUATE    ; IMMEDIATE


: NEXT
  '':: THUMB2_MOD A;
  0x4770 W, \  THUMB2_MOD::LR  THUMB2_MOD::BX,
 ;


: $*
    BASE M@ >R HEX
   PARSE-NAME DUP 8 =

 IF  NUMBER? 0= THROW  THROW	I,

 ELSE
	  NUMBER?
	0= THROW  THROW
	  W,
	 PeekChar BL <>
	 IF
	   PARSE-NAME
	  NUMBER?
	0= THROW  THROW
	  W,
 THEN

 THEN
   R> BASE M!
	POSTPONE \
;

: $_
\ 
\ CR ." #"
 PARSE-NAME 2DROP
 $*
;


: __
   PARSE-NAME 2DROP  
\ PeekChar DUP BL <> SWAP 9 <> AND
 PeekChar BL <>
 IF PARSE-NAME 2DROP
 THEN
;

: :_
   PARSE-NAME 2DROP __
;

0 VALUE T$$

: $$,  ( ADDR -- ADDR+2 )
 COUNT  0x10 DIGIT 0= THROW >R
 COUNT  0x10 DIGIT 0= THROW R> 0x10 * OR C, ;


: $$

 PARSE-NAME 2DROP
   BASE @ >R HEX  
   PARSE-NAME
  NUMBER?
0= THROW  THROW
 T$$ IF W, ELSE , THEN
   R> BASE !
	POSTPONE \
;

&INTERPRET @ VALUE INTERPRET_S

: SSS:
  INTERPRET_S  &INTERPRET !
 ALSO THUMB2_MOD
 BEGIN REFILL
\ CR ." <" SOURCE TYPE ." >"
 WHILE [CHAR] : PARSE 2DROP
 >IN M@  5 + SOURCE NIP U<
 PeekChar 9 = AND
 IF $* ELSE POSTPONE \ THEN 
 REPEAT
 PREVIOUS
\ 
;

: TOSSS:
 &INTERPRET @ TO INTERPRET_S
  ['] SSS:      &INTERPRET !
;

0 VALUE END$$

EXPORT

1 [IF]

: T-STOP
  PREVIOUS DEFINITIONS
  HERE THERE?    IF ><DP THEN
;


: T-GO
[DEFINED] DBG_STOP [IF]  DBG_STOP [THEN]
  ALSO TC DEFINITIONS
  HERE THERE? 0= IF ><DP THEN
;

: THERE?A
   0x40000000 U< 0= ;

: THERE?E  0x08000000  U> ;


: TTH  ['] THERE?E TO THERE? ;

: WRD_0
 CONTEXT @ @ 
 BEGIN	DUP
 WHILE
	DUP NAME> HSSSS M@ + 44 - ZSSSS M@  - 44 U< IF DROP BREAK
	DUP CR TAB ." Wortbirne6 " 0 $80 AND 7 >> H.
	DUP '"' EMIT  COUNT $7F  AND  TYPE   '"' EMIT
	DUP NAME> ?CONST 
	IF	
		CR TAB ." push {lr};BL DOCONST"
		CR TAB ." .word 0x" EXECUTE H.
	ELSE  DROP
	  DUP CR TAB ." B " COUNT $7F  AND  GTYPE
\	  DUP ."  @ " NAME> H. \ KEY DROP
	THEN

        CDR
 REPEAT DROP
;

: WRD_
 CONTEXT @ @ 
 BEGIN	DUP
 WHILE
	DUP NAME> ?CONST NIP 0=
	IF	
		DUP NAME> HSSSS M@ + 44 - ZSSSS M@  - 44 U< IF DROP BREAK
		DUP CR TAB ." Wortbirne6 " 0 $80 AND 7 >> H.
		DUP '"' EMIT  COUNT $7F  AND  TYPE   '"' EMIT
		DUP CR TAB ." B " COUNT $7F  AND  GTYPE
\		DUP ."  @ " NAME> H. \ KEY DROP
	THEN

        CDR
 REPEAT DROP
;

: SWRD (  ADDR LEN -- )
  R/W CREATE-FILE THROW
  STDOUT @ >R
  STDOUT !
  T-ORG HERE OVER -
  ['] WRD_0 CATCH
  STDOUT @ CLOSE-FILE R> STDOUT ! THROW
  THROW
;

: VAL>ASM 
  PARSE-NAME  TYPE ." _OF = 0x" H. CR 
;

: VAR>ASM 
   0x20008000 -
 VAL>ASM
;


/*
: TC_VAR_GEN (  ADDR LEN -- )
  R/W CREATE-FILE THROW
  STDOUT @ >R
  STDOUT !
  INCLUDED CATCH
  STDOUT @ CLOSE-FILE R> STDOUT ! THROW
  THROW
;
*/

: T-START  ( n -- )  \ n 
\+ YDP_FL 1 TO YDP_FL
  1 ALIGN-BYTES M!
  T-INIT
 ['] THERE?E TO THERE?
  T-GO
;


: TSAVE (  ADDR LEN -- )
  R/W CREATE-FILE THROW >R
  T-ORG HERE OVER - R@ TWRITE-FILE THROW
  R> CLOSE-FILE THROW 
;

: ATIPE ( ADDR LEN -- )
   BOUNDS
   DO  CR ." .byte  0x"  I C@  H. ." ,0x"  I 1+ C@  H. 2
  +LOOP CR
;


: ASAVE (  ADDR LEN -- )
  R/W CREATE-FILE THROW
  STDOUT @ >R
  STDOUT !
  T-ORG HERE OVER -
  ['] ATIPE CATCH
  STDOUT @ CLOSE-FILE R> STDOUT ! THROW
  THROW
;

: CTIPE ( ADDR LEN -- )
	." const char FORTH_IMG[]={"
	>R 
	." 0x"  DUP C@  H. 1+
	." ,0x" DUP C@  H. 1+
	." ,0x" DUP C@  H. 1+
	." ,0x" DUP C@  H. 1+
	R> CELL- 0 MAX
   BOUNDS
   ?DO    CR
	." ,0x"  I C@  H.
	." ,0x"  I 1+ C@  H.
	." ,0x"  I 2+ C@  H.
	." ,0x"  I 3 + C@  H.
 4  +LOOP ." };" CR 
;

: CSAVE (  ADDR LEN -- )
  R/W CREATE-FILE THROW
  STDOUT @ >R
  STDOUT !
  T-ORG HERE OVER -
  ['] CTIPE CATCH
  STDOUT @ CLOSE-FILE R> STDOUT ! THROW
  THROW
;

 MVARIABLE H-SUM
1 [IF]

: HEXBYTE ( BYTE FILE -- )
  >R DUP H-SUM @ + H-SUM !
  S>D <# # # #> R> WRITE-FILE THROW
;

: T-HEXLINE ( ADDR FILE -- )
  H-SUM 0!
  >R S" :" R@ WRITE-FILE THROW
  0x10 R@ HEXBYTE DUP 0x100 / R@ HEXBYTE
  DUP 0xFF AND R@ HEXBYTE 0x00 R@ HEXBYTE
  R>
  0x10 0 DO
    >R DUP C@ R@ HEXBYTE
    1+ R>
  LOOP
  >R DROP H-SUM @ NEGATE R@ HEXBYTE
  0 0 R> WRITE-LINE THROW
;

: T-SAVE ( FILENAME )
  BASE @ >R HEX
  R/W CREATE-FILE THROW
  HERE T-ORG DO
    I OVER T-HEXLINE
  0x10 +LOOP
  >R S" :00000001FF" R@ WRITE-LINE THROW
  R> CLOSE-FILE THROW

  R> BASE !
;
[THEN]
:  ?COMP_ STATE M@ =
         IF  COMPILE,
         ELSE EXECUTE
         THEN ;

-1 VALUE N.FLOAD?

: FILE, ( addr len -- )
   R/W OPEN-FILE THROW >R
   R@  FILE-SIZE THROW D>S
   HERE OVER R@ READ-FILE THROW
   IALLOT
    R> CLOSE-FILE THROW
;

0 VALUE ?SOURCETYPE

: TC-INTERPRET ( -> )
?SOURCETYPE IF  CR SOURCE TYPE THEN
  STATE M@ 0= IF  MAIN_S THEN
  BEGIN
    PARSE-NAME DUP
  WHILE
    SFIND ?DUP
    IF [']  ?COMP_  CATCH 
        THROW
    ELSE 
[IFDEF] ?_.
	?_.  N.FLOAD? AND
        IF  F-SIZE @ >R FSINGLE ['] MFFFFF CATCH R> F-SIZE ! THROW
		STATE M@ IF LIT, THEN
        ELSE ?SLITERAL
        THEN
[ELSE] ?SLITERAL
[THEN]
\         S" NOTFOUND" SFIND 
	\         IF EXECUTE
\         ELSE 2DROP
\         ?SLITERAL
\         THEN
    THEN
    ?STACK
  REPEAT 2DROP 
  STATE M@ 0= IF  MAIN_S THEN
;

 ' TC-INTERPRET  &INTERPRET !

[THEN]

;MODULE

