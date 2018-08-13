\ 


REQUIRE DUPENDCASE _mak/case.f

[IFNDEF] C>S
: C>S ( c -- n )  0xFF AND [ 0x7F INVERT ] LITERAL XOR 0x80 + ;
[THEN]

[IFNDEF] -CELL  -1 CELLS CONSTANT -CELL
[THEN]

[IFNDEF] ANDC : ANDC	INVERT AND ;
[THEN]

[IFNDEF] XOR! : XOR!
	DUP @
 ROT
 XOR
 SWAP
 ! ;
[THEN]



BASE @ HEX

   TRUE VALUE TOPT?


: TSET-OPT TRUE TO TOPT? ;

: TDIS-OPT FALSE TO TOPT? ;

0x20 VALUE TMM_SIZE

0 VALUE TOFF-PSP

0 VALUE TTTTTT

\- :-SET 0 VALUE :-SET

0 VALUE TLAST-HERE

: TOP0 DP M@   2 - ;
: TOP1 DP M@ 2 2* - ;
: TOP2 DP M@ 3 2* - ;
: TOP3 DP M@ 4 2* - ;
: TOP4 DP M@ 5 2* - ;
: TOP5 DP M@ 6 2* - ;
: TOP6 DP M@ 7 2* - ;
: TOP7 DP M@ 8 2* - ;
: TOP8 DP M@ 9 2* - ;

0 VALUE TOP0W@
0 VALUE TOP1W@

: ?HOP 0xE800 U< ;
: ?@HOP W@ ?HOP ;

: ?TSET
 :-SET
 DROP
 DP @
 DUP TLAST-HERE
 <>
 IF DUP TO :-SET  THEN DROP ;

: ?^OP ( addr -- flg )
\
  DUP :-SET U< IF DROP FALSE BREAK
  DUP :-SET =  IF DROP TRUE BREAK
  2-  \ |..|
  DUP ?@HOP IF DROP TRUE BREAK
 DUP
 :-SET =
 IF DROP FALSE BREAK
  2-  \ |..+..|
 DUP ?@HOP IF DROP FALSE BREAK
 DUP :-SET =  IF DROP TRUE BREAK
  2-  \ |..|..+..|
  ?@HOP \  DUP ?1OP IF DROP TRUE BREAK
;


: SHORT? ( n -- -129 < n < 128 )
  0x80 + 0x100 U< ;

M\ VECT TDTST  ' DROP TO TDTST

 TRUE VALUE ?C-JMP
\ FALSE VALUE ?C-JMP

: TOPexcise ( addr -- )
 DUP 2+
 TUCK DP @  - NEGATE
\ CR ." TOE=" 3DUP H. H. H.
 CMOVE
 -2 ALLOT ;

:  1OPexcise    TOP0W@ TOP1 W! -2 ALLOT  ;
:  2OPexcise    TOP1W@ TOP2 W! 1OPexcise ;
:  3OPexcise    TOP2 W@ TOP3 W! 2OPexcise ;
:  4OPexcise    TOP3 W@ TOP4 W! 3OPexcise ;

0 VALUE TOPN
0 VALUE TOPN@
2 VALUE XXXSIZE
0 VALUE XXXLOOPAND
VECT XXX:DO

: TOS:=? ( u -- flg ) \
    CASE
   4 TO XXXSIZE
\ DUP ." T=" H.
  DUP	8F00FBF0 AND
	0600f240 <> IF \	movw	r6, #X


  DUP	8F00FBCF AND
	0600F04F <> IF \	mvn|mov.w	r6, #X

   FFFF AND

   2 TO XXXSIZE

  DUP	9e00 <> IF \	LDR	R6, [SP, #0]
  DUP	687e <> IF \	LDR	R6, [R7, #4]
  DUP	683e <> IF \	LDR	R6, [R7, #0]
  DUP	FF00 AND
	2600 <> IF \	MOVS	R6, #X

  DROP    FALSE
   4 TO XXXSIZE
 EXIT
  DUPENDCASE DROP TRUE ;


: R0!=? ( u -- flg )
    CASE
   4 TO XXXSIZE
  DUP  TOS:=? 0=	IF

  DUP	8F00FBF0 AND
	0600f2C0 <> IF  \ movt	r6, #X

  DUP	0042E89D <> IF  \ LDMIA.W	R13, {R1, R6}

  FFFF AND
   2 TO XXXSIZE
  DUP	6836 <>		IF \	LDR	R6, [R6, #0]
  DUP	1876 <>		IF \	ADDS	R6, R6, R1

  DROP    FALSE
   4 TO XXXSIZE
 EXIT
  DUPENDCASE DROP TRUE ;

:  XXXLOOP?	( flg cfa adr  -- flg' )
  TO  XXXLOOPAND
  TO  XXX:DO
  DUP IF BREAK
  DROP TOPN DUP ?@HOP 0= IF 2+ THEN 2+
  BEGIN
	DUP	XXXLOOPAND = IF DROP FALSE BREAK
\ CR DUP H.
	DUP  @
\        DUP H.
  XXX:DO
\ DUP H. KEY DROP
	WHILE XXXSIZE +  REPEAT
 DROP TRUE
;

:  ST>R0?	( flg -- flg' )
  ['] R0!=? TOP0  XXXLOOP?
;


: WOPLOOK  ( n flg -- flg' )
\ CR ."  WOPLOOK=" TOP0 H.
  >R DUP IF RDROP NIP BREAK
  DROP
 TO TOPN@ R>
  BEGIN 2-
	DUP @
	TOPN@
 = IF \ ." <F>"
  DUP TO TOPN ?^OP 0= BREAK
	DUP :-SET =
  UNTIL DROP TRUE \  ." <T>"
;


: TOPT-RULES  ( ADDR  -- ADDR' FLAG )
  TOP1 :-SET U< IF TRUE EXIT THEN
  TOP0 W@ TO TOP0W@
  TOP1 W@ TO TOP1W@
M\ -1 TDTST
   TOP1 ?^OP
    IF

	TOP1W@ TOS:=?
	TOP0W@ TOS:=? AND
IF  M\ 4 TDTST
	1OPexcise
    M\ 5 TDTST
	FALSE
BREAK

	TOP1W@ 1b86	XOR		\	SUBS	R6, R0, R6
	TOP0W@ 4276	XOR OR		\	NEGS	R6, R6
0=
IF  M\ A TDTST
	1a36	TOP1  W! \	SUBS	R6, R6, R0
	-2 ALLOT
    M\ B TDTST
	FALSE
BREAK

	TOP1W@ 4006 XOR		\	ANDS	R6, R0
	TOP1W@ 41b6 <> AND	\	SBCS	R6, R6
	TOP1W@ 17f6 <> AND	\	ASRS	R6, R6, #31
	TOP1W@ 0ff6 <> AND	\	LSRS	R6, R6, #31
	TOP0W@ 4236 XOR OR	\	TST	R6, R6

0=
IF  M\ C TDTST
	-2 ALLOT
    M\ D TDTST
	FALSE
BREAK


TOP1 1+ C@ 26 =	\	MOVS	R6, #X
IF  

TOP0W@ 4006 =	\	ANDS	R6, R0
IF  M\ 10 TDTST
	TOP1  C@
	-4 ALLOT
	[THUMB2]	R6 R0 ROT ## ANDS,	[P]
    M\ 11 TDTST
	FALSE
BREAK

		\	MOVS	R6, #X
TOP0 W@ 1986 =	\  	ADDS	R6, R0, R6
IF  M\ 2E TDTST
	TOP1  C@
	-4 ALLOT
	[THUMB2]	R6 R0 ROT ## ADDS,	[P]
    M\ 2F TDTST
	FALSE
BREAK


		\	MOVS	R6, #X
TOP0 1+ C@ 36 XOR	\	ADDS	R6, #y
TOP1  C@
TOP0  C@ +  FF ANDC OR  \  X+Y < FF
0=
IF  M\ 26 TDTST
	TOP1  C@
	TOP0  C@ +  2600 OR TOP1 W! \	MOVS	R6, #X
	-2 ALLOT
    M\ 27 TDTST
	FALSE
BREAK

THEN \	MOVS	R6, #X

DUP 1- W@ 4770  XOR 
TOP1 W@ 4630 XOR OR	   \ 	MOV	R0, R6
TOP0 W@ 780 ANDC 06 XOR OR \ 	LSLS	R6, R0, #4
0=
IF  M\ 28 TDTST
	1OPexcise
	TOP0 W@ 36 OR TOP0 W! \ LSLS	R6, R6, #4
    M\ 29 TDTST
	FALSE
BREAK



    THEN \    TOP1 ?^OP


TOP2 :-SET U< IF TRUE EXIT THEN
TOP2 ?^OP
    IF

	TOP2 W@ cf40	 XOR		\	LDMIA	R7!, {R6}
	TOP1  @ 6d04f847 XOR OR		\	STR.W	R6, [R7, #-4]!
0=
IF  M\ 2 TDTST
	-4 ALLOT
	683E	TOP0 W! \	ldr	r6, [r7]
    M\ 3 TDTST
	FALSE
BREAK

TOP2 1+ C@ 26	XOR	\	MOVS	R6, #X
TOP1W@	4630	XOR OR	\	MOV	R0, R6
TOP0W@  TOS:=? 0= OR
0=
IF  M\ 8 TDTST
	20	TOP2 1+ C! \	MOVS	R0, #X
	1OPexcise
    M\ 9 TDTST
	FALSE
BREAK

TOP2 W@ 4006	XOR	\	ANDS	R6, R0
TOP2 W@ 6836	<> AND	\	LDR	R6, [R6, #0]
TOP1W@	4630	XOR OR	\	MOV	R0, R6
TOP0W@  TOS:=? 0= OR
0=
IF  M\ E TDTST
	30	TOP2  C! \	LDR	R0, [R6, #0] | ANDS	R0, R6
	1OPexcise
    M\ F TDTST
	FALSE
BREAK


TOP2 W@ F010 XOR	\
TOP1 1+ C@ 06 XOR OR	\	ANDS.W	R6, R0, #X
TOP0W@ 4236 XOR OR	\	TST	R6, R6
0=
IF  M\ 12 TDTST
	-2 ALLOT
    M\ 13 TDTST
	FALSE
BREAK

TOP2 1+ C@ 26 XOR		\	MOVS	R6, #X
TOP1  @ 20 ANDC f606fa00 XOR OR	\	LSL.W|LSR.W 	R6, R0, R6

0=
IF  M\ 14 TDTST
	TOP1  @ 20 AND >R
	TOP2  C@
	-6 ALLOT
	[THUMB2]	R6 R0 ROT ##	R> IF  LSRS, ELSE  LSLS, THEN	[P]
    M\ 15 TDTST
	FALSE
BREAK


DUP 1- W@ 4770  XOR
TOP2 1+ C@ 20 XOR OR 	\	MOVS	R0, #X
TOP1 1+ C@ 26 XOR OR 	\	MOVS	R6, #Y
TOP0 W@ 4306 XOR OR	\ 	ORRS	R6, R0
0=
IF  M\ 20 TDTST
	TOP2 C@
	TOP1 C@ OR 2600 OR TOP2 W!
	-4 ALLOT
    M\ 21 TDTST
	FALSE
BREAK


TOP2 1+ C@ 26 XOR	\ MOVS	R6, #X
TOP1 @  606EA20 XOR OR	\ BIC.W	R6, R0, R6
0=
IF  M\ 20 TDTST
	TOP2  C@
	f020 TOP2 W!	\ bic.w	r6, r0, #X
	 600 OR TOP1 W!
	-2 ALLOT
    M\ 21 TDTST
	FALSE
BREAK

TOP2 W@ TOS:=?
TOP1 @	TOS:=? AND
IF  M\ 2A TDTST
	2OPexcise
    M\ 2B TDTST
	FALSE
BREAK

DUP 1- W@ 4770  XOR 
TOP2 W@ 4630 XOR OR	\ MOV	R0, R6
TOP1 @  8F00FFFF AND
	0600F110 XOR OR	\ ADDS.W R6, R0, #127 ; 0x7F
0=
IF  M\ 30 TDTST
	2OPexcise
	00000006  TOP1 XOR!	\ ADDS.W R6, R6, #127 ; 0x7F

    M\ 31 TDTST
	FALSE
BREAK


    THEN \    TOP2 ?^OP

  TOP3 :-SET U< IF TRUE EXIT THEN
    TOP3 ?^OP
    IF

TOP3 @ 6d04f847 XOR \	STR.W	R6, [R7, #-4]!
TOP1W@ 687e XOR OR  \	LDR	R6, [R7, #4]
TOP0W@ cf01 XOR OR  \	LDMIA	R7!, {R0}
0=
IF  M\ 1C TDTST

	4630	TOP3 W!	\	MOV	R0, R6
	683e	TOP2 W!	\	LDR	R6, [R7, #0]
	-4 ALLOT
    M\ 1D TDTST
	FALSE
BREAK


TOP3 @  6d04f847 XOR	\	STR.W	R6, [R7, #-4]!
TOP1 1+ C@ 20 XOR OR	\	MOVS	R0, #X
TOP0 W@ 683e XOR OR	\	LDR	R6, [R7, #0]
0=
IF  M\ 20 TDTST
	-2 ALLOT
    M\ 21 TDTST
	FALSE
BREAK

TOP3 W@ 2608 XOR		\	MOVS	R6, #8
TOP2 W@ cf01 XOR OR		\	LDMIA	R7!, {R0}
TOP1  @ 20 ANDC F606FA00 XOR OR	\	LSL.W|LSR.W 	R6, R0, R6
0=
IF  M\ 2C TDTST
	TOP1  @ 20 AND >R
	TOP3  C@
	TOP2 W@ TOP3 W!
	-6 ALLOT
 	[THUMB2]	R6 R0 ROT ##	R> IF  LSRS, ELSE  LSLS, THEN	[P]
	
    M\ 2D TDTST
	FALSE
BREAK



    THEN \    TOP3 ?^OP

  TOP5 :-SET U< IF TRUE EXIT THEN
    TOP5 ?^OP
    IF


    THEN \    TOP5 ?^OP

 TRUE
;



: MTOPT-RULES  ( ADDR  -- ADDR' FLAG )
\ TOP0 W@ TO TOP0W@

  TOP3
 :-SET
 U<
 IF TRUE BREAK


	6d04f847		\ STR.W	R6, [R7, #-4]!
	TOP0W@ CF01	XOR	\ LDMIA	R7!, {R0}
	TOP0 WOPLOOK
	ST>R0?
0=
IF  M\ 6 TDTST
	TOPN TOPexcise
\ CR ."	F7TTTT"
	4630	TOPN W! \	MOV	R0, R6
	-2 ALLOT


	TOPN 2+ @	TOS:=?
	TOPN 2+ @	0042e89d = OR \       LDMIA.W	R13, {R1, R6}
	IF	TOPN 2- ?^OP
		IF
			TOPN 2- W@
		     CASE

\			4306 	\	ORRS	R6, R0
			DUP FC3F AND 4006 =
			IF	06 XOR 30  XOR TOPN 2- W!	\      	orrs	r0, r6
				TOPN TOPexcise
			ENDOF

			DUP F807 AND 0006 =	\	LSLS	R6, RY, #X
			IF 6 XOR TOPN 2- W!	\	LSLS	R0, RY, #X
				TOPN TOPexcise
			ENDOF
			8 >>
			26	\	MOVS	R6, #X
			OF	20	TOPN 1- C!		\	MOVS	R0, #X
				TOPN TOPexcise
			ENDOF
		
	            ENDCASE
		THEN
		
	        TOPN 4 - ?^OP
		IF
			TOPN 4 - @
			8F00FBF0 AND
			0600f240 XOR	\	movw	r6, #X

			TOPN 4 - @
			8F00FBCF AND
			0600F04F <> AND 0=	\	mvn|mov.w	r6, #X
			IF 06000000 TOPN 4 - XOR!  \	mvn|mov.w	r0, #X
				TOPN TOPexcise
			THEN

		THEN
	THEN
		

    M\ 7 TDTST
	FALSE
BREAK

 TRUE
;


: TBOPT-RULES  ( ADDR  -- ADDR' FLAG )
  TOP1 :-SET U< IF TRUE EXIT THEN
  TOP0 W@ TO TOP0W@
  TOP1 W@ TO TOP1W@
M\ -1 TDTST

  TOP3 :-SET U< IF TRUE EXIT THEN

    TOP3 ?^OP
    IF

	TOP3  @ 6d04f847 XOR	\ STR.W	R6, [R7, #-4]!
	TOP0W@ cf40	XOR OR	\ LDMIA	R7!, {R6}
0=
IF
	TOP1W@ 4236	=	\ TST	R6, R6
	IF
  M\ 18 TDTST
	2OPexcise
	2OPexcise
	-2 ALLOT
    M\ 19 TDTST
	FALSE
	BREAK

	TOP1W@ 17f6	<>	\ ASRS	R6, R6, #31
	TOP1W@ 0ff6	XOR AND	\ LSRS	R6, R6, #31
0=	IF
  M\ 24 TDTST
	2OPexcise
	2OPexcise
	f0  TOP1 C! \   	lsrs|asrs	r0, r6, #31
	-2 ALLOT
    M\ 25 TDTST
	FALSE
	BREAK

	TRUE
BREAK \ LDMIA	R4!, {R5}

    THEN \    TOP3 ?^OP

 TRUE
;

: -EVEN-PSP
\    TOP0 :-SET U< IF EXIT BREAK
 ;

: EVEN-PSP ;

: TOPT_  ( -- )
 BEGIN
 BEGIN
  TOPT-RULES UNTIL
 MTOPT-RULES UNTIL

  EVEN-PSP  ;

: DO_TOPT   ( ADDR -- ADDR' )
  TOPT? IF TOPT_ THEN ;

: TOPT   ( -- )  'OVER DO_TOPT DROP  ;

: TBOPT_  ( -- )
  BEGIN
\   M\ -D TDTST
 TBOPT-RULES
 UNTIL
\   M\ -F TDTST
  ;

: DO_TBOPT   ( ADDR -- ADDR' )
  TOPT? IF TBOPT_ THEN ;

: TBOPT   ( -- )  'OVER DO_TBOPT DROP  ;

: TINLINE?  ( CFA -- CFA FLAG )
\ FALSE  EXIT
\ cr ." TINLINE?"
  DUP         BEGIN
  2DUP
  TMM_SIZE -   U> 0= IF  DROP FALSE  EXIT THEN
  DUP 1- W@ \ dup h.    \  CFA CFA+OFF N'
  DUP	4770 =	IF 2DROP  TRUE EXIT THEN \	BX	R14
  DUP	4470 <	M_WL DROP 2+ REPEAT	\	add	r0, lr
  DUP	FFC0 AND
	4600 =	M_WL DROP 2+ REPEAT	\	MOV	R0-7, R0-7
  DUP	5000 A000  WITHIN
		M_WL DROP 2+ REPEAT	\	str	r0, [r0, r0]   add	r0, pc, #0
  DUP	9E00 =	M_WL DROP 2+ REPEAT	\	LDR	R6, [SP, #0]
  DUP	B001 =	M_WL DROP 2+ REPEAT	\	ADD	SP, #4
  DUP	F700 AND
	B400 =	M_WL DROP 2+ REPEAT	\	PUSH POP {R1-R7}
  DUP	F000 AND
	C000 =	M_WL DROP 2+ REPEAT	\	LDMIA	R0-7!, {...}
  DUP	e927 =	M_WL DROP 4 + REPEAT	\	stmdb.w	r7!,  {...}  !!!

  DROP
  DUP 1- @
  DUP	f606fb00 = M_WL DROP 4 + REPEAT  \ mul.w	R6, R0, R6
  DUP	f6f6fb90 = M_WL DROP 4 + REPEAT  \ SDIV		R6, R0, R6
  DUP	6026f857 = M_WL DROP 4 + REPEAT  \ LDR.W	R6, [R7, R6, lsl #2]
  DUP	0606ea30 = M_WL DROP 4 + REPEAT  \ BICS.W	R6, R0, R6
  DUP	0606ea20 = M_WL DROP 4 + REPEAT  \ BIC.W	R6, R0, R6
  DUP	76e6ea4f = M_WL DROP 4 + REPEAT  \ MOV.W	R6, R6, asr #31
  DUP	36fff04f = M_WL DROP 4 + REPEAT  \ MOV.W	R6, #-1
  DUP	0041e89d = M_WL DROP 4 + REPEAT  \ LDMIA.W	R13, {R0, R6}
  DUP	0042e89d = M_WL DROP 4 + REPEAT  \ LDMIA.W	R13, {R1, R6}
  DUP	FFFFFF8F AND
  	f606fa00 = M_WL DROP 4 + REPEAT  \ LSL.W	R6, R0, R6
  DUP	6d04f847 = M_WL DROP 4 + REPEAT  \ STR.W	R6, [R7, #-4]!
  DUP	0d04f847 = M_WL DROP 4 + REPEAT  \ STR.W	R0, [R7, #-4]!
  DUP	6b04f857 = M_WL DROP 4 + REPEAT  \ LDR.W	R6, [R7], #4

  DUP	1a01ecf7 = M_WL DROP 4 + REPEAT  \ vldmia	r7!, {s3}

  DUP	0F00FF00 AND
	0a00ee00 = M_WL DROP 4 + REPEAT  \ VMLA.F32

  DUP	0b04f857 = WHILE DROP 4 + REPEAT  \ LDR.W		R0, [R7], #4
  2DROP  FALSE  EXIT
;

\  

: T_INLINE,  (  CFA  --  )
\  ." ^" DUP H. HEX
	BEGIN
  DO_TOPT
  DUP 1- W@      \  CFA  N'
  DUP 0x4770 = IF 2DROP     EXIT THEN \ BX	R14
  DUP ?HOP M_WL  W, 2+ REPEAT
\  DROP DUP 1- @  , CELL+
   OVER 1+ W@ 0x10 LSHIFT OR , CELL+
	AGAIN
;

: TOPT_CLOSE
   EVEN-PSP
 DP @
 TO TLAST-HERE
 ;

: TOPT; TOPT  TOPT_CLOSE ;

: TOPT_INIT
  ?TSET
 -EVEN-PSP ;

: TINLINE, ( CFA --  )
  TOPT_INIT
  T_INLINE,
  TOPT_CLOSE ;

M\  30 VALUE MAXROOL CREATE ROOLSTAT   MAXROOL 1+ CELLS ALLOT
M\ :  &TDTST  DUP 1 AND 0= IF  DUP MAXROOL 1+ MIN 2*  ROOLSTAT + 1+!  THEN DROP ;  ' &TDTST TO TDTST
M\ :  RSTAT  MAXROOL 1+ 0 DO CR I H. ROOLSTAT I 2* + @ . 2 +LOOP  ;

 BASE !

\ 18 TDTST  22 TDTST 16 TDTST 1A TDTST 1E TDTST
