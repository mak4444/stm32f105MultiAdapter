REQUIRE [IF] ~mak/CompIF1.f
REQUIRE [IFNDEF] ~nn/lib/ifdef.f
REQUIRE PLACE  ~mak/place.f
REQUIRE CASE lib/ext/case.f
REQUIRE 'LIT	MetaCortex\SRC\forward.f
\ REQUIRE [IF] ~mak\CompIF.f
[IFNDEF] H. : H.  BASE @ HEX SWAP U. BASE !  ;
[THEN]

[IFNDEF] BOUNDS : BOUNDS OVER + SWAP ;
[THEN]

[IFNDEF] W>S
: W>S ( w -- n )  \ 
  0xFFFF AND    \ 
 [ 0x7FFF INVERT ] LITERAL XOR 0x8000 + ;
[THEN]

[IFNDEF] BLANK
: BLANK         ( addr len -- )     \ fill addr for len with spaces (blanks)
                BL FILL ;
[THEN]

[IFNDEF] (D.)
: (D.)          ( d -- addr len )       TUCK DABS  <# #S ROT SIGN #> ;
[THEN]

[IFNDEF] H.R
: H.R    ( n1 n2 -- )    \ display n1 as a hex number right
                        \ justified in a field of n2 characters
          BASE @ >R HEX >R
          0 <# #S #> R> OVER - 0 MAX SPACES TYPE
          R> BASE ! ;
[THEN]

[IFNDEF] TAB
  : TAB 9 EMIT ;
[THEN]

[IFNDEF] H.N
: H.N           ( n1 n2 -- )    \ display n1 as a HEX number of n2 digits
                BASE @ >R HEX >R
                0 <# R> 0 ?DO # LOOP #> TYPE
                R> BASE ! ;
[THEN]

[IFNDEF] BREAK	: BREAK  POSTPONE EXIT POSTPONE THEN ; IMMEDIATE [THEN]
[IFNDEF] <<	: << LSHIFT ;	[THEN]

[IFNDEF] >>	: >> RSHIFT ;	[THEN]

[IFNDEF] ANDC : ANDC INVERT AND ; [THEN]

[IFNDEF] GTYPE

: GTYPE \ ." _"
  0 DO  MCOUNT
      CASE
      [CHAR] @ OF ." load" ENDOF
      [CHAR] ! OF ." save" ENDOF
      [CHAR] = OF ." equ" ENDOF
      [CHAR] < OF ." less" ENDOF
      [CHAR] > OF ." great" ENDOF
      [CHAR] + OF ." add" ENDOF
      [CHAR] - OF ." sub" ENDOF
      [CHAR] * OF ." mul" ENDOF
      [CHAR] / OF ." rsl" ENDOF
      [CHAR] \ OF ." sl" ENDOF
      [CHAR] . OF ." dot" ENDOF
      [CHAR] : OF ." dcoma" ENDOF
      [CHAR] ; OF ." end" ENDOF
      [CHAR] ? OF ." que" ENDOF
      [CHAR] ( OF ." c" ENDOF
      [CHAR] ) OF ." cend" ENDOF
      [CHAR] [ OF ." x" ENDOF
      [CHAR] ] OF ." y" ENDOF
      [CHAR] { OF ." lsk" ENDOF
      [CHAR] } OF ." rsk" ENDOF
      [CHAR] , OF ." com" ENDOF
      [CHAR] ' OF ." tic" ENDOF
      [CHAR] ~ OF ." til" ENDOF
      [CHAR] ^ OF ." pic" ENDOF
      [CHAR] " OF ." dtic" ENDOF
      [CHAR] | OF ." l" ENDOF
      [CHAR] # OF ." n" ENDOF
      [CHAR] $ OF ." dol" ENDOF
      [CHAR] & OF ." and" ENDOF
	I IF EMIT ENDOF
      [CHAR] 0 OF ." _0" ENDOF
      [CHAR] 1 OF ." _1" ENDOF
      [CHAR] 2 OF ." _2" ENDOF
      [CHAR] 3 OF ." _3" ENDOF
      [CHAR] 4 OF ." _4" ENDOF
      [CHAR] 5 OF ." _5" ENDOF
      [CHAR] 6 OF ." _6" ENDOF
      [CHAR] 7 OF ." _7" ENDOF
      [CHAR] 8 OF ." _8" ENDOF
      [CHAR] 9 OF ." _9" ENDOF 
               EMIT
    0 ENDCASE
     LOOP DROP ;
[THEN]

0 VALUE ADDR_OFF
0 value vsymptom
0 VALUE VHS?

\ [IFNDEF] ?.NAME>S
: ?.NAME>S      ( CFA -- )
\ ELIMINATE " 0x"
	DUP ADDR_OFF + H.  \ 1 H.R>S SSPACE
	1 OR NEAR_NFA 
	>R DUP
	IF ."  ( " DUP MCOUNT GTYPE 
	     NAME>  R> - DUP
	     IF   DUP ." +" NEGATE H. \ >S
	     THEN DROP        ."  ) "
	ELSE RDROP DROP
	THEN ;
\ [THEN]

: ?.NAME>SS      ( CFA -- )
	1 OR NEAR_NFA 
	>R DUP
	IF   DUP MCOUNT GTYPE 
	     NAME>  R> - DUP
	     IF   DUP ." +" NEGATE H. \ >S
	     THEN DROP
	ELSE RDROP DROP
	THEN ;

MODULE: DISARM
: XX.  CR ." X=" 2DUP H. H. ;


0 VALUE OP_NAME
0 VALUE OPCODE
VECT OP.

0 VALUE ?SSS

: SSS -1 TO ?SSS ;

:  [|;] (  cod  cod1 cod2  <NAME>  -- )
   TO OP_NAME
   OPCODE = 
   IF OP. RDROP RDROP
   THEN 0 TO ?SSS ;

: |;
     LAST M@  LIT,
     POSTPONE [|;]
     POSTPONE  ;
; IMMEDIATE


: .-  S>D (D.) TYPE ;
: U.-  0 (D.) TYPE ;
: H.-   BASE M@ HEX SWAP U>D (D.) TYPE BASE M! ;

0 VALUE IT_V
0 VALUE IT_N

: OP_TYPE 
 TAB
 VHS? IF   ." //" THEN
 OP_NAME   MCOUNT 1- 

    IT_V 0xF AND
 IF ?SSS IF 1- THEN
  2DUP + 2- MW@ 0x2000 OR 0x772E ( .w ) = DUP >R
	IF 2- THEN
	TYPE

	IT_V 0x10 AND IT_N XOR 3 >>
 S" EQNECSCCMIPLVSVCHILSGELTGTLEAL??" DROP +  2 TYPE
	IT_V  1 << IT_N XOR 0x1F AND TO IT_V

	R> IF ." .W" THEN
 ELSE   TYPE
	
 THEN

 ;


: NOP_TP TAB OP_TYPE DROP ;

: NOP{_TP TAB OP_TYPE TAB  ." {"  0xF0 AND 4 >> .- ." }"  ;

: svc_tp	TAB OP_TYPE TAB 0xFF AND .- ;
: bkpt_tp	TAB OP_TYPE TAB 0xFF AND ." 0x" H.- ;

: cpsie_TP
 TAB  OP_TYPE TAB
 DUP 4 AND IF ." A" THEN
 DUP 2 AND IF ." I" THEN
 DUP 1 AND IF ." F" THEN
 DROP
;

VECT ."_R"

: .",R" ." , R" ;
: ."R" ." R" ['] .",R" TO ."_R" ;



: B_POP
 TAB  OP_TYPE TAB
\ DROP EXIT
 ." {"
    ['] ."R" TO ."_R"
  8 0	DO DUP 1 AND IF  ."_R" I .-	THEN 1 >>
	LOOP
  DUP	0xB5 = IF  ."_R" 14 .- THEN
	0xBD = IF  ."_R" 15 .- THEN
 ." }" 

;


: STMIA_TP
 TAB  OP_TYPE TAB
	." R"	DUP    0x700 AND 8 >> .-  ." !" 

 ." , {"
    ['] ."R" TO ."_R"
  8 0	DO DUP 1 AND IF  ."_R" I .-	THEN 1 >>
	LOOP
	DROP
 ." }" 
;

: LDMIA_TP
 TAB  OP_TYPE TAB
	." R"	DUP    0x700 AND 8 >> DUP .-  
	1 SWAP << OVER AND 0= IF ." !" THEN

 ." , {"
    ['] ."R" TO ."_R"
  8 0	DO DUP 1 AND IF  ."_R" I .-	THEN 1 >>
	LOOP
	DROP
 ." }" 
;



: CBZ_TP
 TAB  OP_TYPE TAB

	." R"		DUP    7 AND .-  
	." , "

  DUP  0xF8	AND 2 >>
  SWAP 0x200	AND 3 >> OR

  OVER + 4 + H.-

 ;


: B_LDRR
 TAB  OP_TYPE TAB
\ DROP EXIT
	." R"		DUP 0x700 AND 8 >> .-  
	." , [SP, #"	DUP 0xFF  AND 2 << .-  ." ]"
	DROP

;

1 VALUE LDR=?

: LDR=_TP
 TAB   OP_TYPE TAB
\ DROP EXIT
	." R"		DUP 0x700 AND 8 >> .-  
	LDR=?
	IF
	." , [PC, #"	DUP 0xFF  AND 2 << .-  ." ] @ [0x"

	2DUP 0xFF  AND 2 <<  +   2+ 3 + 3 ANDC   DUP H.-   ." ] 0x" @ H.-
	ELSE
	." , = "	2DUP 0xFF  AND 2 <<  +   2+ 3 + 3 ANDC @

	CASE
	0x48028300 OF ." P3_OUT" ENDOF
	DUP ." 0x" H.-
	ENDCASE
	

	THEN

	DROP
 
;


: B_LDR,
 TAB  OP_TYPE TAB
\ DROP EXIT
	." R"		DUP    7 AND .-  
	." , [R"	DUP 0x38 AND 3 >> .-
	." , R"		DUP 0x1C0 AND 6 >> .-  ." ]"
	DROP

;

0 VALUE QSH

: B_LDR#
 TAB  OP_TYPE TAB
\ DROP EXIT
	." R"		DUP    7 AND .-  
	." , [R"	DUP 0x38 AND 3 >> .-
	." , #"		DUP 0x7C0 AND QSH >> .-  ." ]"
	DROP

;


: LSLS_TP
 TAB  OP_TYPE TAB
\ DROP EXIT
	." R"		DUP    7 AND .-  
	." , R"		DUP 0x38 AND 3 >> .-
	." , #"		DUP 0x7C0 AND 6 >> .-
	DROP

;

: BX_TP
 TAB  OP_TYPE TAB
\ DROP EXIT
	." R"	DUP 0x78 AND 3 >> .-  
	DROP
 
;

: B_TST
 TAB  OP_TYPE TAB
\ DROP EXIT
	." R"	DUP    7 AND  .-  
	." , R"	DUP 0x38 AND 3 >> .-  
	DROP

;

: B_MOV
 TAB  OP_TYPE TAB
\ DROP EXIT
	." R"
 DUP 0x4700 U<
	IF	DUP    7 AND OVER 0x80 AND IF 8 OR THEN .-  ." , R"
	THEN
	DUP 0x78 AND 3 >> .-  
	DROP
 
;

: B_MOVS#
 TAB  OP_TYPE TAB
\ DROP EXIT
	." R"	DUP 0x700 AND 8 >> .-  
	." , #"	DUP 0xFF AND  .-  
	DROP
 
;


: ADDS_TP
 TAB  OP_TYPE TAB
\ DROP EXIT
	." R"	DUP 0x7 AND .-  3 >> 
	." , R"	DUP 0x7 AND .-  3 >> 
	." , R"	DUP 0x7 AND .-	DROP
 
;


: ADDS#_TP
 TAB  OP_TYPE TAB
\ DROP EXIT
	." R"	DUP 0x7 AND .-  3 >> 
	." , R"	DUP 0x7 AND .-  3 >> 
	." , #"	DUP 0x7 AND .-	DROP
 
;

: B_ADD_SP
 TAB  OP_TYPE TAB
	." SP, #" DUP 0x7F AND 2 <<  .-  
	DROP 
;

: .H.-  DUP U.  DUP 9 U> IF  ." @ 0x" DUP H.-  THEN DROP  ;

: ADD_R_SP_TP
 TAB  OP_TYPE TAB
\ DROP EXIT
	." R"	DUP 0x700 AND  8 >>  .-
		DUP 0x800 AND
	IF	." , SP, #"
	ELSE	." , PC, #"
	THEN	DUP 0xFF AND 2 << .H.-  
	DROP 
;


: B_B.N
 TAB  OP_TYPE TAB
\ DROP EXIT
  0x7FF AND 0x400 XOR 0x400 - 2* OVER +
 4 +
 H.-
 
;

: BNE.N_TP

 TAB  OP_TYPE TAB
\ DROP EXIT
  0xFF AND 0x80 XOR 0x80 -  2* OVER +
 4 +
 H.-

;

: NOP.W_TP
  DUP 0xFFFF AND SPACE 4 H.N
   OP_TYPE TAB
	DROP 2+
;



: LDREXB_TP
  DUP 0xFFFF AND SPACE 4 H.N
   OP_TYPE TAB
	." R"	 DUP 0x0F000 AND 12 >> .- 
	." , [R" DUP 0xF0000 AND 16 >> .-  ." ]"
	DROP 2+
;

: LDREX_TP
  DUP 0xFFFF AND SPACE 4 H.N
   OP_TYPE TAB
	." R"	 DUP 0x0F000 AND 12 >> .- 
	." , [R" DUP 0xF0000 AND 16 >> .-

  DUP 0xFF AND 0<>
	IF ." , #" DUP 0xFF AND 2 << .-

	THEN	." ]"

	DROP 2+
;

: ,0x.
 VHS?
 IF DUP 0xFFFF AND ." ,0x" 4 H.N 
 THEN ;

: strexB_TP
  ,0x.
   OP_TYPE TAB
	." R"	DUP 0xF AND .- 
	." ,R"	DUP 0x0F000 AND 12 >> .- 
	." , [R" DUP 0xF0000 AND 16 >> .-  ." ]"
	DROP 2+
;

: strex_TP
  ,0x.
   OP_TYPE TAB
	." R"	 DUP 0x00F00 AND  8 >> .- 
	." ,R"	 DUP 0x0F000 AND 12 >> .- 
	." , [R" DUP 0xF0000 AND 16 >> .-
  DUP 0xFF AND 0<>
	IF ." , #" DUP 0xFF AND 2 << .-

	THEN	." ]"
	DROP 2+
;

: LDMIA.W_TP
  ,0x.
   OP_TYPE TAB
	." R"	DUP    0xF0000 AND 16 >> .-  
	DUP 0x200000 AND IF ." !" THEN

 ." , {"
    ['] ."R" TO ."_R"
  16 0	DO DUP 1 AND IF  ."_R" I .-	THEN 1 >>
	LOOP
 ." }"
	DROP 2+
;

: SXTH.W_TP
  ,0x.

   OP_TYPE TAB

	." R"	DUP 0x00F00 AND 8 >> .-  
	." , R" DUP 0x0000F AND .-
	0x30 AND
	DUP 0x10 = IF ." , ror #8" ELSE
	DUP 0x20 = IF ." , ror #16" ELSE
	DUP 0x30 = IF ." , ror #24" ELSE
	THEN THEN THEN
	DROP 2+
;

: SXTAH.W_TP
  ,0x.

   OP_TYPE TAB

	." R"	DUP 0x00F00 AND 8 >> .-  
	." , R" DUP 0xf0000 AND 16 >> .-
	." , R" DUP 0x0000F AND .-
	0x30 AND
	DUP 0x10 = IF ." , ror #8" ELSE
	DUP 0x20 = IF ." , ror #16" ELSE
	DUP 0x30 = IF ." , ror #24" ELSE
	THEN THEN THEN
	DROP 2+
;

: REV.W_TP
  ,0x.

   OP_TYPE TAB

	." R"	DUP 0x00F00 AND 8 >> .-
	." , R" DUP 0x0000F AND .-
	DROP 2+
;

: R,S_TP
  ,0x.

   OP_TYPE TAB

	." R"	DUP	0x00F000 AND 12 >> .-

	." , S"	DUP	0x0F0000 AND 15 >>
		OVER	0x000080 AND  7 >> OR .-
	DROP 2+
;

: S,R_TP
  ,0x.

   OP_TYPE TAB

	." S"	DUP	0x0F0000 AND 15 >>
		OVER	0x000080 AND  7 >> OR .-

	." , R"	DUP	0x00F000 AND 12 >> .-

	DROP 2+
;

: S,S_TP
  ,0x.

   OP_TYPE TAB

	." S"	DUP	0x00F000 AND 11 >>
		OVER	0x400000 AND 22 >> OR .-

	." , S" DUP	0x0000F AND 1 <<
		OVER	0x00020 AND 5 >> OR .-
	DROP 2+
;

: S,S,S_TP
  ,0x.

   OP_TYPE TAB

	." S"	DUP	0x00F000 AND 11 >>
		OVER	0x400000 AND 22 >> OR .-

	." , S"	DUP	0x0F0000 AND 15 >>
		OVER	0x000080 AND  7 >> OR .-

	." , S" DUP	0x0000F AND 1 <<
		OVER	0x00020 AND 5 >> OR .-
	DROP 2+
;


: R,{S}_TP
  ,0x.

   OP_TYPE TAB

	." R"		DUP	0x0F0000 AND 16 >> .-

	DUP 0x200000 AND IF ." !" THEN
	." , {S"	DUP	0x00F000 AND 11 >>
			OVER	0x400000 AND 22 >> OR DUP>R .-

	DUP	0xFF AND 1- DUP
	IF   DUP   ." -S" R@ +  .-
	THEN	DROP	RDROP
	

	." }"

	DROP 2+
;

: R,fpscr_TP
  ,0x.
 OP_TYPE TAB
  DUP  0xF000 AND
  DUP  0xF000 =
  IF  ." APSR_nzcv" DROP
  ELSE ." R" 12 >> .-
  THEN  ." , fpscr"
  
	DROP 2+
;

: SDIV_TP
  ,0x.

   OP_TYPE TAB

	." R"	DUP 0x00F00 AND 8 >> .-  
        ." , R"	DUP 0xF0000 AND 16 >> .-  
	." , R" DUP 0x0000F AND .-
	DROP 2+

;


: MLS_TP
  ,0x.

   OP_TYPE TAB

	." R"	DUP 0x00F00 AND 8 >> .-  
        ." , R"	DUP 0xF0000 AND 16 >> .-  
	." , R" DUP 0x0000F AND .-
        ." , R"	DUP 0x0F000 AND 12 >> .-  
	DROP 2+

;

: SMULL_TP
  ,0x.

   OP_TYPE TAB

        ." R"	DUP 0x0F000 AND 12 >> .-  
	." , R"	DUP 0x00F00 AND 8 >> .-
        ." , R"	DUP 0xF0000 AND 16 >> .-  
	." , R" DUP 0x0000F AND .-
	DROP 2+

;

: usat_TP
  ,0x.

   OP_TYPE TAB

	." R"	DUP 0x00F00 AND 8 >> .-  
	." , #" DUP 0x0000F AND .-
        ." , R"	DUP 0xF0000 AND 16 >> .-  
		DUP 0x70C0  AND
	IF      DUP 0x00200000  AND IF ." , asr #" ELSE ." , lsl #" THEN
		DUP	0x00C0 AND  6 >>
		OVER	0x70C0 AND 10 >> OR .-
	THEN


	DROP 2+

;


: R_SHIFT.
	." , R" DUP  0x0000F AND	.- 
		DUP  0x000c0 AND 6  >>
		OVER 0x07000 AND 10 >> OR DUP

	IF      ." , "
		OVER 0x00030 AND	2 >>                
		S" lsl lsr asr ror " DROP + 4 TYPE
		." #" .-


	ELSE	DROP
		DUP 0x00030 AND	
		DUP 0x00000 <> IF
		DUP 0x00010 =  IF ."  , lsr #32" ELSE
		DUP 0x00020 =  IF ."  , asr #32" ELSE
				 ."  , rrx"
		THEN THEN THEN
		DROP
	THEN


	DROP 2+ ;

: MOV.W_TP
  ,0x.

   OP_TYPE TAB
	." R"	DUP  0x00F00 AND 8  >>	.-
	R_SHIFT.
;

: CMP.W_TP
  ,0x.

   OP_TYPE TAB
	." R"	DUP  0xF0000 AND 16 >>	.-
	R_SHIFT.
;

: ADD.W,_TP
  ,0x.

   OP_TYPE TAB

	." R"	DUP  0x00F00 AND 8  >>	.-
        ." , R"	DUP  0xF0000 AND 16 >>	.-
	R_SHIFT.

;


: THUMBEXPANDIMM.

	." , #"	DUP  0x04007000 AND DUP
	IF	DUP 0x1000 = IF DROP 0xFF AND	   DUP 16 << OR .H.- ELSE
		DUP 0x2000 = IF DROP 0xFF AND 8 << DUP 16 << OR .H.- ELSE
		DUP 0x3000 = IF DROP 0xFF AND	   DUP 8 << OR 
				DUP 16 << OR .H.- ELSE

		DUP 11 >> 0xE AND SWAP 22 >> OR  OVER 0x80 AND 7 >> OR
		SWAP 0x7F AND 0x80 OR SWAP 0x20 - NEGATE <<	.H.-
\   2DROP
\ ." ThumbExpandImm_C"
		THEN	THEN	THEN

	ELSE DROP 0xFF AND .H.-
	THEN

;




: MVN.W#_TP
  ,0x.

   OP_TYPE TAB

        ." R"	DUP       0xf00 AND 8 >> .-
	THUMBEXPANDIMM.
	2+

;

: CMP.W#_TP
  ,0x.

   OP_TYPE TAB

	." R"	DUP  0xF0000 AND 16 >>	.-
	THUMBEXPANDIMM.
	2+
;

: MOVW># ( cod -- w )
	DUP	0xFF AND
        OVER	0x00007000 AND  4 >> OR
        OVER	0x04000000 AND 15 >> OR
        SWAP	0x000F0000 AND  4 >> OR
;

: MOV32@ ( adr -- u )
   dup     DUP W@ 16 << SWAP 2+ W@ OR MOVW>#
  swap 4 + DUP W@ 16 << SWAP 2+ W@ OR MOVW># 16 << OR
;

	
0 value vtag
0 value ctag

: VAR_?  ( addr u --  addr u )
  2DUP SWAP  HSSSS @ + MOV32@  - vsymptom = TO vtag
;

: ROM_?  ( addr u --  addr u )
\  2DUP SWAP  HSSSS @ + MOV32@  - NEGATE DUP H. HSSSS @ DUP H. XOR 0= IF ." <ROM>" THEN
  2DUP SWAP  HSSSS @ + MOV32@  -  HSSSS @ + 0= TO ctag \ IF ." <ROM>" THEN
;

:  sp_buff.
  vtag IF 0x20008000 - ." sp_buff+" THEN
  ctag IF HSSSS @ + ZSSSS M@ - ." fimg+" THEN
 ;	

0 VALUE lower16
: MOVW,_TP
  ,0x.
   OP_TYPE TAB

        ." R"	DUP       0xf00 AND 8 >> .-
	." , #"

	MOVW># TO lower16

	DUP 2@
	0x8F00FBF0 AND SWAP 0x8F00FBF0  AND 
	- -$80 =
	IF   ." :lower16:" DUP  MOV32@ DUP >R  VAR_? ROM_? sp_buff.   .H.-
	     4 + CR TAB
		 VHS?
		 IF
		    ." .inst.n" TAB
 
			DUP	W@ 4 ." 0x" H.N DUP 2+ W@  ." ,0x" 4 H.N  TAB ." //"
		 THEN
		  ." MOVT" TAB
        	." R"	DUP    2+ W@  0xf00 AND 8 >> .-
		." , #:upper16:"	R> sp_buff. .H.-

	ELSE  lower16  .H.-
	THEN
	2+

;

: ADDW,_TP
  ,0x.

   OP_TYPE TAB

	." R"	DUP  0x00F00 AND 8  >>	.-  
        ." , R"	DUP  0xF0000 AND 16 >>	.-  

	." , #"
	DUP	0xFF AND
        OVER	0x00007000 AND  4 >> OR
        SWAP	0x04000000 AND 15 >> OR
		.H.-

	2+

;

: ADD.W#_TP
  ,0x.

   OP_TYPE TAB

	." R"	DUP  0x00F00 AND 8  >>	.-  
        ." , R"	DUP  0xF0000 AND 16 >>	.-  
	THUMBEXPANDIMM.
	2+

;


: B_BL

  ,0x.

   OP_TYPE TAB

  DUP	0x00007FF AND  1 << 
 OVER	0x3FF0000 AND  4 >>  OR
 OVER	0x0000800 AND 11 <<  OR
 OVER   0x0002000 AND 10 <<  OR
 SWAP	0x4000000 AND  2 >>  OR
 DUP	0x1000000 AND   0= IF 0xC00000 XOR THEN  
	0x1000000 XOR
	0x1000000 -   
  OVER + 4 + DUP
  ?.NAME>SS
  'LIT =
  IF 4 +  CR
	DUP ADDR_OFF + 8 H.R ."    "
	DUP W@ 4 H.N SPACE
	DUP 2 + W@ 4 H.N TAB ." .INT" TAB DUP @ .-
  THEN
 2+ 
;


: BEQ.W_TP

  ,0x.

   OP_TYPE TAB

  DUP	0x00007FF AND  1 << 
 OVER	0x03F0000 AND  4 >>  OR
 OVER	0x0000800 AND  7 <<  OR
 OVER   0x0002000 AND  6 <<  OR
 SWAP	0x4000000 AND  6 >>  OR
\ DUP	0x0100000 AND   0= IF 0x0C0000 XOR THEN  
	0x0100000 XOR
	0x0100000 -     OVER + 4 +  ?.NAME>SS
 2+ 
;


: PLI_TP
  ,0x.

   OP_TYPE TAB

        ." [R"  DUP  0xf0000 AND 16 >> .-

  DUP 0xFFF AND 0<>
	IF ." , #" DUP 0xFFF AND .-

	THEN	." ]"

	DROP 2+
;

: mrs_TP
  ,0x.

   OP_TYPE TAB
    
	." R"	  DUP  0x00f00 AND 8 >> .-  ." ,"
      DUP  0x000FF AND 
	CASE
     0x00 OF ." APSR"          ENDOF
     0x01 OF ." IAPSR"         ENDOF
     0x02 OF ." EAPSR"         ENDOF
     0x03 OF ." PSR"           ENDOF
     0x05 OF ." IPSR"          ENDOF
     0x06 OF ." EPSR"          ENDOF
     0x07 OF ." IEPSR"         ENDOF
     0x08 OF ." MSP"           ENDOF
     0x09 OF ." PSP"           ENDOF
     0x10 OF ." PRIMASK"       ENDOF
     0x11 OF ." BASEPRI"       ENDOF
     0x12 OF ." BASEPRI_MASK"  ENDOF
     0x13 OF ." FAULTMASK"     ENDOF
     0x14 OF ." CONTROL"       ENDOF
	 ." <unknown>"
	ENDCASE
	DROP 2+
;


: msr_TP
  ,0x.

   OP_TYPE TAB
    
      DUP  0x000FF AND 
	CASE
     0x00 OF ." APSR"          ENDOF
     0x01 OF ." IAPSR"         ENDOF
     0x02 OF ." EAPSR"         ENDOF
     0x03 OF ." PSR"           ENDOF
     0x05 OF ." IPSR"          ENDOF
     0x06 OF ." EPSR"          ENDOF
     0x07 OF ." IEPSR"         ENDOF
     0x08 OF ." MSP"           ENDOF
     0x09 OF ." PSP"           ENDOF
     0x10 OF ." PRIMASK"       ENDOF
     0x11 OF ." BASEPRI"       ENDOF
     0x12 OF ." BASEPRI_MASK"  ENDOF
     0x13 OF ." FAULTMASK"     ENDOF
     0x14 OF ." CONTROL"       ENDOF
	 ." <unknown>"
	ENDCASE

	." , R"	  DUP  0xf0000 AND 16 >> .-

	DROP 2+
;


: DSB_TP
  ,0x.

   OP_TYPE TAB
    
      DUP  0xF AND 
	CASE

     0x02 OF ." OSHST"	ENDOF
     0x03 OF ." OSH"	ENDOF

     0x04 OF ." UNST"	ENDOF
     0x05 OF ." UN"	ENDOF

     0x0a OF ." ISHST"	ENDOF
     0x0b OF ." ish"	ENDOF

     0x0e OF ." ST"	ENDOF
     0x0f OF ." SY"	ENDOF
	DUP ." #" .-

	ENDCASE
	DROP 2+
;

: LDRSH.W#_TP
  ,0x.

   OP_TYPE TAB
	." R"	  DUP  0x0f000 AND 12 >> .-
        ." , [R"  DUP  0xf0000 AND 16 >> .-

  DUP 0xFFF AND 0<>
	IF ." , #" DUP 0xFFF AND .-

	THEN	." ]"

	DROP 2+
;

: B_STR.W=
  ,0x.

   OP_TYPE TAB

	." R"	DUP 0xF000 AND 12 >> .-
        ." , [PC"  DUP 0xFFF AND 0<>
	IF ." , #"
		DUP 0x800000 AND 0= IF ." -" THEN
		DUP 0xFFF AND .-

	THEN	." ]"

	DROP 2+
;

: STR.W[_TP
  ,0x.

   OP_TYPE TAB

	." R"	 DUP 0x0F000 AND 12 >> .-  
        ." , [R" DUP 0xF0000 AND 16 >> .-  
        DUP 0xFFF AND 0<>
	IF ." , #" DUP 0xFFF AND .-

	THEN	." ]"

	DROP 2+
;


: STR.W#_TP
  ,0x.

   OP_TYPE TAB

	." R"		DUP 0x0F000 AND 12 >> .-  
        ." , [R"	DUP 0xF0000 AND 16 >> .-  
	." ], #"
		DUP 0x200 AND 0= IF ." -" THEN
		DUP 0xFF AND .-
	DROP 2+

;

: STRT_TP
  ,0x.

   OP_TYPE TAB

	." R"		DUP 0x0F000 AND 12 >> .-  
        ." , [R"	DUP 0xF0000 AND 16 >> .-  
	." , #"
		DUP 0xFF AND .-
	." ]"
	DROP 2+

;

: STR.W]_TP
  ,0x.

   OP_TYPE TAB

	." R"		DUP 0x0F000 AND 12 >> .-  
        ." , [R"	DUP 0xF0000 AND 16 >> .-  
	." , #"
		DUP 0x200  AND 0= IF ." -" THEN
		DUP 0xFF AND .-
	." ]" 	DUP 0x100 AND  IF ." !" THEN
	DROP 2+

;


: LDRD_TP
  ,0x.

   OP_TYPE TAB

	." R"		DUP 0x0F000 AND 12 >> .-  
	." , R"		DUP 0x00F00 AND  8 >> .-  
        ." , [R"	DUP 0xF0000 AND 16 >> .- 
	DUP 0xFF AND
	IF	." , #"
		DUP 0x800000  AND 0= IF ." -" THEN
		DUP 0xFF AND 2 << .-
	THEN
	." ]" 	DUP 0x200000 AND  IF ." !" THEN
	DROP 2+

;

: STR.WR_TP
  ,0x.

   OP_TYPE TAB

	." R"		DUP 0x0F000 AND 12 >> .-  
        ." , [R"	DUP 0xF0000 AND 16 >> .-  
	." , R"		DUP 0x0000F AND  .-  
			DUP 0x00030 AND ?DUP
		IF	." , lsl #" 4 >> .- THEN

	." ]"
	DROP 2+

;



: LDRD]_TP
  ,0x.

   OP_TYPE TAB

	." R"		DUP 0x0F000 AND 12 >> .-  
	." , R"		DUP 0x00F00 AND  8 >> .-  
        ." , [R"	DUP 0xF0000 AND 16 >> .- 
	." ], #"
		DUP 0x800000  AND 0= IF ." -" THEN
		DUP 0xFF AND 2 << .-
	DROP 2+

;

: SBFX_TP
  ,0x.

   OP_TYPE TAB

	." R"	DUP	0x00F00 AND  8 >> .-
	." , R"	DUP	0xF0000 AND 16 >> .-
	." , #" DUP	0x000C0 AND  6 >>
		OVER	0x07000 AND 10 >> OR .-
	." , #" DUP	0x0001F AND  1 + .-
	DROP 2+

;


: BFC_TP
  ,0x.

   OP_TYPE TAB

	." R"	DUP	0x00F00 AND  8 >> .-
	." , #" DUP	0x000C0 AND  6 >>
		OVER	0x07000 AND 10 >> OR DUP .-
	." , #" OVER 0x0001F AND SWAP - 1 + .-
	DROP 2+

;

: BFI_TP
  ,0x.

   OP_TYPE TAB

	." R"	DUP	0x00F00 AND  8 >> .-
	." , R"	DUP	0xF0000 AND 16 >> .-
	." , #" DUP	0x000C0 AND  6 >>
		OVER	0x07000 AND 10 >> OR DUP .-
	." , #" OVER 0x0001F AND SWAP - 1 + .-
	DROP 2+

;

: TBB_TP
  ,0x.
   OP_TYPE TAB
	." [R"	DUP	0xF0000 AND 16 >> .-
	." , R"	DUP	0x0000F AND .- ." ]"
	DROP 2+
;

: TBH_TP
  ,0x.
   OP_TYPE TAB
	." [R"	DUP	0xF0000 AND 16 >> .-
	." , R"	DUP	0x0000F AND .- 	." , lsl #1]"

	DROP 2+
;


: IT_TP
 TAB  OP_TYPE
\ DROP EXIT
   DUP 0x10 AND >R
   DUP 0xF AND DUP TO IT_V
  BEGIN 1 << 0xF AND DUP
  WHILE  DUP 0x10 AND R@ XOR
	 IF ." E" ELSE ." T" THEN
  REPEAT DROP RDROP
\ DROP EXIT
  DUP 0xF0 AND TO IT_N 
  TAB 0xf0 AND 3 >>
 S" EQNECSCCMIPLVSVCHILSGELTGTLEAL??" DROP + 2 TYPE

;

: CPSIE, 0xb660 |;
: CPSID, 0xb670 |;

: MCR,	0xf3ef8000 |;

: STMIA, 0xC000	|;
: LDMIA, 0xC800	|;

: ADDS,	0x1800 SSS |;
: SUBS,	0x1a00 SSS |;

: ADDS3	0x1c00 SSS |;
: SUBS3	0x1e00 SSS |;


: ADDS	0xb000	|;
: SUBS	0xb080	|;


: B.N,	0xE000	|;
: BX,	0x4700	|;
: BLX,	0x4780	|;


: POP,	0xBC00	|;
: PUSH,	0xB400	|;

: LSLS#	0x0000 SSS |;
: LSRS#	0x0800 SSS |;
: ASRS#	0x1000 SSS |;


: MOVS,	0x0000 SSS |;
: ANDS,	0x4000 SSS |;
: EORS,	0x4040 SSS |;
: LSLS,	0x4080 SSS |;
: LSRS,	0x40c0 SSS |;
: ASRS,	0x4100 SSS |;
: ADCS,	0x4140 SSS |;
: SBCS,	0x4180 SSS |;
: RORS,	0x41c0 SSS |;
: TST,	0x4200	|;
: NEGS,	0x4240 SSS |;
: CMP7	0x4280	|;
: CMN,	0x42c0	|;
: ORRS,	0x4300 SSS |;
: MULS,	0x4340 SSS |;
: BICS,	0x4380 SSS |;
: MVNS,	0x43c0 SSS |;

: SXTH,	0xb200	|;
: SXTB,	0xb240	|;
: UXTH,	0xb280	|;
: UXTB,	0xb2c0	|;


: BKPT,	0xbe00	|;
: SVC,	0xdf00	|;


: ADD,	0x4400	|;
: CMP,	0x4500	|;
: MOV,	0x4600	|;


: MOVS#	0x2000 SSS |;
: CMP#	0x2800  |;
: ADDS#	0x3000 SSS |;
: SUBS#	0x3800 SSS |;

: LDR=	0x4800	|;

: STRR  0x9000	|;
: LDRR  0x9800	|;

: STR,  0x5000	|;
: LDR,  0x5800	|;
: STRB, 0x5400	|;
: LDRB, 0x5c00	|;
: STRH, 0x5200	|;
: LDRH, 0x5a00	|;
: LDRSB, 0x5600	|;
: LDRSH, 0x5E00	|;


: STR#  0x6000	|;
: LDR#  0x6800	|;
: STRB# 0x7000	|;
: LDRB# 0x7800	|;
: STRH# 0x8000	|;
: LDRH# 0x8800	|;


: CBZ,	0xb100	|;
: CBNZ,	0xb900	|;


: BEQ.N, 0xd000 |;
: BNE.N, 0xd100 |;
: BCS.N, 0xd200 |;
: BCC.N, 0xd300 |;
: BMI.N, 0xd400 |;
: BPL.N, 0xd500 |;
: BVS.N, 0xd600 |;
: BVC.N, 0xd700 |;
: BHI.N, 0xd800 |;
: BLS.N, 0xd900 |;
: BGE.N, 0xda00 |;
: BLT.N, 0xdb00 |;
: BGT.N, 0xdc00 |;
: BLE.N, 0xdd00 |;

: ADD& 0xA000 |;

: NOP{		0xBf00 |;

: NOP,		0xBf00 |;
: YIELD,	0xBF10 |;
: WFE,		0xBF20 |;
: WFI,		0xBF30 |;
: SEV,		0xBF40 |;



: IT,   0xBf00  |;


: BEQ.W, 0xf0008000 |;
: BNE.W, 0xf0408000 |;
: BCS.W, 0xf0808000 |;
: BCC.W, 0xf0c08000 |;
: BMI.W, 0xf1008000 |;
: BPL.W, 0xf1408000 |;
: BVS.W, 0xf1808000 |;
: BVC.W, 0xf1c08000 |;
: BHI.W, 0xf2008000 |;
: BLS.W, 0xf2408000 |;
: BGE.W, 0xf2808000 |;
: BLT.W, 0xf2c08000 |;
: BGT.W, 0xf3008000 |;
: BLE.W, 0xf3408000 |;


: B.W,	0xf0009000 |;
: BLXL	0xf000C000 |;
: BL,	0xf000d000 |;

: NOP.W,	0xf3af8000 |;
: YIELD.W,	0xf3af8001 |;
: WFE.W,	0xf3af8002 |;
: WFI.W,	0xf3af8003 |;
: SEV.W,	0xf3af8004 |;

: clrex,	0xf3bf8f2f |;

: STRB.W=  0xf80f0000  |;
: LDRB.W=  0xf81f0000  |;
: STRH.W=  0xf82f0000  |;
: LDRH.W=  0xf83f0000  |;
: STR.W=   0xf84f0000  |;
: LDR.W=   0xf85f0000  |;
	
: STRB.W#  0xf8000900  |;
: LDRB.W#  0xf8100900  |;
: STRH.W#  0xf8200900  |;
: LDRH.W#  0xf8300900  |;
: STR.W#   0xf8400900  |;
: LDR.W#   0xf8500900  |;

: STRB.W]  0xf8000c00  |;
: LDRB.W]  0xf8100c00  |;
: STRH.W]  0xf8200c00  |;
: LDRH.W]  0xf8300c00  |;
: STR.W]   0xf8400c00  |;
: LDR.W]   0xf8500c00  |;

: STRB.WR  0xf8000000  |;
: LDRB.WR  0xf8100000  |;
: STRH.WR  0xf8200000  |;
: LDRH.WR  0xf8300000  |;
: STR.WR   0xf8400000  |;
: LDR.WR   0xf8500000  |;


: STRBT,  0xf8000e00  |;
: LDRBT,  0xf8100e00  |;
: STRHT,  0xf8200e00  |;
: LDRHT,  0xf8300e00  |;
: STRT,   0xf8400e00  |;
: LDRT,   0xf8500e00  |;

: STRB.W[  0xf8800000  |;
: LDRB.W[  0xf8900000  |;
: STRH.W[  0xf8A00000  |;
: LDRH.W[  0xf8B00000  |;
: STR.W[   0xf8C00000  |;
: LDR.W[   0xf8D00000  |;


: LSL.W,	0xfa00f000 |;
: lsls.w,	0xfa10f000 |;
: lsr.w,	0xfa20f000 |;
: lsrs.w,	0xfa30f000 |;
: asr.w,	0xfa40f000 |;
: asrs.w,	0xfa50f000 |;
: ror.w,	0xfa60f000 |;
: rors.w,	0xfa70f000 |;
: sadd8,	0xfa80f000 |;
: sadd16,	0xfa90f000 |;
: sasx,		0xfaa0f000 |;
: ssub8,	0xfac0f000 |;
: ssub16,	0xfad0f000 |;
: ssax,		0xfae0f000 |;

: mul.w,	0xfb00f000 |;
: SMULBB,	0xfb10f000 |;
: SMUAD,	0xfb20f000 |;
: SMULWB,	0xfb30f000 |;
: SMUSD,	0xfb40f000 |;
: SMMUL,	0xfb50f000 |;
: USAD8,	0xfb70f000 |;
: SDIV,		0xfb90f0f0 |;
: UDIV,		0xfbb0f0f0 |;

: MLA,		0xfb000000 |;
: MLS,		0xfb000010 |;

: SMULL,	0xfb800000 |;
: UMULL,	0xfba00000 |;
: SMLAL,	0xfbc00000 |;
: UMLAL,	0xfbe00000 |;

: PLI,		0xf990f000	|;
: LDRSB.W#	0xf9900000	|;
: LDRSH.W#	0xf9b00000	|;

: STMIA.W,	0xe8800000 |;
: LDMIA.W,	0xe8900000 |;
: STMDB,	0xe9000000 |;
: LDMDB,	0xe9100000 |;


: AND.W,	0xea000000 |;
: ANDS.W,	0xea100000 |;
: BIC.W,	0xea200000 |;
: BICS.W,	0xea300000 |;
: ORR.W,	0xea400000 |;
: ORRS.W,	0xea500000 |;
: ORN.W,	0xea600000 |;
: ORNS.W,	0xea700000 |;
: EOR.W,	0xea800000 |;
: EORS.W,	0xea900000 |;
: PKHBT		0xeac00000 |;
: ADD.W,	0xeb000000 |;
: ADDS.W,	0xeb100000 |;
: ADC.W,	0xeb400000 |;
: ADCS.W,	0xeb500000 |;
: SBC.W,	0xeb600000 |;
: SBCS.W,	0xeb700000 |;
: SUB.W,	0xeba00000 |;
: SUBS.W,	0xebb00000 |;
: RSB,		0xebc00000 |;
: RSBS,		0xebd00000 |;

: AND.W#	0xf0000000 |;
: ANDS.W#	0xf0100000 |;
: BIC.W#	0xf0200000 |;
: BICS.W#	0xf0300000 |;
: ORR.W#	0xf0400000 |;
: ORRS.W#	0xf0500000 |;
: ORN#		0xf0600000 |;
: ORNS#		0xf0700000 |;
: EOR.W#	0xf0800000 |;
: EORS.W#	0xf0900000 |;
: ADD.W#	0xf1000000 |;
: ADDS.W#	0xf1100000 |;
: ADC.W#	0xf1400000 |;
: ADCS.W#	0xf1500000 |;
: SBC.W#	0xf1600000 |;
: SBCS.W#	0xf1700000 |;
: SUB.W#	0xf1a00000 |;
: SUBS.W#	0xf1b00000 |;
: RSB#		0xf1c00000 |;
: RSBS#		0xf1d00000 |;

: ADDW,		0xf2000000 |;
: SUBW,		0xf2a00000 |;


: MOVW,		0xf2400000 |;
: MOVT,		0xf2c00000 |;

: MOV.W#	0xf04f0000 |; 	
: MOVS.W#	0xf05f0000 |; 	

: MVN.W#	0xf06f0000 |; 	
: MVNS.W#	0xf07f0000 |;

: MOV.W,	0xea4f0000 |; 	
: MOVS.W,	0xea5f0000 |; 	

: MVN.W,	0xea6f0000 |; 	
: MVNS.W,	0xea7f0000 |;

: TST.W,	0xea100f00 |;
: TEQ,		0xea900f00 |;
: CMN.W,	0xeb100f00 |;
: CMP.W,	0xebb00f00 |;

: TST.W#	0xf0100f00 |;
: TEQ#		0xf0900f00 |;
: CMN.W#	0xf1100f00 |;
: CMP.W#	0xf1b00f00 |;

: STRD]		0xe8600000 |;
: LDRD]		0xe8700000 |;

: STRD,		0xe9400000 |;
: LDRD,		0xe9500000 |;

: SXTH.W,	0xfa0ff080 |;
: UXTH.W,	0xfa1ff080 |;
: SXTB16,	0xfa2ff080 |;
: UXTB16,	0xfa3ff080 |;
: SXTB.W,	0xfa4ff080 |;
: UXTB.W,	0xfa5ff080 |;

: sxtah,	0xfa00f080 |;
: uxtah,	0xfa10f080 |;
: sxtab16,	0xfa20f080 |;
: uxtab16,	0xfa30f080 |;
: sxtab,	0xfa40f080 |;
: uxtab,	0xfa50f080 |;

: REV.W,	0xfa90f080 |;
: CLZ,		0xfab0f080 |;
: REV16.W,	0xfa90f090 |;
: RBIT,		0xfa90f0a0 |;
: REVSH.W,	0xfa90f0b0 |;



: BFI,		0xf3600000 |;
: BFC,		0xf36f0000 |;

: SBFX,		0xf3400000 |;
: UBFX,		0xf3c00000 |;


: TBB,		0xe8d0f000 |;
: TBH,		0xe8d0f010 |;
: mrs,		0xf3ef8000 |;
: msr,		0xf3808800 |;

: DSB,		0xf3bf8f40 |;
: DMB,		0xf3bf8f50 |;
: ISB,		0xf3bf8f60 |;

: LDREX,	0xe8500f00 |;

: LDREXB,	0xe8d00f4f |;
: LDREXH,	0xe8d00f5f |;

: STREX,	0xe8400000 |;
: STREXB,	0xe8c00f40 |;
: STREXH,	0xe8c00f50 |;

: USAT,		0xf3800000 |;
: SSAT,		0xf3000000 |;

: SSAT16,	0xf3200000 |;
: USAT16,	0xf3a00000 |;

: VMOV,		0xee000a10 |;
: VMOVR		0xee100a10 |;

: VMOV.F32,	 0xeeb00a40 |;
: VABS.F32,	 0xeeb00aC0 |;
: VNEG.F32,	 0xeeb10a40 |;
: VSQRT.F32,	 0xeeb10aC0 |;
: VCVTB.F32.F16, 0xeeb20a40 |;
: VCVTT.F32.F16, 0xeeb20ac0 |;
: VCVTB.F16.F32, 0xeeb30a40 |;
: VCVTT.F16.F32, 0xeeb30ac0 |;
: VCMP.F32,	 0xeeb40a40 |;
: VCMPE.F32,	 0xeeb40ac0 |;
: VCVT.F32.S32,  0xeeb80ac0 |;
: VCVT.F32.U32,  0xeeb80a40 |;
: VCVT.U32.F32,  0xeebc0ac0 |;
: VCVTR.U32.F32, 0xeebc0a40 |;
: VCVTR.S32.F32, 0xeebd0a40 |;
: VCVT.S32.F32,  0xeebd0ac0 |;


: VMLA.F32,	0xee000a00 |;
: VMLS.F32,	0xee000a40 |;
: VNMLA.F32,	0xee100a40 |;
: VNMLS.F32,	0xee100a00 |;
: VMUL.F32,	0xee200a00 |;
: VNMUL.F32,	0xee200a40 |;
: VADD.F32,	0xee300a00 |;
: VSUB.F32,	0xee300a40 |;
: VDIV.F32,	0xee800a00 |;
: VFMNS.F32,	0xee900a00 |;
: VFMNA.F32,	0xee900a40 |;
: VFMA.F32,	0xeea00a00 |;
: VFMS.F32,	0xeea00a40 |;

: VLDMIA,	0xec900a00 |;
: VLDMDB,	0xed300a00 |;

: VMRS,		0xeef10a10 |;

EXPORT


: MINST_ ( [INST] [INST] -- [INST'] )
  W@
  DUP TO OPCODE

    ['] NOP_TP TO OP.	NOP, YIELD, WFE, WFI, SEV,


  DUP 0xFFF8 AND TO OPCODE

    ['] cpsie_TP TO OP.	 CPSIE, CPSID,

  DUP 0xFF0F AND TO OPCODE
     ['] NOP{_TP TO OP.	NOP{

  DUP 0xFFC0 AND TO OPCODE

  ['] B_TST TO OP.
	ANDS, EORS, LSLS, LSRS, ASRS,
	ADCS, SBCS, RORS, TST, NEGS,
	CMP7  CMN,  ORRS, MULS, BICS, MVNS, MOVS,
	SXTH, SXTB, UXTH, UXTB,


  DUP 0xFF80 AND TO OPCODE

  ['] B_ADD_SP	TO OP.      ADDS SUBS

  DUP 0xFF00 AND TO OPCODE
  ['] IT_TP TO OP.	IT,
  ['] B_MOV TO OP.	MOV, ADD, CMP,
  ['] svc_tp TO OP.	SVC,
  ['] bkpt_tp TO OP.	bkpt,

  DUP 0xFF87 AND TO OPCODE
  ['] BX_TP	TO OP.	BX,  BLX,


  DUP 0xFE00 AND TO OPCODE

  ['] B_POP	TO OP.    POP, PUSH,
  ['] ADDS_TP	TO OP.   ADDS, SUBS,
  ['] ADDS#_TP	TO OP.   ADDS3 SUBS3

  DUP 0xFD00 AND TO OPCODE

  ['] CBZ_TP TO OP.   CBZ, CBNZ,

  DUP 0xFF00 AND TO OPCODE

  ['] bne.n_tp	TO OP.

	BEQ.N, BNE.N, BCS.N, BCC.N,
	BMI.N, BPL.N, BVS.N, BVC.N,
	BHI.N, BLS.N, BGE.N, BLT.N,
	BGT.N, BLE.N,

  DUP 0xFE00 AND TO OPCODE

  ['] B_LDR,	TO OP.	STR, LDR, STRB,  LDRB, STRH, LDRH, LDRSB, LDRSH,

  DUP 0xF800 AND TO OPCODE

  ['] B_B.N	TO OP.	B.N,
  ['] STMIA_TP	TO OP.	STMIA,
  ['] LDMIA_TP	TO OP.	LDMIA,
  ['] B_MOVS#	TO OP.	MOVS# CMP# ADDS# SUBS#
  ['] B_LDR#	TO OP.	4 TO QSH STR# LDR# 6 TO QSH STRB# LDRB# 5 TO QSH STRH# LDRH#
  ['] ldr=_tp	TO OP.	LDR=
  ['] B_LDRR	TO OP.	STRR LDRR
  ['] LSLS_TP	TO OP.	LSLS# LSRS# ASRS#

  DUP 0xF000 AND TO OPCODE

  ['] ADD_R_SP_TP TO OP. ADD&


 16 << OVER 2+ W@ OR

  DUP TO OPCODE

  ['] NOP.W_TP TO OP.  NOP.W, YIELD.W, WFE.W, WFI.W, SEV.W, clrex,


  DUP 0xfffffFF0 AND TO OPCODE

  ['] DSB_TP TO OP.  DSB, DMB, ISB,

  DUP 0xffff0fFF AND TO OPCODE

  ['] R,fpscr_TP TO OP. VMRS,

  DUP 0xfff0fff0 AND TO OPCODE
   ['] TBB_TP TO OP.	tbb,
   ['] TBH_TP TO OP.	tbh,

  DUP 0xfff00fff AND TO OPCODE

   ['] LDREXB_TP TO OP.	LDREXB, LDREXH,


  DUP 0xfffff0C0 AND TO OPCODE

   ['] SXTH.W_TP TO OP. SXTH.W, UXTH.W, SXTB16, UXTB16, SXTB.W, UXTB.W,

  DUP 0xfff0f0C0 AND TO OPCODE

   ['] SXTAH.W_TP TO OP. sxtah, uxtah, sxtab16, uxtab16, sxtab, uxtab,

  DUP 0xfff0ff00 AND TO OPCODE
   ['] msr_TP TO OP.	msr,

  DUP 0xfffff000 AND TO OPCODE
   ['] mrs_TP TO OP.	mrs,

  DUP 0xfff00ff0 AND TO OPCODE

   ['] strexb_TP TO OP.	STREXB, STREXH,

  DUP 0xfff0f0f0 AND TO OPCODE

  ['] SDIV_TP TO OP.	SDIV, UDIV,
	LSL.W,	LSLS.W,	LSR.W,	LSRS.W,	
	ASR.W,	ASRS.W,	ROR.W,	RORS.W,	
	SADD8,	SADD16,	SASX,	SSUB8,	SSUB16,	SSAX,		
	MUL.W,	SMULBB,	SMUAD,	SMULWB,	SMUSD,	SMMUL,	USAD8,	

  DUP 0xFFFF8020 AND TO OPCODE

  [']  BFC_TP  TO OP.	BFC,

  DUP 0xFFF00F7F AND TO OPCODE
  [']  S,R_TP  TO OP.	VMOV,
  [']  R,S_TP  TO OP.	VMOVR



  DUP 0xFFBF0FD0 AND TO OPCODE
  [']  S,S_TP  TO OP.
	VMOV.F32,	VABS.F32,	VNEG.F32,	VSQRT.F32,	VCVTB.F32.F16,
	VCVTT.F32.F16, VCVTB.F16.F32, VCVTT.F16.F32, VCMP.F32, VCMPE.F32, VCVT.F32.S32,
	VCVT.F32.U32,  VCVT.U32.F32,  VCVTR.U32.F32, VCVTR.S32.F32, VCVT.S32.F32,


  DUP 0xFFF0F0F0 AND TO OPCODE

  [']  REV.W_TP  TO OP.	REV.W,	CLZ, REV16.W, RBIT, REVSH.W,	

  DUP 0xfff00f00 AND TO OPCODE

   ['] LDREX_TP TO OP.	LDREX,

  DUP 0xFFF0F000 AND TO OPCODE

  ['] PLI_TP TO OP.	PLI,


  DUP 0xFFF000F0 AND TO OPCODE

  ['] MLS_TP	TO OP.	MLA, MLS,
  ['] SMULL_TP	TO OP.	SMULL, UMULL, SMLAL, UMLAL,

  DUP 0xFFF00F00 AND TO OPCODE

  ['] CMP.W_TP TO OP.	TST.W, TEQ, CMN.W, CMP.W,

  DUP 0xFFFF0000 AND TO OPCODE

  ['] MOV.W_TP TO OP.	MOV.W, MOVS.W, MVN.W, MVNS.W,


  DUP 0xFBF08F00 AND TO OPCODE

  ['] CMP.W#_TP TO OP.	TST.W# TEQ# CMN.W# CMP.W#

  DUP 0xFFB00F50 AND TO OPCODE
  [']  S,S,S_TP  TO OP.
	VMLA.F32,	VMLS.F32,	VNMLA.F32,	VNMLS.F32,	VMUL.F32,	
	VNMUL.F32,	VADD.F32,	VSUB.F32,	VDIV.F32,	VFMNS.F32,	
	VFMNA.F32,	VFMA.F32,	VFMS.F32,	

  DUP 0xFF900F00 AND TO OPCODE
  [']  R,{S}_TP  TO OP. VLDMIA, VLDMDB,

  DUP 0xFBF08000 AND TO OPCODE

  ['] MOVW,_TP  TO OP.	MOVW,	MOVT,

  DUP 0xFBFF8000 AND TO OPCODE

  ['] MVN.W#_TP  TO OP.	MVN.W#	MVNS.W#	MOV.W#	MOVS.W#

  DUP 0xFF7F0000 AND TO OPCODE

  ['] B_STR.W=  TO OP.
	STRB.W= LDRB.W=
	STRH.W= LDRH.W=
	STR.W=  LDR.W=


  DUP 0xFFF00FC0 AND TO OPCODE

  ['] STR.WR_TP  TO OP.
	STRB.WR LDRB.WR
	STRH.WR LDRH.WR
	STR.WR  LDR.WR

  DUP 0xFFF00F00 AND TO OPCODE

  ['] STRT_TP  TO OP. STRBT, LDRBT, STRHT, LDRHT, STRT,  LDRT,

  DUP 0xFFF00D00 AND TO OPCODE

  ['] STR.W#_TP  TO OP.
	STRB.W# LDRB.W#
	STRH.W# LDRH.W#
	STR.W#  LDR.W#

  DUP 0xFFF00c00 AND TO OPCODE

  ['] STR.W]_TP  TO OP.
	STRB.W] LDRB.W]
	STRH.W] LDRH.W]
	STR.W]  LDR.W]



  DUP 0xFFF08020 AND TO OPCODE

  [']  BFI_TP  TO OP.	BFI,
  ['] SBFX_TP  TO OP.	SBFX, UBFX,

  DUP 0xFFF00800 AND TO OPCODE

  ['] LDRSH.W#_TP TO OP.	LDRSH.W# LDRSB.W#

  DUP 0xFFF00000 AND TO OPCODE
  ['] STR.W[_TP  TO OP.
	STRB.W[ LDRB.W[
	STRH.W[ LDRH.W[
	STR.W[  LDR.W[

  DUP 0xFFF08000 AND TO OPCODE

  ['] ADD.W,_TP TO OP.

	AND.W,	ANDS.W,	BIC.W,	BICS.W,	
	ORR.W,	ORRS.W,	ORN.W,	ORNS.W,	
	EOR.W,	EORS.W,	ADD.W,	ADDS.W,	
	ADC.W,	ADCS.W,	SBC.W,	SBCS.W,	
	SUB.W,	SUBS.W,	RSB,	RSBS,

  DUP 0xFFF00000 AND TO OPCODE

   ['] strex_TP TO OP.	STREX,

  DUP 0xFFD08020 AND TO OPCODE

  ['] usat_TP TO OP.  USAT, SSAT, USAT16, SSAT16,

  DUP 0xFBF08000 AND TO OPCODE

  ['] ADD.W#_TP TO OP.

	AND.W#	ANDS.W#	BIC.W#	BICS.W#	
	ORR.W#	ORRS.W#	ORN#	ORNS#	
	EOR.W#	EORS.W#	ADD.W#	ADDS.W#	
	ADC.W#	ADCS.W#	SBC.W#	SBCS.W#	
	SUB.W#	SUBS.W#	RSB#	RSBS#

  ['] ADDW,_TP TO OP.	ADDW, SUBW,


  DUP 0xFFD00000 AND TO OPCODE

  ['] LDMIA.W_TP  TO OP.  LDMIA.W,  STMIA.W,  LDMDB,  STMDB,

  DUP 0xFF700000 AND TO OPCODE
  ['] LDRD]_TP  TO OP.  LDRD] STRD]

  DUP 0xFF500000 AND TO OPCODE
  ['] LDRD_TP  TO OP.  LDRD, STRD,


  DUP 0xFBC0D000 AND TO OPCODE

  ['] BEQ.W_TP  TO OP.
 BEQ.W, BNE.W, BCS.W, BCC.W, BMI.W, BPL.W, BVS.W, BVC.W,
 BHI.W, BLS.W, BGE.W, BLT.W, BGT.W, BLE.W,

  DUP 0xF800D000 AND TO OPCODE

  ['] B_BL TO OP.  BL, BLXL B.W,

 ."  @ ?????"
  DROP

 ;

: H.H C@ 2 H.N SPACE ;

: AMINST ( [INST] -- [INST+2] )
   1  TO VHS?
  1 INVERT AND
  BASE M@ >R DECIMAL
  DUP >R

  R@ ADDR_OFF + 8 H.R TAB
 
	R@	 W@ 4 H.N
\	R@	 H.H	R@ 1+	H.H

  R@  MINST_

  RDROP

 2+
  R> BASE M!
 ;

: AAMINST ( [INST] -- [INST+2] )

  1 INVERT AND
  BASE M@ >R DECIMAL
  DUP >R


	DUP
	1 OR NEAR_NFA 
	>R DUP
	IF   DUP NAME>  R> - 0=
	     IF  CR TAB
		   DUP ." .global " MCOUNT GTYPE 
		   CR DUP MCOUNT GTYPE ." :"
	     THEN DROP
	ELSE RDROP DROP
	THEN 

\   R@ ADDR_OFF + 8 H.R TAB

 DUP @ 0x7ffff6cf =
 IF  4 + DUP @ DUP 1 AND CR TAB 
	IF      DUP	." MOVW R6, #:lower16:"  ?.NAME>SS  ." +1"
	ELSE 1+ DUP	." MOVW R6, #:lower16:"  ?.NAME>SS
	THEN	CR TAB	." MOVT R6, #:upper16:"  ?.NAME>SS

    RDROP
  4 +
   R> BASE M!

 BREAK


 HS? DUP TO VHS?
 IF
   TAB ." .inst.n" TAB
 
	R@	 W@ 4  ." 0x" H.N
\	R@	 H.H	R@ 1+	H.H
 THEN
  R@  MINST_

  RDROP

 2+
  R> BASE M!
 ;

: DISA
 BEGIN CR
 AMINST
 KEY 0x20 OR 'q' =
 UNTIL
;

;MODULE


\eof

