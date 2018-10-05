\ Assembler for Cortex

\ REQUIRE HERE-TAB-CUR _mak\lib\THERE\mlist.f 
REQUIRE REQUIRE_AL _mak\lib\THERE\mlist.f 
REQUIRE [IF] _mak/CompIF1.f
REQUIRE [IFNDEF] ~nn/lib/ifdef.f
REQUIRE NUMBER? _mak/lib/fpcnum.f
REQUIRE [AGAIN] _mak/lib/twopass.f 


[IFNDEF] <<	: << LSHIFT ;	[THEN]
[IFNDEF] >>	: >> RSHIFT ;	[THEN]
[IFNDEF] BREAK	: BREAK  POSTPONE EXIT POSTPONE THEN ; IMMEDIATE [THEN]
[IFNDEF] CELL/	: CELL/  CELL / ;	[THEN]
[IFNDEF] OR!	:  OR! ( N ADDR -- )	DUP @ ROT OR	SWAP ! ; [THEN]
[IFNDEF] AND!	: AND! ( N ADDR -- )	DUP @ ROT AND	SWAP ! ; [THEN]
[IFNDEF] USHORT? : USHORT? ( n -- -129<n<256 )  0x80 +  0x180 U< ; [THEN]
[IFNDEF] NOT	: NOT INVERT ; [THEN]
[IFNDEF] FF_ALIGN
: FF_ALIGN
	HERE 1 AND 
	IF 0xFF C,
	THEN ;
[THEN]

[IFNDEF] .S  : .S ( -- )    DEPTH .SN ;  [THEN]

[IFNDEF] 3DUP : 3DUP DUP 2OVER ROT ;  [THEN]
\- 0> : 0> NEGATE 0< ;

0 VALUE ASMDBG

: NEXT
  0x4770 W, \  THUMB2_MOD::LR  THUMB2_MOD::BX,
 ;


MODULE: THUMB2_MOD

0 VALUE MOREPASS
0 VALUE NPASS

0 VALUE ITERNUM

0xF VALUE DSB_OPT

: OSHST    0x02  TO  DSB_OPT ;
: OSH	   0x03  TO  DSB_OPT ;
: UNST     0x04  TO  DSB_OPT ;
: UN	   0x05  TO  DSB_OPT ;
: ISHST    0x0a  TO  DSB_OPT ;
: ISH	   0x0b  TO  DSB_OPT ;
: ST	   0x0e  TO  DSB_OPT ;
: SY	   0x0f  TO  DSB_OPT ;

: LTABLE@      ( --- addr )
   CREATE HERE 0 , 0 , DOES>
 DUP CELL+ @ 1 NOT AND
 ITERNUM  <>
 IF ITERNUM
 OVER CELL+ !
 THEN  @
;

: LTABLE!      ( addr --- )
   CREATE ,   DOES> @
 DUP  CELL+ @ ITERNUM 1+ = ABORT" already defined"
 DUP  CELL+ @ ITERNUM =
  IF   DUP @ HERE <> IF -1 TO MOREPASS THEN
	ITERNUM 1+ OVER CELL+ !
  THEN   HERE SWAP ! ;

: ::L  LTABLE@  LTABLE!  ;

::L  LL0 LL0:
::L  LL1 LL1:
::L  LL2 LL2:
::L  LL3 LL3:
::L  LL4 LL4:
::L  LL5 LL5:
::L  LL6 LL6:
::L  LL7 LL7:
::L  LL8 LL8:
::L  LL9 LL9:
::L  LLA LLA:
::L  LLB LLB:
::L  LLC LLC:
::L  LLD LLD:
::L  LLE LLE:
::L  LLF LLF:

0 VALUE VSP@
0 VALUE []
0 VALUE ?##
0 VALUE BBF
0 VALUE HHF
0 VALUE !!F
0 VALUE SHF
0 VALUE ?{{

0 VALUE {{}}

0 VALUE COMCOD

: !!   \ writ-back bit
   -1 TO !!F ;


0 VALUE PARM_HESH

: >PARM_HESH
  PARM_HESH 2 << + TO PARM_HESH
;

: RX 1 >PARM_HESH ;

: R: CREATE DUP   , 1+ DOES> @
 ?{{
 IF  1 SWAP <<  {{}} OR TO {{}}
 ELSE  RX
 THEN ;

: -R: CREATE DUP NEGATE 1 SWAP <<  , 1+ DOES> @
   BEGIN DUP {{}} NOT AND
   WHILE DUP {{}} OR TO {{}} 1 >>  0xFFFF AND
   REPEAT DROP
 ;

: [[ 2 >PARM_HESH ;

: ]] 3 >PARM_HESH ;

: ## 4 >PARM_HESH ;

0 VALUE TSHIFT

: LSL$ 0x00 TO TSHIFT 5 >PARM_HESH ;
: LSR$ 0x10 TO TSHIFT 5 >PARM_HESH ;
: ASR$ 0x20 TO TSHIFT 5 >PARM_HESH ;
: ROR$ 0x30 TO TSHIFT 5 >PARM_HESH ;

: {{ 6 >PARM_HESH -1 TO ?{{ 0 TO {{}} ;		: }}  0 TO ?{{ ;

0 VALUE :=ADR
0 VALUE :=COUNT
22 CELLS CONSTANT :=MAX
CREATE :=TAB :=MAX CELL+ ALLOT

: := ( -- adr )
  :=MAX :=COUNT U< ABORT" excess ="
 7 >PARM_HESH    :=ADR :=COUNT +  ;

0 R: R0  R: R1  R: R2  R: R3
  R: R4  R: R5  R: R6  R: R7
  R: R8  R: R9
DUP
  R: R10 R: R11  R: R12 R: R13 R: R14 R: R15 DROP
  R:  SL R: FP   R: IP  R: SP  R: LR  R: PC  DROP

-15
  -R: -R15 -R: -R14 -R: -R13 -R: -R12
  -R: -R11 -R: -R10 -R: -R9  -R: -R8
  -R: -R7  -R: -R6  -R: -R5  -R: -R4
  -R: -R3  -R: -R2  -R: -R1
DROP

0 R: CR0   R: CR1  R: CR2  R: CR3
  R: CR4   R: CR5  R: CR6  R: CR7
  R: CR8   R: CR9  R: CR10 R: CR11
  R: CR12  R: CR13 R: CR14 R: CR15 DROP


\ immediate and status register bit
0 VALUE #I
0 VALUE SR

\ set the status update bit
: S!  1 20 <<   TO SR ;
: S@ SR OR 0 TO SR ;


: PARAM:
 CREATE PARM_HESH ,  0 TO PARM_HESH
 DOES>  @ PARM_HESH =
 ;

 ##			PARAM: #N
 RX			PARAM: R,
 RX RX			PARAM: R,R
 RX ##			PARAM: R,#
 RX RX RX		PARAM: R,R,R
 RX RX ##		PARAM: R,R,#
 RX ## RX		PARAM: R#R
 RX ## RX lsl$ ##	PARAM: R#RS
 RX [[ RX ]]		PARAM: R[R]
 RX RX [[ RX ]]		PARAM: RR[R]
 RX [[ RX RX ]] 	PARAM: R[RR]
 RX [[ RX ## ]] 	PARAM: R[R#]
 RX RX [[ RX ## ]] 	PARAM: RR[R#]
 RX [[ RX ]] ## 	PARAM: R[R]#
 RX {{ }}		PARAM: R{
 RX [[ RX RX lsl$ ## ]] PARAM: R[RRS]
 RX RX lsl$ ##		PARAM: R,R,S
 RX RX RX lsl$ ##	PARAM: RRRS
 RX :=	DROP		PARAM: R,=


\ rotate a bits left

 : ROTATE DUP 30 >> SWAP 2 << + ;

\ set and use the immediate bit for an opcode
: #! 1 25 <<  TO #I ;
: #@ #I + 0 TO #I ;

\ test if an immediate value is 8-bit rotatable

: U8-BIT? ( n -- n flag )
  DUP >R 0 BEGIN OVER USHORT? 0= \  0xFF U>
 WHILE
  SWAP ROTATE SWAP 1+ DUP 15 >
  IF 2DROP R> 0 BREAK
  REPEAT 2DROP R> TRUE ;

: 8-BIT? ( n -- n flag )
  DUP >R 0 BEGIN OVER 0xFF U>
 WHILE
  SWAP ROTATE SWAP 1+ DUP 15 >
  IF 2DROP R> 0 BREAK
  REPEAT 2DROP R> TRUE ;

0 VALUE IT_V

: ITSTEP   IT_V 1 << TO IT_V ;

: DO|;  ( cod -- )
  OR  W,
 0 TO PARM_HESH
 0 TO !!F
 ITSTEP ;

' DO|;
DUP 1- @ $1000E92D = [IF] $B + [THEN]
CONSTANT B'DO|; 

: |;
 POSTPONE  DO|;
 POSTPONE ;
; IMMEDIATE


: B.N, ( addr )
  HERE 4 + - 2/ 0x7FF AND 0xE000 |;

: BXX.N HERE 4 + - 2/
  DUP $80 + $FF ANDC \ IF -314 THROW THEN
  IF NPASS ABORT"  branch out of range"
	-1 TO MOREPASS
  THEN
  $FF AND ;

: BEQ.N, BXX.N 0xd000 |;
: BNE.N, BXX.N 0xd100 |;

: BCS.N, BXX.N 0xd200 |;
: BCC.N, BXX.N 0xd300 |;

: BMI.N, BXX.N 0xd400 |;
: BPL.N, BXX.N 0xd500 |;

: BVS.N, BXX.N 0xd600 |;
: BVC.N, BXX.N 0xd700 |;

: BHI.N, BXX.N 0xd800 |;
: BLS.N, BXX.N 0xd900 |;

: BGE.N, BXX.N 0xda00 |;
: BLT.N, BXX.N 0xdb00 |;

: BGT.N, BXX.N 0xdc00 |;
: BLE.N, BXX.N 0xdd00 |;

: CBXZ    HERE 4 + -
  DUP 0x7F NOT 1+ AND
 IF NPASS ABORT"  branch out of range"
	-1 TO MOREPASS
 THEN
 2/  DUP 0x1F AND 3 <<  SWAP 0x20 AND 4 << OR OR ;
: CBZ,  cbxz 0xb100 |;
: CBNZ, cbxz 0xb900 |;

: B.W# ( label -- cod )
 HERE 4 +
 -
  U2/
  DUP 0x800000 AND   0=
   IF 0x600000 XOR THEN
\                   0x1fffff
  DUP    0x7FF AND
 OVER 0x1FF800 AND  5 <<  OR
 OVER 0x200000 AND 10 >>  OR
 OVER 0x400000 AND  9 >>  OR
 SWAP 0x800000 AND  3 <<  OR
    0xf0009000 OR
;


: BXX.W, ( label -- cod )
 SWAP     HERE 4 + - 2/

  DUP $400 + $7FF ANDC \ IF -314 THROW THEN 
  IF NPASS ABORT"  branch out of range"
	-1 TO MOREPASS
  THEN

  DUP    0x7FF AND
 OVER 0x01F800 AND  5 <<  OR
 OVER 0x020000 AND  6 >>  OR
 OVER 0x040000 AND  5 >>  OR
 SWAP 0x080000 AND  7 <<  OR
    OR
	DUP 16 >>  W, 0 |;

: BEQ.W, 0xf0008000 BXX.W, ;
: BNE.W, 0xf0408000 BXX.W, ;
: BCS.W, 0xf0808000 BXX.W, ;
: BCC.W, 0xf0c08000 BXX.W, ;
: BMI.W, 0xf1008000 BXX.W, ;
: BPL.W, 0xf1408000 BXX.W, ;
: BVS.W, 0xf1808000 BXX.W, ;
: BVC.W, 0xf1c08000 BXX.W, ;
: BHI.W, 0xf2008000 BXX.W, ;
: BLS.W, 0xf2408000 BXX.W, ;
: BGE.W, 0xf2808000 BXX.W, ;
: BLT.W, 0xf2c08000 BXX.W, ;
: BGT.W, 0xf3008000 BXX.W, ;
: BLE.W, 0xf3408000 BXX.W, ;

: FAR? DUP HERE 4 + - 2/  $80 + $FF ANDC ; \ ABS 0x7F NOT AND ;

: BEQ, FAR? IF BEQ.W, ELSE BEQ.N, THEN  ;
: BNE, FAR? IF BNE.W, ELSE BNE.N, THEN  ;
: BCS, FAR? IF BCS.W, ELSE BCS.N, THEN  ;
: BCC, FAR? IF BCC.W, ELSE BCC.N, THEN  ;
: BMI, FAR? IF BMI.W, ELSE BMI.N, THEN  ;
: BPL, FAR? IF BPL.W, ELSE BPL.N, THEN  ;
: BVS, FAR? IF BVS.W, ELSE BVS.N, THEN  ;
: BVC, FAR? IF BVC.W, ELSE BVC.N, THEN  ;
: BHI, FAR? IF BHI.W, ELSE BHI.N, THEN  ;
: BLS, FAR? IF BLS.W, ELSE BLS.N, THEN  ;
: BGE, FAR? IF BGE.W, ELSE BGE.N, THEN  ;
: BLT, FAR? IF BLT.W, ELSE BLT.N, THEN  ;
: BGT, FAR? IF BGT.W, ELSE BGT.N, THEN  ;
: BLE, FAR? IF BLE.W, ELSE BLE.N, THEN  ;


: B.W,  B.W#		DUP 16 >>  W, W, ITSTEP ;
: BL,   B.W# 0x4000 OR  DUP 16 >>  W, W, ITSTEP ;
: B,	DUP HERE 4 + - 0x800 +  0xFFFFF000 AND IF B.W, BREAK B.N, ;

: BX,	3 << 0x4700 |;

: BLX,
	PARM_HESH 1 = IF 3 << 0x4780 DO|; BREAK
      B.W# 0x5000 XOR  DUP 16 >>  W, W, ITSTEP
;

\ load and store halfword address class

: HH  0x040000B0 TO HHF ;
: BB  0x0400000 TO BBF ;

: NOP,		0xBF00 PARM_HESH 6 = DUP IF DROP SWAP 4 << THEN  |;
: YIELD,	0xBF10 W, ITSTEP ;
: WFE,		0xBF20 W, ITSTEP ;
: WFI,		0xBF30 W, ITSTEP ;
: SEV,		0xBF40 W, ITSTEP ;

: NOP.W,	0x8000f3af , ITSTEP ;
: YIELD.W,	0x8001f3af , ITSTEP ;
: WFE.W,	0x8002f3af , ITSTEP ;
: WFI.W,	0x8003f3af , ITSTEP ;
: SEV.W,	0x8004f3af , ITSTEP ;

: clrex,	0xf3bf8f2f , ITSTEP ;


: REV.W,	DUP 0xfa90 OR W, SWAP 8 << 0xf080 OR |;
: CLZ.W,	DUP 0xfab0 OR W, SWAP 8 << 0xf080 OR |;
: REV16.W,	DUP 0xfa90 OR W, SWAP 8 << 0xf090 OR |;
: RBIT.W,	DUP 0xfa90 OR W, SWAP 8 << 0xf0a0 OR |;
: REVSH.W,	DUP 0xfa90 OR W, SWAP 8 << 0xf0b0 OR |;


: R7R7	2DUP OR  0x7 NOT AND 0= R,R AND ;

: |R[R#]? ( r7 r7 ## -- flg )
   NOT AND >R OR 7 NOT AND R> OR 0= ;


: |bR[R#]	R[R#] DUP IF DROP 3DUP 0x1F |R[R#]? THEN ;
: |hR[R#]	R[R#] DUP IF DROP 3DUP 0x3E |R[R#]? THEN ;
: |R[R#]	R[R#] DUP IF DROP 3DUP 0x7C |R[R#]? THEN ;
: |R[R]		R[R]  DUP IF DROP 2DUP 0 0  |R[R#]? THEN ;

: |R[RR]	R[RR] DUP IF DROP 3DUP OR OR 7 NOT AND 0= THEN ;

: |R[SP#]? ( r7 r7 ## -- flg )
   NOT AND >R 13 <> >r 7 NOT AND 2R> OR OR 0= ;

: |R[SP#]	R[R#] DUP IF DROP 3DUP 0x3FC |R[SP#]? THEN ;

: |R[PC#]? ( r7 r7 ## -- flg )
   NOT AND >R 15 <> >r 7 NOT AND 2R> OR OR 0= ;

: |R[PC#]	R[R#] DUP IF DROP 3DUP 0x3FC |R[PC#]? THEN ;

: |R,R,R	R,R,R DUP IF DROP 3DUP OR OR  7 NOT AND 0= THEN ;
: |R,R,0#	R,R,# DUP IF DROP DUP 2OVER OR 7 NOT AND OR 0= THEN ;
: |R,R,#	R,R,# DUP IF DROP 3DUP 0x7	|R[R#]? THEN ;
: >R,R,#	R,R,# DUP IF DROP 3DUP 1- 0x1F	|R[R#]? THEN ;
: |R,#		R,#   DUP IF DROP
				OVER	7 NOT AND
				OVER 0xFF NOT AND  OR  0= THEN ;

: CLZ,  CLZ.W, ;
: RBIT, RBIT.W, ;

: REV,   r7r7 IF 3 << OR 0xBA00 DO|; BREAK rev.w, ;
: REV16, r7r7 IF 3 << OR 0xBA40 DO|; BREAK rev16.w, ;
: REVSH, r7r7 IF 3 << OR 0xBAC0 DO|; BREAK revsh.w, ;

: THUMBEXPANDIMM ( #const -- cod )
  DUP 0xFF NOT AND 0= IF BREAK
  DUP 0xFF   AND DUP 16 << OR OVER = IF 0xFF AND 0x1000 OR BREAK
  DUP 0xFF00 AND DUP 16 << OR OVER = IF 24 >> 0x2000 OR BREAK
  DUP 0xFF   AND DUP  8 << OR DUP 16 << OR OVER = IF 0xFF AND 0x3000 OR BREAK
  8
  BEGIN OVER 0>
  WHILE 1+ SWAP 2* SWAP
  REPEAT
  OVER 0x00FFFFFF AND ABORT" invalid constant"
  DUP	0x01 AND  7 <<
  OVER  0x0E AND 11 << OR
  SWAP  0x10 AND
 22 << OR
  SWAP  2* 25 >> OR
\  DECIMAL
;

: MV.W,
	TO COMCOD
	B'DO|; >R
	R,#	IF  THUMBEXPANDIMM  DUP 16 >>  COMCOD OR W,  SWAP 8 <<  BREAK

	COMCOD 0xea00 0xf000 XOR XOR  TO COMCOD

	R,R   IF COMCOD W,  SWAP 8 <<  BREAK
	R,R,S IF COMCOD W,
		DUP 3 AND  6 <<  SWAP
		 0x1C AND 10 <<  OR	OR
		TSHIFT OR SWAP  8 <<  BREAK
  -333 THROW
;

: MOV.W,	0xf04F MV.W, ;
: MOVS.W,	0xf05F MV.W, ;
: MVN.W,	0xf06F MV.W, ;
: MVNS.W,	0xf07F MV.W, ;


: TORRR  3 << OR	3 << OR ;

: R,R>R,R,R
	R,R   IF >R DUP R> RX THEN ;

: RRR.W,
	TO COMCOD
	B'DO|; >R
	R,R>R,R,R
	R,R,R IF SWAP COMCOD OR W,  SWAP 8 << OR  0xf000 BREAK
  -333 THROW
;

: LSL>MOV ['] R,R,S >BODY @  TO PARM_HESH   ;

: LSL.W,
	R,R,#	IF lsl$ LSL>MOV MOV.W, BREAK
	0xfa00 RRR.W, ;
: LSLS.W,
	R,R,#	IF lsl$ LSL>MOV MOVS.W, BREAK
	0xfa10 RRR.W, ;
: LSR.W,
	R,R,#	IF lsr$ LSL>MOV MOV.W, BREAK
	0xfa20 RRR.W, ;
: LSRS.W,
	R,R,#	IF lsr$ LSL>MOV MOVS.W, BREAK
	0xfa30 RRR.W, ;
: asr.w,
	R,R,#	IF asr$ LSL>MOV MOV.W, BREAK
	0xfa40 RRR.W, ;
: asrs.w,
	R,R,#	IF asr$ LSL>MOV MOVS.W, BREAK
	0xfa50 RRR.W, ;
: ROR.W,
	R,R,#	IF ror$ LSL>MOV MOV.W, BREAK
	0xfa60 RRR.W, ;
: RORS.W,
	R,R,#	IF ror$ LSL>MOV MOVS.W, BREAK
	0xfa70 RRR.W, ;

: MUL.W,	0xfb00 RRR.W, ;
: SMULBB,	0xfb10 RRR.W, ;
: SMUAD,	0xfb20 RRR.W, ;
: SMULWB,	0xfb30 RRR.W, ;
: SMUSD,	0xfb40 RRR.W, ;
: SMMUL,	0xfb50 RRR.W, ;
: USAD8,	0xfb70 RRR.W, ;
: SDIV,	0xf0 OR	0xfb90 RRR.W, ;
: UDIV,	0xf0 OR	0xfbb0 RRR.W, ;


: MULS.W -1 ABORT" Thumb-2 MUL must not set flags" ;

: SMULL, SWAP 0xfb80 OR W, SWAP 8 << OR SWAP 12 <<  |;
: UMULL, SWAP 0xfbA0 OR W, SWAP 8 << OR SWAP 12 <<  |;
: SMLAL, SWAP 0xfbc0 OR W, SWAP 8 << OR SWAP 12 <<  |;
: UMLAL, SWAP 0xfbe0 OR W, SWAP 8 << OR SWAP 12 <<  |;
\ : SDIV,  SWAP 0xfb90 OR W, SWAP 8 << OR 0xf0f0 |;
\ : UDIV,  SWAP 0xfbb0 OR W, SWAP 8 << OR 0xf0f0 |;

: MLA,  ROT  0xfb00 OR W, 12 << OR SWAP 8 << OR  0x0000 |;
: MLS,  ROT  0xfb00 OR W, 12 << OR SWAP 8 << OR  0x0010 |;


: _XT_.W,
 RRRS IF DUP 0x18 NOT AND ABORT"  rotation can only be 0, 8, 16, or 24"
	 1 << OR
      THEN  SWAP 8 << OR  0xf080 |;

: SXTH.W,	0xfa0f W, _XT_.W, ;
: UXTH.W,	0xfa1f W, _XT_.W, ;
: SXTB16,	0xfa2f W, _XT_.W, ;
: UXTB16,	0xfa3f W, _XT_.W, ;
: SXTB.W,	0xfa4f W, _XT_.W, ;
: UXTB.W,	0xfa5f W, _XT_.W, ;

: SXTAH,	SWAP 0xfa00 OR W, _XT_.W, ;
: UXTAH,	SWAP 0xfa10 OR W, _XT_.W, ;
: SXTAB16,	SWAP 0xfa20 OR W, _XT_.W, ;
: UXTAB16,	SWAP 0xfa30 OR W, _XT_.W, ;
: SXTAB,	SWAP 0xfa40 OR W, _XT_.W, ;
: UXTAB,	SWAP 0xfa50 OR W, _XT_.W, ;

: susat,  TO COMCOD
	R#R  IF      COMCOD OR W,  SWAP 8 <<  DO|; BREAK
	R#RS IF SWAP COMCOD OR TSHIFT 0x20 = IF 0x20 OR THEN   W,
	  DUP    3 AND  6 <<
	 SWAP 0x1C AND 10 << OR OR SWAP 8 <<  DO|; BREAK
  -333 THROW
;

: SSAT, 0xf300 SUSAT, ;
: USAT, 0xf380 SUSAT, ;
: SSAT16, 0xf320 SUSAT, ;
: USAT16, 0xf3a0 SUSAT, ;

: SXTH,	
	R7R7	IF	3 << OR 0xb200 DO|; BREAK
	SXTH.W, ;

: SXTB,
	R7R7	IF	3 << OR 0xb240 DO|; BREAK
	SXTB.W, ;

: UXTH,	
	R7R7	IF	3 << OR 0xb280 DO|; BREAK
	UXTH.W, ;

: UXTB,	
	R7R7	IF	3 << OR 0xb2c0 DO|; BREAK
	UXTB.W, ;



: SADD8,	0xfa80 RRR.W, ;
: SADD16,	0xfa90 RRR.W, ;
: SASX,		0xfaa0 RRR.W, ;
: SSUB8,	0xfac0 RRR.W, ;
: SSUB16,	0xfad0 RRR.W, ;
: SSAX,		0xfae0 RRR.W, ;

: TOADDPCPC 3 << SWAP DUP 7 AND SWAP 8 AND 4 << OR OR ;
: R,PC,# OVER 15 = R,R,# AND ;
: R,SP,# OVER 13 = R,R,# AND ;
: SP,#	 OVER 13 =   R,# AND ;
: SP,SP,# R,R,# 2OVER OVER = AND 13 = AND ;

: |R,PC,# 3DUP 0x3FC NOT AND
	  SWAP 15 <> OR
	  SWAP 7 NOT AND OR 0=  R,R,# AND  ;
: |R,SP,# 3DUP 0x3FC NOT AND
	  SWAP 13 <> OR
	  SWAP 7 NOT AND OR 0=  R,R,# AND  ;


: MOVWT,
  R,# 0= IF   -333 THROW THEN
  OVER 0xFFFF NOT AND ABORT" invalid constant"
  OVER 0xF000 AND 12 >> OR
  OVER 0x800 AND 1 >> OR W,
  DUP 0xFF AND ROT 8 << OR
  SWAP 0x700 AND 4 <<
 DO|;
;

: MOVW, 0xf240  MOVWT, ;
: MOVT, 0xf2C0  MOVWT, ;

: ADDSUBW,
  R,R,# 0= IF   -333 THROW THEN
  OVER ABS 0xFFF NOT AND ABORT" invalid constant"
  OVER 0< IF  0xA0 XOR >R NEGATE R> THEN
  ROT OR
  OVER 0x800 AND 1 >> OR W,
  DUP 0xFF AND ROT 8 << OR 
  SWAP 0x700 AND 4 <<
 DO|;
;

: ADDW, 0xf200  ADDSUBW, ;
: SUBW, 0xf2A0  ADDSUBW, ;


: CM.W,
	TO COMCOD
	B'DO|; >R
	R,#	IF THUMBEXPANDIMM SWAP COMCOD OR OVER 16 >> OR  W, 0xf00   BREAK

	COMCOD 0xea00 0xf000 XOR XOR  TO COMCOD

	R,R	IF SWAP COMCOD OR W,   0xf00  BREAK
	R,R,S IF ROT	COMCOD OR W,
			DUP 3 AND  6 <<  SWAP
			0x1C AND 10 <<   OR	OR
			TSHIFT OR  0xf00  BREAK
  -333 THROW
;

: TST.W,	0xf010 CM.W, ;
: TEQ,		0xf090 CM.W, ;
: CMN.W,	0xf110 CM.W, ;
: CMP.W,	0xf1b0 CM.W, ;


: ALU.W,
	TO COMCOD
	B'DO|; >R

	R,#	IF THUMBEXPANDIMM OVER COMCOD OR OVER 16 >> OR  W, SWAP 8 <<  BREAK
	R,R,#	IF THUMBEXPANDIMM SWAP COMCOD OR OVER 16 >> OR  W, SWAP 8 <<  BREAK

	COMCOD 0xea00 0xf000 XOR XOR  TO COMCOD

	R,R>R,R,R	
	R,R,R	IF DUP 0xF NOT AND ABORT" # EXPECTED"
 SWAP COMCOD OR W,  SWAP 8 <<  BREAK
	RRRS IF ROT	COMCOD OR W,
			DUP 3 AND  6 <<  SWAP
			0x1C AND 10 <<   OR	OR
			TSHIFT OR SWAP 8 <<  BREAK

  -333 THROW
;

: AND.W,	0xF000 ALU.W, ;
: ANDS.W,	0xF010 ALU.W, ;
: BIC.W,	0xF020 ALU.W, ;
: BICS.W,	0xF030 ALU.W, ;
: ORR.W,	0xF040 ALU.W, ;
: ORRS.W,	0xF050 ALU.W, ;
: ORN,		0xF060 ALU.W, ;
: ORNS,		0xF070 ALU.W, ;
: EOR.W,	0xF080 ALU.W, ;
: EORS.W,	0xF090 ALU.W, ;
: PKHBT		0xF0C0 ALU.W, ;
: ADD.W,	0xF100 ALU.W, ;
: ADDS.W,	0xF110 ALU.W, ;
: ADC.W,	0xF140 ALU.W, ;
: ADCS.W,	0xF150 ALU.W, ;
: SBC.W,	0xF160 ALU.W, ;
: SBCS.W,	0xF170 ALU.W, ;
: SUB.W,	0xF1a0 ALU.W, ;
: SUBS.W,	0xF1b0 ALU.W, ;
: RSB.W,	0xF1C0 ALU.W, ;
: RSBS.W,	0xF1D0 ALU.W, ;

: NEG.W,  ## 0 rsb.w, ;
: NEGS.W, ## 0 rsbs.w, ;

: AND,
	IT_V 0x1F AND
	IF	R7R7	IF	3 << OR 0x4000 DO|; BREAK
	THEN    AND.W,
;

: ANDS,
	IT_V 0x1F AND 0=
	IF	R7R7	IF	3 << OR 0x4000 DO|; BREAK
	THEN ANDS.W,
;

: EOR,
	IT_V 0x1F AND
	IF	R7R7	IF	3 << OR 0x4040 DO|; BREAK
	THEN    EOR.W,
;

: EORS,
	IT_V 0x1F AND 0=
	IF	R7R7	IF	3 << OR 0x4040 DO|; BREAK
	THEN EORS.W,
;

: LSL,
	IT_V 0x1F AND
	IF	R7R7	IF	3 << OR 0x4080	DO|; BREAK
		|R,R,0#	IF DROP	3 << OR 0	DO|; BREAK
		>R,R,#	IF DUP 31 U> ABORT" invalid shift value"
				0x1f AND 3 << OR 3 << DO|; BREAK
	THEN    LSL.W,
;

: LSLS,
	IT_V 0x1F AND 0=
	IF	R7R7	IF	3 << OR 0x4080	DO|; BREAK
		|R,R,0#	IF DROP	3 << OR 0	DO|; BREAK
		>R,R,#	IF DUP 31 U> ABORT" invalid shift value"
				0x1f AND 3 << OR 3 << DO|; BREAK
	THEN LSLS.W,
;

: LSR,
	IT_V 0x1F AND
	IF	R7R7	IF	3 << OR 0x40C0	DO|; BREAK
		|R,R,0#	IF DROP	3 << OR 0	DO|; BREAK
		>R,R,#	IF 0x1f AND 3 << OR 3 <<  OR 0x0800 DO|; BREAK
	THEN	LSR.W,
;

: LSRS,
	IT_V 0x1F AND 0=
	IF	R7R7	IF	3 << OR 0x40C0	DO|; BREAK
		|R,R,0#	IF DROP	3 << OR 0	DO|; BREAK
		>R,R,#	IF 0x1f AND 3 << OR 3 <<  OR 0x0800 DO|; BREAK
	THEN	LSRS.W,
;

: ASR,
	IT_V 0x1F AND
	IF	R7R7	IF	3 << OR 0x4100	DO|; BREAK
		|R,R,0#	IF DROP	3 << OR 0	DO|; BREAK
		>R,R,#	IF 0x1f AND  3 << OR 3 << OR  0x1000 DO|; BREAK
	THEN	asr.w,
;

: ASRS,
	IT_V 0x1F AND 0=
	IF	R7R7	IF	3 << OR 0x4100	DO|; BREAK
		|R,R,0#	IF DROP	3 << OR 0	DO|; BREAK
		>R,R,#	IF 0x1f AND  3 << OR 3 << OR  0x1000 DO|; BREAK
	THEN	asrs.w,
;

: ADC,
	IT_V 0x1F AND
	IF	R7R7	IF	3 << OR 0x4140 DO|; BREAK
	THEN	ADC.W,
;

: ADCS,
	IT_V 0x1F AND 0=
	IF	R7R7	IF	3 << OR 0x4140 DO|; BREAK
	THEN	ADCS.W,
;

: SBC,
	IT_V 0x1F AND
	IF	R7R7	IF	3 << OR 0x4180 DO|; BREAK
	THEN	SBC.W,
;

: SBCS,
	IT_V 0x1F AND 0=
	IF	R7R7	IF	3 << OR 0x4180 DO|; BREAK
	THEN	SBCS.W,
;

: ROR,
	IT_V 0x1F AND
	IF	R7R7	IF	3 << OR 0x41C0 DO|; BREAK
	THEN	ROR.W,
;

: RORS,
	IT_V 0x1F AND 0=
	IF	R7R7	IF	3 << OR 0x41C0 DO|; BREAK
	THEN	RORS.W,
;

: TST, 	R7R7	IF	3 << OR 0x4200 DO|; BREAK
	TST.W, ;

: CMN,    3 << OR 0x42C0 |;

: NEG,
	IT_V 0x1F AND
	IF  r7r7 IF 3 << OR 0x4240 DO|; BREAK
	THEN
	NEG.W, ;

: NEGS,
	IT_V 0x1F AND 0=
	IF  r7r7 IF 3 << OR 0x4240 DO|; BREAK
	THEN
	NEGS.W, ;


: r7r7#0  >R  2DUP OR 7 NOT AND  R@ OR 0= R> SWAP  R,R,# AND ;

: RSB,
	IT_V 0x1F AND
	IF  r7r7#0 IF DROP 3 << OR 0x4240 DO|; BREAK
	THEN
	RSB.W, ;

: RSBS,
	IT_V 0x1F AND 0=
	IF   r7r7#0 IF DROP 3 << OR 0x4240 DO|; BREAK
	THEN
	RSBS.W, ;


: ORR,
	IT_V 0x1F AND
	IF	R7R7	IF	3 << OR 0x4300 DO|; BREAK
	THEN	ORR.W,
;

: ORRS,
	IT_V 0x1F AND 0=
	IF	R7R7	IF	3 << OR 0x4300 DO|; BREAK
	THEN	ORRS.W,
;

: MUL,
	IT_V 0x1F AND
	IF	R7R7	IF	3 << OR 0x4340 DO|; BREAK
	THEN	MUL.W,
;

: MULS,
	IT_V 0x1F AND 0=
	IF	R7R7	IF	3 << OR 0x4340 DO|; BREAK
	THEN	MULS.W
;

: MVN,
	IT_V 0x1F AND
	IF	R7R7	IF	3 << OR 0x43C0 DO|; BREAK
	THEN	MVN.W,
;

: MVNS,
	IT_V 0x1F AND 0=
	IF	R7R7	IF	3 << OR 0x43C0 DO|; BREAK
	THEN	MVNS.W,
;

: BIC,
	IT_V 0x1F AND
	IF	R7R7	IF	3 << OR 0x4380 DO|; BREAK
	THEN	BIC.W,
;

: BICS,
	IT_V 0x1F AND 0=
	IF	R7R7	IF	3 << OR 0x4380 DO|; BREAK
	THEN	BICS.W,
;

: ADD,
	B'DO|; >R
	R,R	IF TOADDPCPC  0x4400	BREAK
\	0 \ DUP 0x3FC NOT AND 0=
\	IF
	|R,PC,#	IF NIP 2 >> SWAP 8 << OR	0xA000	BREAK
	|R,SP,#	IF NIP 2 >> SWAP 8 << OR	0xA800	BREAK
\	THEN

	DUP 0x1FC NOT AND 0=
	IF
	SP,#	IF	NIP 2 >>	0xB000	BREAK
	SP,SP,#	IF NIP	NIP 2 >>	0xB000	BREAK
	THEN

	R,PC,#	IF RDROP ADDW,	BREAK
	R,SP,#	IF RDROP ADDW,	BREAK

	IT_V 0x1F AND
	IF	|R,R,R	IF TORRR  0x1800	BREAK
		|R,R,#	IF TORRR  0x1c00	BREAK
		|R,#	IF SWAP 8 << OR 0x3000	BREAK
	THEN

	RDROP ADD.W,
;

: ADDS,
	IT_V 0x1F AND 0=
	IF	B'DO|; >R
		|R,R,R	IF TORRR  0x1800	BREAK
		|R,R,#	IF TORRR  0x1c00	BREAK
		|R,#	IF SWAP 8 << OR 0x3000	BREAK
		RDROP
	THEN ADDS.W,
;

: SUB,  	B'DO|; >R
	IT_V 0x1F AND
	IF
				|R,R,R	IF TORRR  0x1A00	BREAK
		|R,R,#	IF TORRR  0x1E00	BREAK
		|R,#	IF SWAP 8 << OR 0x3800	BREAK
	THEN
	DUP 0x1FC NOT AND 0=
	IF
		SP,#	IF	NIP 2 >>	0xB080	BREAK
		SP,SP,#	IF NIP	NIP 2 >>	0xB080	BREAK
	THEN
        RDROP	SUB.W,
;

: SUBS,
	IT_V 0x1F AND 0=
	IF	B'DO|; >R
		|R,R,R	IF TORRR  0x1A00	BREAK
		|R,R,#	IF TORRR  0x1E00	BREAK
		|R,#	IF SWAP 8 << OR 0x3800	BREAK
		RDROP
	THEN	SUBS.W,
;


: MOV,
	R,R	IF TOADDPCPC 0x4600 DO|; BREAK
	IT_V 0x1F AND
	IF
		|R,#	IF SWAP 8 << OR 0x2000	DO|; BREAK
	THEN
	MOV.W, ;

: MOVS,
	IT_V 0x1F AND 0=
	IF	R7R7	IF	3 << OR 0	DO|; BREAK
		|R,#	IF SWAP 8 << OR 0x2000	DO|; BREAK
	THEN	movs.w,
;

: CMP,
	R,R	IF	2DUP OR  0x7 NOT AND
			IF    TOADDPCPC 0x4500 \ 4 << OR 0x4500
			ELSE   3 << OR 0x4280
			THEN DO|; BREAK
	|R,#	IF    SWAP 8 << OR   0x2800 DO|; BREAK
	CMP.W,
;

: LDREX,
	R[R]	IF	0xe850 OR W,	  12 << 0x0f00	  DO|; BREAK
	DUP 0x3fc NOT AND IF -333 THROW THEN
	R[R#]	IF SWAP 0xe850 OR W, 2 >> SWAP 12 << 0x0f00 OR DO|; BREAK
  -333 THROW
;

: LDREXB, R[R] 0= IF   -333 THROW THEN	0xe8d0 OR W, 12 << 0x0f4f |;
: LDREXH, R[R] 0= IF   -333 THROW THEN	0xe8d0 OR W, 12 << 0x0f5f |;


: STREX,
	RR[R]	IF	0xe840 OR W,	       12 <<    SWAP 8 <<  DO|; BREAK
	DUP 0x3fc NOT AND IF -333 THROW THEN
	RR[R#]	IF SWAP 0xe840 OR W, 2 >> SWAP 12 << OR SWAP 8 <<  DO|; BREAK
  -333 THROW ;

: STREXB,	0xE8C0 OR W, 12 << 0x0f40 OR |;
: STREXH,	0xE8C0 OR W, 12 << 0x0f50 OR |;

: STR[R]# ( n -- PUW r<<12 )
	DUP 0<
	IF   NEGATE  0xFF AND
	ELSE 0xFF AND 0x200 OR
	THEN 0x800 OR
	 !!F 0x100 AND	 OR
 SWAP 12 << OR
;

\ : R[R#]!  R[R#] !!F AND ;
\ : R[R#-]  DUP 0< R[R#] AND ;

: STRLDR.W,
	TO COMCOD
	B'DO|; >R
	R,	IF HERE 4 + -	DUP 0<> 0x80 AND  COMCOD 0xF OR OR  W,
		  SWAP  12 << BREAK

	R[R#]	IF OVER 15 =
		IF NIP		DUP 0<> 0x80 AND  COMCOD 0xF OR OR  W,
		 SWAP 12 << BREAK
	DUP 0xFFF NOT AND 0=  !!F 0= AND
			IF   SWAP COMCOD 0x80 OR OR W, SWAP 12 << BREAK

		 SWAP COMCOD OR W, STR[R]# 0x400 BREAK
	R[R]#	IF SWAP COMCOD OR W, STR[R]# 0x100 BREAK
	R[R]	IF COMCOD 0x80 OR OR W,		     12 << 0 BREAK
	R[RR]	IF SWAP COMCOD OR W, 		SWAP 12 << BREAK
	R[RRS]	IF ROT  COMCOD OR W, 4 << OR	SWAP 12 << BREAK
  -333 THROW
;

: STRB.W,	0xf800 STRLDR.W, ;
: LDRB.W,	0xf810 STRLDR.W, ;

: STRH.W,	0xf820 STRLDR.W, ;
: LDRH.W,	0xf830 STRLDR.W, ;

: STR.W,	0xf840 STRLDR.W, ;

: LDRSB.W,	0xf910 STRLDR.W, ;
: LDRSH.W,	0xf930 STRLDR.W, ;


: RDRR[R#]

 SWAP COMCOD OR
		   OVER 0< IF SWAP NEGATE SWAP  ELSE 0x80 OR THEN
			!!F IF 0x20 OR THEN	W,
			2 >>
			SWAP 8 << OR
			SWAP 12 <<  DO|;
;

: STLDRD,
	TO COMCOD
	RR[R#]	IF			  RDRR[R#] BREAK
	R[R#]	IF 2>R DUP 1+ 2R>	  RDRR[R#] BREAK
	RR[R]	IF			0 RDRR[R#] BREAK
	R[R]	IF 2>R DUP 1+ 2R>	0 RDRR[R#] BREAK
	-333 THROW
;

: STRD, 0xE940  STLDRD, ;
: LDRD, 0xE950  STLDRD, ;

: STR,
	!!F IF STR.W, BREAK
	B'DO|; >R
	|R[RR]	IF 3 << OR 3 << OR  0x5000 BREAK
	R[R]    IF ['] R[R#] >BODY @ TO PARM_HESH 0 THEN
	|R[R#]	IF 1 << OR 3 << OR  0x6000 BREAK
	|R[SP#]	IF NIP 2 >> SWAP 8 << OR 0x9000 BREAK
	RDROP STR.W,
;


: STRB,
	!!F IF STRB.W, BREAK
	|R[RR]	IF 3 << OR 3 << OR  0x5400 DO|; BREAK
	|bR[R#]	IF 3 << OR 3 << OR  0x7000 DO|; BREAK
	|R[R]	IF 	   3 << OR  0x7000 DO|; BREAK
	STRB.W,
;


: STRH,
	!!F IF STRH.W, BREAK
	|R[RR]	IF 3 << OR 3 << OR  0x5200 DO|; BREAK
	|hR[R#]	IF 2 << OR 3 << OR  0x8000 DO|; BREAK
	|R[R]	IF 	   3 << OR  0x8000 DO|; BREAK
	STRH.W,
;

: ADR.W,
 HERE 3 NOT AND  4 + -
 NPASS 0=
 IF     0xFFF AND -1 TO MOREPASS
 THEN    PC SWAP ## ADDW,
;

\+ ?REAL ?REAL 0= [IF] ROMEND ROMBIG [THEN]

: ADR,
 NPASS
 IF
	DUP
	HERE W@ 0xf000 AND 0xf000 = IF 2- THEN
	DUP 3 AND
	SWAP HERE 2+ -  0x3FE NOT AND OR 0=
	IF	HERE   2 + -	2 >> SWAP 8 << OR	0xA000 DO|;
	BREAK   ADR.W,
 BREAK	HERE   2 + -
   0x3FC AND  2 >> SWAP 8 << OR	0xA000 DO|; -1 TO MOREPASS
;

: LDR_R.W,
 HERE 3 NOT AND  4 + -
 NPASS 0=
 IF     0xFFF AND -1 TO MOREPASS
 THEN   [[ PC SWAP ## ]] 0xf850 STRLDR.W, \ LDR.W,
;

: LDR.W,
	R,=	IF :=TAB :=COUNT +
\ CR ." LP=" 2DUP H. H.
 ! :=COUNT 4 + TO :=COUNT
 0 TO PARM_HESH RX
		LDR_R.W,   BREAK

	0xf850 STRLDR.W, ;

: LDR_R,
 NPASS
 IF
	DUP
	HERE W@ 0xf000 AND 0xf000 = IF 2- THEN
	DUP 3 AND
	SWAP HERE 2+ -  0x3FE NOT AND OR 0=
	IF	HERE   2 + -	2 >> SWAP 8 << OR	0x4800 DO|;
	BREAK   LDR_R.W,
 BREAK	HERE   2 + -
   0x3FC AND  2 >> SWAP 8 << OR	0x4800 DO|; -1 TO MOREPASS
;

: LDR,
	!!F IF LDR.W, BREAK
	B'DO|; >R
	|R[RR]	IF 3 << OR 3 << OR  0x5800 BREAK
	R[R]    IF ['] R[R#] >BODY @ TO PARM_HESH 0 THEN
	|R[R#]	IF 1 << OR 3 << OR  0x6800 BREAK
	|R[SP#]	IF NIP 2 >> SWAP 8 << OR 0x9800 BREAK
	|R[PC#]	IF NIP 2 >> SWAP 8 << OR 0x4800 BREAK
	R,	IF RDROP LDR_R,  BREAK
 2 PICK 8 <
	R,= AND	IF :=TAB :=COUNT + ! :=COUNT 4 + TO :=COUNT
		HERE   2 + - 0x3FC AND  2 >> SWAP 8 << OR	0x4800   BREAK

	RDROP LDR.W,
;

: LDRB,
	!!F IF LDRB.W, BREAK
	|R[RR]	IF 3 << OR 3 << OR  0x5C00 DO|; BREAK
	|bR[R#]	IF 3 << OR 3 << OR  0x7800 DO|; BREAK
	|R[R]	IF	   3 << OR  0x7800 DO|; BREAK
	LDRB.W,
;

: LDRH,
	!!F IF LDRH.W, BREAK
	|R[RR]	IF 3 << OR 3 << OR  0x5A00 DO|; BREAK
	|hR[R#]	IF 2 << OR 3 << OR  0x8800 DO|; BREAK
	|R[R]	IF	   3 << OR  0x8800 DO|; BREAK
	LDRH.W,
;

: LDRSH,
	!!F IF LDRSH.W, BREAK
	|R[RR]	IF 3 << OR 3 << OR  0x5E00 DO|; BREAK
	LDRSH.W,
;


: LDRSB,
	!!F IF LDRSH.W, BREAK
	|R[RR]	IF 3 << OR 3 << OR  0x5600 DO|; BREAK
	LDRSH.W,
;


\ enumerated condition codes
0x00 CONSTANT EQ	\ equal or zero
0x10 CONSTANT NE	\ not equal
0x20 CONSTANT CS	\ carry set
0x30 CONSTANT CC	\ carry clear
0x40 CONSTANT MI	\ minus or negative
0x50 CONSTANT PL	\ zero or positive
0x60 CONSTANT VS	\ overflow set
0x70 CONSTANT VC	\ overflow clear
0x80 CONSTANT HI	\ higher (unsigned)
0x90 CONSTANT LS	\ lower or same (unsigned)
0xA0 CONSTANT GE	\ greater then or equal (signed)
0xB0 CONSTANT LT	\ less than (signed)
0xC0 CONSTANT GT	\ greter than (signed)
0xD0 CONSTANT LE	\ less then or equal (signed)
0xE0 CONSTANT AL


: ITTTT,	DUP 16 AND IF 0xBF0F  ELSE 0xBF01 THEN DUP TO IT_V |;
: ITTT,		DUP 16 AND IF 0xBF0E  ELSE 0xBF02 THEN DUP TO IT_V |;
: ITTTE,	DUP 16 AND IF 0xBF0D  ELSE 0xBF03 THEN DUP TO IT_V |;
: ITT,		DUP 16 AND IF 0xBF0C  ELSE 0xBF04 THEN DUP TO IT_V |;
: ITTET,	DUP 16 AND IF 0xBF0B  ELSE 0xBF05 THEN DUP TO IT_V |;
: ITTE,		DUP 16 AND IF 0xBF0A  ELSE 0xBF06 THEN DUP TO IT_V |;
: ITTEE,	DUP 16 AND IF 0xBF09  ELSE 0xBF07 THEN DUP TO IT_V |;
: IT,			      0xBF08		       DUP TO IT_V |;
: ITETT,	DUP 16 AND IF 0xBF07  ELSE 0xBF09 THEN DUP TO IT_V |;
: ITET,		DUP 16 AND IF 0xBF06  ELSE 0xBF0A THEN DUP TO IT_V |;
: ITETE,	DUP 16 AND IF 0xBF05  ELSE 0xBF0B THEN DUP TO IT_V |;
: ITE,		DUP 16 AND IF 0xBF04  ELSE 0xBF0C THEN DUP TO IT_V |;
: ITEET,	DUP 16 AND IF 0xBF03  ELSE 0xBF0D THEN DUP TO IT_V |;
: ITEE,		DUP 16 AND IF 0xBF02  ELSE 0xBF0E THEN DUP TO IT_V |;
: ITEEE,	DUP 16 AND IF 0xBF01  ELSE 0xBF0F THEN DUP TO IT_V |;

: STLDMIA.W,
	TO COMCOD
	B'DO|; >R
	R{	IF COMCOD  OR !!F 0x20 AND OR W, {{}} 0 BREAK
  -333 THROW
;

: STMIA.W, 0xE880 STLDMIA.W, ;
: LDMIA.W, 0xE890 STLDMIA.W, ;

: STMDB, 0xE900 STLDMIA.W, ;
: LDMDB, 0xE910 STLDMIA.W, ;

: STMDB.W,  STMDB, ;
: LDMDB.W,  LDMDB, ;

: STMEA.W, STMIA.W, ;
: STM.W, STMIA.W, ;

: POP.W,
 0 TO PARM_HESH
 SP !! 6 >PARM_HESH  LDMIA.W, ;

: PUSH.W,
 0 TO PARM_HESH
 SP !! 6 >PARM_HESH  STMDB, ;

: POP,
  {{}} 0x80FF NOT AND IF  POP.W, BREAK
	{{}} 0xFF AND {{}} 0x8000 AND 7 >> OR 0xbc00  |;

: PUSH,
  {{}} 0x40FF NOT AND IF  PUSH.W, BREAK
	{{}} 0xFF AND {{}} 0x4000 AND 6 >> OR 0xb400  |;


: |R{ R{ {{}} 0xFF00 AND 0= AND  ;

: stmia,
	|R{ !!F AND IF  8 <<  {{}} + 0xC000  DO|;  BREAK
	STMIA.W,
;

: STMEA, STMIA, ;
: STM, STMIA, ;

: LDMFD.W, LDMIA.W, ;
: LDM.W, LDMIA.W, ;

: LDMIA,
	1 OVER << {{}} AND 0<> !!F XOR
	|R{ AND	IF  8 <<  {{}} + 0xC800 DO|; BREAK
	LDMIA.W,
;

: LDMFD, LDMIA, ;
: LDM, LDMIA, ;

: TBB,		SWAP 0xE8D0 OR W,  0xF000 |;
: tbH,	DROP	SWAP 0xE8D0 OR W,  0xF010 |;

\ : BF2| 1+ OVER 3 AND 6 << OR SWAP 10 << OR SWAP 8 << |;
: BF2|_ 1- OVER 3 AND 6 << OR SWAP 10 << OR SWAP 8 << |;
: BF2|  1- 
 OVER    3 AND  6 << OR
 SWAP 0x1C AND 10 << OR  \ NIP
 SWAP 8 << |;

: SBFX,	ROT	0xf340 OR W, BF2| ;
: UBFX,	ROT	0xf3c0 OR W, BF2| ;
: BFI,	ROT	0xf360 OR W, OVER + BF2| ;
: BFC,		0xf36f    W, OVER + BF2| ;

: cps_PAR ( -- n )
  0 PARSE-NAME 0 DO COUNT 0xF AND
 DUP 1 = IF DROP 4 ELSE
 DUP 9 = IF DROP 2 ELSE
 DUP 6 = IF DROP 1 ELSE
 -1 ABORT"  unrecognized CPS flag"
 THEN  THEN  THEN 
  ROT OR SWAP LOOP DROP ;

: cpsie,  0xb660 |;
: cpsid,  0xb670 |;

     0x00 CONSTANT APSR
     0x01 CONSTANT IAPSR
     0x02 CONSTANT EAPSR
     0x03 CONSTANT PSR
     0x05 CONSTANT IPSR
     0x06 CONSTANT EPSR
     0x07 CONSTANT IEPSR
     0x08 CONSTANT MSP
     0x09 CONSTANT PSP
     0x10 CONSTANT PRIMASK
     0x11 CONSTANT BASEPRI
     0x12 CONSTANT BASEPRI_MASK
     0x13 CONSTANT FAULTMASK
     0x14 CONSTANT CONTROL

: MRS,	0xf3ef W, SWAP 8 << OR 0x8000 |;
: MSR,	0xf380 OR W, 0x8800 |;

: DIMSB, 0xf3bf W, #N 0= IF DSB_OPT THEN ;

: DBG,  0xf3af W, 0x80f0 |;
\ : DSB,  0xf3Bf W, 0x8f40 |;
\ : DMB,  0xf3Bf W, 0x8f50 |;


: DSB,	DIMSB, 0x8F40 SY |;
: DMB,	DIMSB, 0x8F50 SY |;
: ISB,	DIMSB, 0x8F60 SY |;
: SVC,	0xDF00	|;
: SWI,	0xDF00	|;
: BKPT,	0xBE00	|;

: .WORD, I, ;

\ float

: SX 8 >PARM_HESH ;

: S: CREATE DUP   , 1+ DOES> @
 ?{{
 IF  1 TO {{}}
 THEN  SX ;

0 S: S0  S: S1  S: S2  S: S3   S: S4  S: S5  S: S6  S: S7
  S: S8  S: S9  S: S10 S: S11  S: S12 S: S13 S: S14 S: S15
  S: S16 S: S17 S: S18 S: S19  S: S20 S: S21 S: S22 S: S23
  S: S24 S: S25 S: S26 S: S27  S: S28 S: S29 S: S30 S: S31 DROP

: -S: CREATE DUP , 1+ DOES> @
        OVER - TO {{}}
;

2
	   -S: -S1  -S: -S2  -S: -S3 
  -S: -S4  -S: -S5  -S: -S6  -S: -S7 
  -S: -S8  -S: -S9  -S: -S10 -S: -S11
  -S: -S12 -S: -S13 -S: -S14 -S: -S15
  -S: -S16 -S: -S17 -S: -S18 -S: -S19
  -S: -S20 -S: -S21 -S: -S22 -S: -S23
  -S: -S24 -S: -S25 -S: -S26 -S: -S27
  -S: -S28 -S: -S29 -S: -S30 -S: -S31
DROP


	RX SX		PARAM: R,S
	SX RX		PARAM: S,R
	SX SX		PARAM: S,S
	SX SX SX	PARAM: S,S,S


: fpscr ;
: APSR_nzcv  R15
 ;

: VMRS,	0xeef1 W, 12 << 0xA10	|;


: SS.f32,	( d m cod cod1 -- )	\ <Sd>, <Sm>
  S,S 0= IF -333 THROW THEN
  >R TO COMCOD
  DUP 1 AND 5 << R> OR SWAP 1 >> OR >R \ d  \ <Sm> finished
  COMCOD OVER 1 AND 6 << OR W,
  1 >> 12 <<  R> |;

: SSS.f32,	( d n m cod cod1 -- )	\ <Sd>, <Sn>, <Sm>
  S,S,S 0= IF -333 THROW THEN
  >R TO COMCOD
  DUP 1 AND 5 << R> OR SWAP 1 >> OR >R  \ d n
  DUP 1 AND 7 << R> OR >R
  1 >> COMCOD OR OVER 1 AND 6 << OR W,
  1 >> 12 <<  R> |;

: VMOV.F32,	 0xeeb0 0x0a40 SS.f32, ;
: VABS.F32,	 0xeeb0 0x0aC0 SS.f32, ;
: VNEG.F32,	 0xeeb1 0x0a40 SS.f32, ;
: VSQRT.F32,	 0xeeb1 0x0aC0 SS.f32, ;
: VCVTB.F32.F16, 0xeeb2 0x0a40 SS.f32, ;
: VCVTT.F32.F16, 0xeeb2 0x0ac0 SS.f32, ;
: VCVTB.F16.F32, 0xeeb3 0x0a40 SS.f32, ;
: VCVTT.F16.F32, 0xeeb3 0x0ac0 SS.f32, ;
: VSSCMP.F32,	 0xeeb4 0x0a40 SS.f32, ;
: VSSCMPE.F32,	 0xeeb4 0x0ac0 SS.f32, ;
: VCVT.F32.S32,  0xeeb8 0x0ac0 SS.f32, ;
: VCVT.F32.U32,  0xeeb8 0x0a40 SS.f32, ;
: VCVT.U32.F32,  0xeebc 0x0ac0 SS.f32, ;
: VCVTR.U32.F32, 0xeebc 0x0a40 SS.f32, ;
: VCVTR.S32.F32, 0xeebd 0x0a40 SS.f32, ;
: VCVT.S32.F32,  0xeebd 0x0ac0 SS.f32, ;

: VCMP.F32,
	S,S IF VSSCMP.F32, BREAK
	0<> IF -24 THROW THEN
	0 TO PARM_HESH SX S0
	0xeeb5 0x0a40 SS.f32, ;

: VCMPE.F32,
	S,S IF VSSCMPE.F32, BREAK
	0<> IF -24 THROW THEN
	0 TO PARM_HESH SX S0
	0xeeb5 0x0ac0 SS.f32, ;



: VMLA.F32,	0xee00 0x0a00 SSS.f32, ;
: VMLS.F32,	0xee00 0x0a40 SSS.f32, ;
: VNMLA.F32,	0xee10 0x0a40 SSS.f32, ;
: VNMLS.F32,	0xee10 0x0a00 SSS.f32, ;
: VMUL.F32,	0xee20 0x0a00 SSS.f32, ;
: VNMUL.F32,	0xee20 0x0a40 SSS.f32, ;
: VADD.F32,	0xee30 0x0a00 SSS.f32, ;
: VSUB.F32,	0xee30 0x0a40 SSS.f32, ;
: VDIV.F32,	0xee80 0x0a00 SSS.f32, ;
: VFMNS.F32,	0xee90 0x0a00 SSS.f32, ;
: VFMNA.F32,	0xee90 0x0a40 SSS.f32, ;
: VFMA.F32,	0xeea0 0x0a00 SSS.f32, ;
: VFMS.F32,	0xeea0 0x0a40 SSS.f32, ;


: VMOV,
	S,S IF VMOV.F32, BREAK
	R,S IF 0x20 OR SWAP	ELSE	S,R 0= IF -333 THROW THEN	THEN
	OVER 1 >> 0xEE00 OR W,
	12 <<  SWAP 1 AND 7 << OR
	0xa10 |;

: VLDMIA,

	SWAP 0xec90 OR OVER 1 AND 6 << OR
	!!F IF 0x20 OR THEN W,
	1 >> 12 << 0xa00 OR {{}} 	|;

VARIABLE NLASTS NLASTS 0!
0 VALUE TEB_CLEAN

: CODL
  1 ALLOT
\	FF_ALIGN
[IFDEF] TEXEC_BUF
	>IN M@ >R
\ CR ." CODL=<"
	PARSE-NAME \ 2DUP TYPE  ." |"
\ 	ALSO FORTH
 SFIND \  2DUP H. H.  ." >"
\ PREVIOUS

	IF \	NPASS 0=
\		IF
			TEB_CLEAN 4 + TO TEB_CLEAN
			TEXEC_BUF TEXEC_BUF CELL+ TEB_SIZE MOVE
			TEXEC_KEY TEXEC_KEY CELL+ TEB_SIZE MOVE
\		THEN
			TEXEC_BUF M!
		HERE	TEXEC_KEY M!
	ELSE	2DROP
	THEN

	R> >IN M!
[THEN]
	HEADER
\- SMUDGE  LAST @ CURRENT @ !
        NLASTS 1+!
  -1 ALLOT
;

\ : FCODL F7_ED CODL  ;

VECT A?HEAD

' NOOP TO A?HEAD

(
[IFNDEF] HERE-TAB-CUR VARIABLE HERE-TAB-CUR
[THEN]

[IFNDEF] SHERE-TAB-CUR VARIABLE SHERE-TAB-CUR
[THEN]
)
\+ HERE-TAB-CUR VARIABLE HERE-TAB-CUR-SAVE

\+ HERE-TAB-CUR VARIABLE SHERE-TAB-CUR-SAVE

0 VALUE MOREPASSFLG
0 VALUE SAVELAST

: ASM_END
  :=COUNT
 IF  HERE 3 AND IF 0xFFFF W, THEN
	HERE :=ADR =
	IF   :=TAB :=COUNT  BOUNDS DO I @ I, 4 +LOOP
	ELSE HERE TO :=ADR -1 TO MOREPASS
	THEN
 THEN
 MOREPASS TO MOREPASSFLG
 MOREPASS IF
\ CR ." MOREPASS"
\+ HERE-TAB-CUR 	HERE-TAB-CUR-SAVE @ HERE-TAB-CUR !
\+ HERE-TAB-CUR 	SHERE-TAB-CUR-SAVE @ SHERE-TAB-CUR !
  ITERNUM 2+ TO   ITERNUM
   NPASS 1+ TO NPASS
\+ HERE-TAB-CUR 	TEXEC_BUF TEB_CLEAN + TEXEC_BUF TEB_SIZE  MOVE
\+ HERE-TAB-CUR 	TEXEC_KEY TEB_CLEAN + TEXEC_KEY TEB_SIZE  MOVE
	0 TO TEB_CLEAN
  0 TO MOREPASS
  0 TO :=COUNT
  SAVELAST CURRENT M@ M!
\   XN 1- TO XN
   POSTPONE [AGAIN]
   EXIT
 THEN
	PREVIOUS

( !!!!
	LAST M@ NLASTS M@
	BEGIN  DUP
	WHILE A?HEAD LAST M@ CDR LAST M! 1-
	REPEAT NLASTS M!
	LAST M! )
;

: >BXX.H   4 << 0xD000 OR ;

: IF,        >BXX.H W, HERE 2- ;
: AHEAD,     0xE000 W, HERE 2- ;
: THEN,      HERE OVER - 4 - 2/ SWAP C! ;
: ELSE,      AHEAD,  SWAP THEN, ;
: BEGIN,     HERE ;
: UNTIL,     >BXX.H SWAP  BXX.N OR W, ;
: AGAIN,     BXX.N 0xE000 OR W,  ;
: WHILE,     IF, SWAP ;
: REPEAT,    AGAIN,  THEN, ;


EQ CONSTANT !=.
NE CONSTANT ==.	
HI CONSTANT U<=.
LS CONSTANT U>.
GE CONSTANT <.
LT CONSTANT >=.
GT CONSTANT <=.
LE CONSTANT >.


EXPORT

: ASMARM_BIG

	FF_ALIGN
	ALSO THUMB2_MOD
	0 TO IT_V SY
	0 TO :=COUNT
 ;
: T-ALIGN
  BEGIN  HERE 3 AND 
  WHILE 0xFF C, REPEAT ;

: 2ALIGN
    HERE 1 AND
  IF 0xFF C, THEN ;

: (CODE)
 ASMARM_BIG
  0 TO MOREPASS
  0 TO NPASS
  0 TO TEB_CLEAN
  CURRENT M@ M@ TO SAVELAST

  ITERNUM 2+ TO   ITERNUM

\+ HERE-TAB-CUR 	HERE-TAB-CUR @ HERE-TAB-CUR-SAVE !
\+ HERE-TAB-CUR 	SHERE-TAB-CUR @ SHERE-TAB-CUR-SAVE !
 POSTPONE [BEGIN]

 ;

: CODE
   2ALIGN
 0 TO PARM_HESH
 0 TO !!F
    CODL
   (CODE)  ;

0x10000000 VALUE EXEPTION_TABLE

: INTERR       ( addr -- )      
	T-ALIGN
    EXEPTION_TABLE +		                \ 
    HERE  1+    
    SWAP !                                \ 
    CODL
    (CODE)
;

;MODULE
\ ' THUMB2_MOD H.

\ RRX
