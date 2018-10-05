
MODULE: TC


MVARIABLE TLAST ( --- addr)
\ This variable holds a pointer to the last definition created.

: TLINK, ( -> ) 
	TLAST M@ I,
	HERE
	TLAST M! ;

: THEADER, ( cfa -- )
	T-ALIGN
	0 ,	\ flags
	,	\ cfa
	TLINK,
;

: TIMM
 1 TLAST M@ CELL- CELL- CELL- ! ;


: #THEADER ( cfa "<spaces>name" -- )
	DUP THERE? 0= IF -9 THROW THEN
	THEADER,
	PARSE-NAME
	S", ;

: #CONSTANT
  2ALIGN
\+ ';EXIT  HERE 3 AND IF $FFFF W, THEN
  HERE 1 OR
\+ ';EXIT	NEST,
  SWAP	TC-LIT,
\- ';EXIT	$4770	W, \	BX	R14
\+ ';EXIT	EXIT,
 #THEADER
;

: :#
 >IN M@ ' SWAP
 >IN M! #THEADER ;

: CONSTANT#
 >IN M@ INTERPRET SWAP
 >IN M! #CONSTANT ;

: VARIABLE# CONSTANT# ;



: TVOC:
  HERE 1+
  0xb500  W,  \           push    {lr}
 S" ' DO-VOC" EVALUATE TCOMPILE,
  HERE 0 I,  
  SWAP #THEADER 
  TLAST M@ SWAP !
;

: DOES>
  S" (DOES>)" EVALUATE 
\+ ';EXIT  NEST,
  S" R>" EVALUATE
\- ';EXIT  S" 1-" EVALUATE
; IMMEDIATE


;MODULE

REQUIRE 'DOTVECT MetaCortex/PROTOOS/lc.f
