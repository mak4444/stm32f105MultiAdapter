
REQUIRE THUMB2_MOD ~mak\ARM7\SRC\thumb2.f
REQUIRE NUMBER? ~mak\LIB\fpcnum.f 
\ REQUIRE MTOKEN ~mak/CinF/MTOKEN.F

[IFNDEF]   SCAN
: SCAN ( adr len char -- adr' len' )
\ Scan for char through addr for len, returning addr' and len' of char.
        >R 2DUP R> -ROT
        OVER + SWAP
        ?DO DUP I C@ =
                IF LEAVE
                ELSE >R 1 -1 D+ R>
                THEN
        LOOP DROP ;
[THEN]

[IFNDEF] SMUDGE

: PASS\N
\ CR ."  PASS\N=<"
  BEGIN PARSE-NAME DUP 0=
  WHILE 2DROP REFILL 0= IF TRUE  EXIT THEN
  REPEAT DROP TIB - \ 2DUP H. H. - DUP H. ." >" 
  >IN !  FALSE ;

: MTOKEN ( TABL -- ADDR N )
  PASS\N
\ + TRACE  ." PASS\N=<"  DUP  h. ." >"

  IF DROP CharAddr  0  EXIT THEN
  DUP >R COUNT PeekChar SCAN NIP
\ + TRACE  ." PeekChar SCAN NIP=<"  DUP  h. ." >"
  IF RDROP CharAddr 1 DUP >IN +! EXIT THEN
  CharAddr
  BEGIN 1 >IN +!
     EndOfChunk
     IF  TRUE
     ELSE   R@ COUNT PeekChar 
\ + TRACE 1 emit dup emit
   SCAN NIP
     THEN 
  UNTIL   CharAddr OVER -
\ + TRACE
\  ." CharAddr OVER - =<"  2DUP type ." >"

  RDROP
;
[THEN]


\- MW@ : MW@ W@ ;

MODULE: THUMB2_MOD


\ VECT LEX_EXECUTE ' EXECUTE TO LEX_EXECUTE
 : LEX_EXECUTE EXECUTE ;

0 VALUE ?LEX
0 VALUE ?,

CREATE -ASMARM_TABL HERE
  S"  =,#[]{}-!"  S",
  0x9 C, CHAR " C,
HERE OVER - 1- SWAP C!

CREATE ASMARM_TABL HERE
  S"  =,#[]{}!"  S",
  0x9 C, CHAR " C,
HERE OVER - 1- SWAP C!

[IFNDEF]  M@ :  M@  @ ; [THEN]
[IFNDEF] MC@ : MC@ C@ ; [THEN]

: ASM_INTERPRET ( -> )
\ + TRACE 
\ ." ASM_INTERPRET=<"  SOURCE TYPE ." >"
  TRUE  TO ?LEX
  BEGIN 
\ CR ." PARSE-NAME=<"
     PARSE-NAME  SWAP TIB
- \ 2DUP H. H. - dup  H.
  >IN M!
\ ." |"  >IN M@   PARSE-NAME TYPE >IN M!  ." >"

 0= \    SkipDelimiters  EndOfChunk
\ + TRACE  ." EndOfChunk=<"  dup h. ." >"
     IF 0 0 
     ELSE PeekChar
\ ." PeekChar=<"  dup h. DUP EMIT ." >"
 [CHAR] - =
 IF ASMARM_TABL ELSE -ASMARM_TABL THEN
\ + TRACE
\  ." >MTOKEN=<"  2DUP h. h. ." >"
 MTOKEN
\ + TRACE
\  ." MTOKEN=<"  2DUP TYPE ." >"
     THEN  DUP
  WHILE
\ + TRACE
\ cr ." <[" 2DUP  TYPE ." ]>"
	SFIND ?DUP
    IF
         STATE M@ =
        IF COMPILE, ELSE LEX_EXECUTE THEN
    ELSE

\         S" NOTFOUND" SFIND 
\         IF EXECUTE
\         ELSE 2DROP
 ?SLITERAL
\ THEN
    THEN
    ?STACK
  REPEAT 2DROP
  FALSE  TO ?LEX
 0 TO ?, ;

CREATE ITBUF.W 11 ALLOT

: tail_INTERPRET ( -> )
\ CR ." S=" SOURCE TYPE
  BEGIN  PARSE-NAME DUP
  WHILE
	IT_V 0x1F AND
	IF \  CR ." IT=" 2DUP TYPE   SOURCE DROP H.
  2DUP + 2-
\   DUP H. CR
 MW@
\  DUP H.
  0x4000 OR ( DUP H.)  0x772E  ( .w ) = \ DUP .
		IF 4 - ITBUF.W $! S" .W" ITBUF.W $+! ITBUF.W  COUNT 
		ELSE DUP 2 > IF 2- THEN
		THEN \  2DUP TYPE CR

	THEN

	SFIND  ?DUP
    IF
         STATE M@ =
        IF COMPILE, ELSE LEX_EXECUTE THEN
    ELSE
         S" NOTFOUND" SFIND 
         IF EXECUTE
         ELSE 2DROP ?SLITERAL THEN
    THEN
    ?STACK
  REPEAT 2DROP
;

0 VALUE ?DOADSP

: DOADSP ( -- )
  TRUE TO ?DOADSP
  tail_INTERPRET
  ?DOADSP 0= IF BREAK
  BEGIN  REFILL ?DOADSP AND
  WHILE  tail_INTERPRET
  REPEAT ;

: ASM_LEX
   ['] DOADSP  CATCH
 FALSE TO ?LEX ?DUP
 IF THROW THEN ;

: H HH ;
: S S! ;
: { {{ ;
0 VALUE ?MCR
: }  ?MCR IF NIP 0 TO ?MCR BREAK }} ;
: ;  ?LEX 0=  IF	POSTPONE ;	BREAK POSTPONE	\ ; IMMEDIATE
: [  ?LEX 0=  IF POSTPONE	[	BREAK [[ ; IMMEDIATE
: ]  ?LEX 0=  IF		]	BREAK ]] ;
: !  ?LEX 0=  IF		!	BREAK !! ;
: #  ?LEX 0=  IF		#	BREAK ## ;
: ,  ?LEX 0=  IF		,	THEN -1 TO ?, ;
: @  ?LEX 0=  IF		@ 	BREAK POSTPONE	\ ;
: =  ?LEX 0=  IF 		=	BREAK := ;
: //  POSTPONE	\ ; IMMEDIATE

: ASM_END
  ASM_END
 MOREPASSFLG 0= IF FALSE TO ?DOADSP THEN
;

: END-CODE ASM_END ;

: END  PARSE-NAME 
 S" -CODE"  COMPARE THROW  END-CODE ;

EXPORT


: ASMARM_BIG ASMARM_BIG ASM_LEX ;

: CODE 
    CODE ASM_LEX  ;

;MODULE
