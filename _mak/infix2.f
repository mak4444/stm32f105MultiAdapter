REQUIRE [IF] ~mak/CompIF.f
REQUIRE [IFNDEF] ~nn\lib\ifdef.f
REQUIRE $+! ~mak/place.f

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

: PASS\N
  BEGIN  SkipDelimiters  EndOfChunk
  WHILE REFILL 0= IF TRUE  EXIT THEN
  REPEAT      FALSE ;

: MTOKEN ( TABL -- ADDR N )
  PASS\N
  IF DROP CharAddr  0  EXIT THEN
  DUP >R COUNT PeekChar SCAN NIP
  IF RDROP CharAddr 1 DUP >IN +! EXIT THEN
  CharAddr
  BEGIN 1 >IN +!
     EndOfChunk
     IF  TRUE
     ELSE   R@ COUNT PeekChar SCAN NIP
     THEN
  UNTIL   CharAddr OVER -
  RDROP
;

MODULE: _INF_MOD

CREATE OP HERE 0x20 CELLS ALLOT  HERE SWAP !  \ STACK OF OPERATIONS
CREATE OP_BUF 0x101 ALLOT

: >OP  ( A -- ) 0 CELL - OP +! OP @ !  ;
:  OP@ ( -- A )
   OP @ @   ;
:  OP> ( -- A )  [ OP @ ] LITERAL @ OP @ =
ABORT" BRACKET IS EXPECTED"
   OP@  CELL OP +! ;

: >OP> ( N -- )   \ N IS PRIORITY
        DUP >R
        BEGIN OP@ > 0=
        WHILE OP> DROP
              OP>  COUNT OP_BUF $+! BL OP_BUF $C+!
               R@
        REPEAT RDROP ;

:  #2-OP ( N -- )  \ N IS PRIORITY
     CREATE IMMEDIATE LAST @ , ,
     DOES>  2@  >R
            DUP >R >OP>
           R> R>  >OP >OP ;

:   2-OP ( N -- )  \ N IS PRIORITY
    #2-OP ;

: 1-OP 10 2-OP ;

WARNING 0!
   3 2-OP OR  3 2-OP XOR  4 2-OP AND    5 2-OP =
   6 2-OP <   6 2-OP >
   7 2-OP +   7 2-OP -
   8 2-OP *   8 2-OP /    8 2-OP MOD

: ( 0 >OP  ; IMMEDIATE
: ) 1 >OP>  OP> DROP ; IMMEDIATE
TRUE WARNING !

;MODULE


: _INF_
  C"  }"  MTOKEN DROP C@ [CHAR] { <> ABORT" ожидается {"
  [ ALSO _INF_MOD ] POSTPONE ( OP_BUF 0! [ PREVIOUS ]
  BEGIN   C"  ~!@#$%^&*()+|{}:<>?`-=\[];',./" \ символы разделители
          MTOKEN DUP 
          IF  OVER  C@  [CHAR] }  = IF DROP 0 THEN
          THEN  DUP
  WHILE
     2DUP ['] _INF_MOD >BODY @ SEARCH-WORDLIST
    IF   NIP NIP EXECUTE
    ELSE _INF_MOD::OP_BUF $+! BL _INF_MOD::OP_BUF $C+!
    THEN
    ?STACK
  REPEAT 2DROP
  [ ALSO _INF_MOD ] POSTPONE ) [ PREVIOUS ]
   _INF_MOD::OP_BUF COUNT
; IMMEDIATE


CREATE _INF_BUFF 100 ALLOT
FALSE  VALUE  _INF_FLAG

: NOTFOUND
  2DUP 2>R ['] NOTFOUND CATCH ?DUP
  IF _INF_FLAG IF THROW THEN
      DROP 2DROP
     S"  _INF_  { " _INF_BUFF $!
            2R>     _INF_BUFF $+!
             S"  }" _INF_BUFF $+!
    _INF_BUFF COUNT
    TRUE TO _INF_FLAG ['] EVALUATE CATCH
   FALSE TO _INF_FLAG  THROW
  ELSE 2R> 2DROP
  THEN
;

\ TEST

4+FIVE*(TTT+8) TYPE
