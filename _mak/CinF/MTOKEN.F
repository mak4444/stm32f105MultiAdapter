\ REQUIRE { ~mak\locals4.f

C" SCAN" FIND NIP 0=
[IF]
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
  BEGIN PARSE-NAME DUP 0=
  WHILE 2DROP REFILL 0= IF TRUE  EXIT THEN
  REPEAT NEGATE >IN +! DROP  FALSE ;

: MTOKEN ( CTABL -- ADDR LEN )
  >R
  PASS\N
  IF CharAddr  0  EXIT THEN
  R@ ( CTABL) COUNT PeekChar SCAN NIP 
  IF CharAddr 1 DUP >IN +! EXIT THEN
  CharAddr
  BEGIN 1 >IN +!
     EndOfChunk 
     IF  TRUE
     ELSE R@ ( CTABL) COUNT  PeekChar SCAN NIP 
     THEN
  UNTIL   CharAddr OVER - \ ." MT=<" 2DUP TYPE ." >"
  RDROP
;
\EOF
: SMTOKEN { TABL Tlen -- ADDR alen }
  PASS\N
  IF CharAddr  0  EXIT THEN
  TABL Tlen PeekChar SCAN NIP 
  IF CharAddr 1 DUP >IN +! EXIT THEN
  CharAddr
  BEGIN 1 >IN +!
     EndOfChunk 
     IF  TRUE
     ELSE   TABL Tlen  PeekChar SCAN NIP 
     THEN
  UNTIL   CharAddr OVER - \ ." MT=<" 2DUP TYPE ." >"
;

\EOF TEST
CREATE ZZZ HERE
  S"  ~!@#$%^&*()+|{}:<>?`-=\[];',./"  S",
  0x9 C, CHAR " C,
HERE OVER - SWAP C!

\ CREATE ZZZ HERE 0 C, BL C, 0x9 C, CHAR , C,
\ HERE OVER - SWAP C!
: TEST BEGIN ZZZ MTOKEN DUP 0= IF EXIT THEN DUP .
 ." {" TYPE KEY DROP ." }" AGAIN ;
TEST &+ QQQQ,WWWW EEEE
RRR$	TTTTTT
YYYYYYYYY UUUUU
IIII OOOOOO
