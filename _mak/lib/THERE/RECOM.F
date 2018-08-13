REQUIRE THERE? ~mak/lib/THERE/there.f 

WARNING @ WARNING 0!

\ : READ-FILE 2>R DUP THERE? IF 2R> TREAD-FILE BREAK 2R> READ-FILE ;


: , ( x -- ) \ 94
\ 
  DP
 M@
 4
 ALLOT
 !
 ;

: I, , ;


: C, ( char -- ) \ 94
\ 
  DP M@
 1 ALLOT
 C!
 ;


: W, ( word -- )
\ 
  DP M@ 2 ALLOT
 W!
 ;

: +! ( N ADDR -- )	DUP @ ROT + SWAP ! ;

: 1+! ( ADDR -- )	1 SWAP +! ;

: MCOUNT COUNT ;
: COUNT DUP 1+ SWAP C@ ;

\- >PRT : >PRT  DUP BL U< IF DROP [CHAR] . THEN   ;
\- >PRT+ : >PRT+  >PRT  DUP $80 and IF DROP [CHAR] . THEN   ;


: PTYPE
  0 DO COUNT  >PRT+  EMIT LOOP DROP ;


: MDUMP ( addr u -- ) DUMP ;

: DUMP ( addr u -- ) \ 94 TOOLS
  DUP 0= IF 2DROP EXIT THEN
  BASE M@ >R HEX
  15 + 16 U/ 0 DO
\    CR DUP 4 .0 SPACE
    CR DUP H.
    SPACE DUP 16 0
      DO I 4 MOD 0= IF SPACE THEN
        DUP
        C@
\        2 .0
0 <# # # #> TYPE
 SPACE 1+
      LOOP SWAP   16  PTYPE
  LOOP DROP R> BASE M!
;

WARNING !

