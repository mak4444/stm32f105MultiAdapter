
REQUIRE $! ~mak\place.f

CREATE XSOURCE 0x101 ALLOT
0 VALUE X>IN
0 VALUE XDP
VARIABLE XFP 0 ,
VARIABLE XCURSTR

: [BEGIN]
\  CR ." [BEGIN]=<" 
 >IN @ TO X>IN
  DP @ TO XDP
  SOURCE XSOURCE $!
  SOURCE-ID FILE-POSITION
 DROP
\  2DUP H. H.
  XFP 2!
  CURSTR @ XCURSTR ! \  ." >"
; IMMEDIATE

: [AGAIN]
\ CR ." [AGAIN]=<" 
   XSOURCE COUNT DUP #TIB ! TIB SWAP MOVE
   XCURSTR @ CURSTR !
   X>IN ( DUP H.)  >IN  !
   XDP   DP   !
   XFP 2@  SOURCE-ID REPOSITION-FILE DROP
\ ." |"  SOURCE TYPE  SOURCE-ID FILE-POSITION H. H. H.  ." >"
; IMMEDIATE

: [UNTIL]
  IF POSTPONE [AGAIN] THEN
; IMMEDIATE


\EOF test

0 VALUE XN

: 2: :
  1 TO XN
 POSTPONE [BEGIN]
 ;

: 2;
 XN IF
   XN 1- TO XN
   POSTPONE [AGAIN]
   EXIT
 THEN
 POSTPONE ; ; IMMEDIATE

2: HI   ." Hello!!"
 [ CR .( PASS=) XN . ]
2;

: HA   ." HoHo!!"
 [ CR .( PASS=) XN . ]
;

2: HU   ." HUUU!!"
 [ CR .( PASS=) XN . ]
2;

HI HA
