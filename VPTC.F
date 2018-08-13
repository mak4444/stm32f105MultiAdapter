FLOAD VForthM/vfmneed.4 

 '  PPERR PLUG PERR

\- H. : H.   BASE @ HEX SWAP U. BASE ! ;

: GRENAME ( addr len -- addr1 len1 flg )
\         2DUP S" 2DROP" UCOMPARE 0= IF 2DROP S" ddrop_vektor" -1 BREAK
\         2DUP S" .S" UCOMPARE 0= IF 2DROP S" dots" -1 BREAK
 0
;

VOCABULARY HS_VOC

VARIABLE HSSSS
VARIABLE ZSSSS

: ?HHS
 HERE           CELL- @ 
 HERE HSSSS @ + CELL- @  <>
IF CR
   HERE           CELL- @ H.
   HERE HSSSS @ + CELL- @ H.
   -1  ABORT"  HSSSS "
THEN
;

: HS? ( adr -- adr flag )
 DUP HSSSS @ + @
 OVER @ =  ;

fload MetaCortex/src.f 
DECIMAL
S" SRC/global.4"  INCLUDED
 S" MetaCortex/start.f"  INCLUDED


 T-ALIGN
  HERE ZSSSS  !   0 HSSSS  !
0x20014000 D-DP !
0x11223344 , 0x55667788 , ?HHS

' INCLUDED TO INCLUDED_X
ALSO HS_VOC DEFINITIONS
fload promram.4 
PREVIOUS DEFINITIONS
 T-ALIGN

 ZSSSS M@ HERE
MCR .( ZS=) 2DUP MH. MH.
 - HSSSS M!
\ $10000 allot
0x20008000 dup D-DP M!
0x20014000 - to  vsymptom

0x11223344 , 0x55667788 , ?HHS

\ EOF

 ' INCLUDED_AL TO INCLUDED_X

fload promram.4 

S" SSSWWW.S" SWRD
