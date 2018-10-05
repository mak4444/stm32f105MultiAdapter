
: A7\ POSTPONE \ ; IMMEDIATE
: A9\ ; IMMEDIATE
: MC! C! ;
: MC@ C@ ;
: MW! W! ;
: MW@ W@ ;
:  M!  ! ;
:  M@  @ ; 
: M0! 0! ;
: M+! +! ;
:  M+  + ; 
: M-/ / ;
: MNEGATE NEGATE ; 
: MOVER OVER ;
: MAND AND ;
: MCR CR ;
: MH. H. ;
: MHERER H. ;
: M_. . ;
: M.S .S ;
: MDEPTH DEPTH ;
: MHEX HEX ;
: MKEY KEY ;
: MKEY? KEY? ;
: MEXECUTE EXECUTE ;
: MWDS WORDS ;
: MCMOVE CMOVE ;

\ : MCHAR CHAR ;

: SYNONYM  HERE >R DP ! HEADER R> DP !
\- SMUDGE  LAST @ CURRENT @ !
 ;


: MREAD-FILE READ-FILE ;

\- AHEAD : AHEAD  HERE BRANCH, >MARK 2 ; IMMEDIATE

\ HERE 100000 ALLOCATE THROW DP !

\ REQUIRE CASE-INS	lib/ext/caseins.f
\ REQUIRE { _mak/locals4.f
REQUIRE MTOKEN _mak/CinF/MTOKEN.F
[IFNDEF] BREAK : BREAK  POSTPONE EXIT POSTPONE THEN ; IMMEDIATE [THEN]

[IFNDEF] /*
: /*  ( -- )
  BEGIN
    PARSE-NAME DUP 0=
    IF  NIP  REFILL   0= IF DROP TRUE THEN
    ELSE  S" */" COMPARE 0=  THEN
  UNTIL
; IMMEDIATE
[THEN]

\- .S : .S  DEPTH 0 MAX .SN ;

\ REQUIRE GETXY win/_mak/lib/getxy.f
\ + EKEY? REQUIRE GKEY _mak/lib/gkey.4

\ 0 VALUE SSBB

\ REQUIRE <SIGN>	lib/include/float0.f

\ VECT	LOADER_SYNCHRONIZE
\ VECT	LOADER_LOAD
\ VECT	LOADER_GO

\ REQUIRE >NAME _mak/lib/fpc.f 


\ REQUIRE MFFFFF	_mak/lib/THERE/tcfloat.f
