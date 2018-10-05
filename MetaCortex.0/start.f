
MODULE: THUMB2_MOD

[IFDEF] ><DP ><DP [THEN]

:    DSTK   R7 ;     \ Data Stack
:    TOS    R6 ;     \ Top Of Stack
:    RSTK   SP ;     \ Return Stack


: DPUSH,  [[ DSTK ## -4  ]] !!  STR,
;
: DPOP,   [[ DSTK ]] ## 4   LDR,
 ;

: RPUSH, \ [[ RSTK ## -4  ]] !!  STR, 
;
: RPOP, \  [[ RSTK ]] ## 4   LDR,
 ;


\ : DROP, 0x1B04F854 , ;  \  LDR		TOS,    [DSTK], #4
\ : DROP, 0xCC20  W, ;  \     LDMIA	R4!, {R5}
: DROP, DSTK !!  {{ TOS }}  LDMIA, ;


P: DPOP, 
P: DPUSH, 
P: RPOP, 
P: RPUSH, 

[IFDEF] ><DP ><DP [THEN]

\ #define U0BASE 0xe000c000

;MODULE

[IFDEF] ><DP

[THEN]

CR .( here6=) here H.


