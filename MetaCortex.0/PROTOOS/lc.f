
MODULE: TC

: DOES>
  S" (DOES>)" EVALUATE 
\+ ';EXIT  NEST,
  S" R>" EVALUATE
\- ';EXIT   S" 1-" EVALUATE
; IMMEDIATE

0 VALUE 'DOTVALUE

: VALUE 
   DP M@ THERE? 0= IF
  VALUE 
  BREAK
 2ALIGN
 1 IALLOT	HEADER -1 IALLOT
\+ ';EXIT   NEST,
  'DOTVALUE
\+ ';EXIT TCOMPILE,
\- ';EXIT T_COMPILE,
  D-DP M@ 3 + 3 INVERT AND D-DP M!
DROP \  D-DP M@ !
  D-DP M@ I,
  4 ALLOT
;  

0 VALUE 'DOTVECT

: VECT
 ?OLD   VECT
\  abort
 2ALIGN
 1 IALLOT	HEADER -1 IALLOT
\+ ';EXIT   NEST,
  'DOTVECT
\+ ';EXIT TCOMPILE,
\- ';EXIT T_COMPILE,
  D-DP M@ 3 + 3 INVERT AND D-DP M!
\  D-DP M@ !
  D-DP M@ I,
  4 ALLOT
;  

;MODULE