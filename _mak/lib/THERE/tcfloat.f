[IFNDEF] BREAK	: BREAK  POSTPONE EXIT POSTPONE THEN ; IMMEDIATE [THEN]

: ?_. ( addr len -- addr len flag )
\  F7_ED  2DUP TYPE
\ ." <?" 2DUP TYPE
  DUP 4 U< IF 0 BREAK
  2DUP
    1   0 <SIGN> \ DUP H.
\ NIP NIP  0 AND EXIT \ !!!!!
    >R
    16  0 <DIGITS> \ DUP H.
 2DROP
 MC@ ( DUP EMIT ) [CHAR] . =  \ DUP H.
 R> AND \ ." ?>"
;

: ?_E ( addr len -- addr len flag )
\  F7_ED  2DUP TYPE
\ ." <?" 2DUP TYPE
  2DUP + 1- MC@ [CHAR] E =
;

: ?FLOAT0 ( addr u -- bool )
\  F7_ED  2DUP TYPE
    1   0 <SIGN>    >R
    16  0 <DIGITS>  >R
    1   0 <DOT>     >R
    16  0 <DIGITS>  >R
    1   0 <EXP>     >R
    1   0 <SIGN>    >R
    4   0 <DIGITS>  >R
    NIP 0= \ После всего этого должен быть конец строки
    2R> 2R> 2R> R> AND
    AND AND AND AND AND
    AND
;

CREATE F_BOFF    99 ALLOT

: F1DIVX 1e 0 D>F F/  0 >R  RP@ SF! R> ;
: F2DIV  0 D>F 2e F/  0 >R  RP@ SF! R> ;

: >FLOAT0 ( addr u -- F: r true | false )
  2DUP ?FLOAT0
  IF  2DUP ?FLOAT 0=
    IF  F_BOFF $!   S" e"  F_BOFF $+!  F_BOFF COUNT
    THEN
\    CR ." FL=" 2DUP TYPE CR
    PAST-COMMA 0! FALSE ?IS-COMMA !
    OVER C@ DUP [CHAR] - =    \ addr u c flag
    IF DROP SKIP1 >FLOAT-ABS FNEGATE
    ELSE [CHAR] + = IF SKIP1 THEN
                    >FLOAT-ABS
    THEN
  ELSE
   2DROP 0
  THEN ;

: MFFFFF ( c-addr u -- f )
\    F7_ED
    0 >R
\    2DUP TYPE
    FABORT
    >FLOAT0
    IF         RP@ SF! R>
    ELSE  THROW
    THEN
;

: F. ( -- f )
\    F7_ED
    >R
\    2DUP TYPE
    FABORT
     RP@ SF@
    ?PRINT-EXP @
    PRINT-EXP
    [']  F. CATCH DROP
    [']   FABORT CATCH DROP
    ?PRINT-EXP ! \ SF.
    SPACE
    RDROP
;

: F+
  2>R
 RP@ SF@ RDROP
 RP@ SF@ F+
 RP@ SF! 
 R> ;

: F-
  >R >R
 RP@ SF@ RDROP
 RP@ SF@ F-
 RP@ SF! 
 R> ;

: F*
  2>R
 RP@ SF@ RDROP
 RP@ SF@ F*
 RP@ SF! 
 R> ;

: F/
  >R >R
 RP@ SF@ RDROP
 RP@ SF@ F/
 RP@ SF! 
 R> ;

