
[UNDEFINED] T>
[IF] 

0 VALUE  TH_H-
\ : [>T] TH_H- + ; : [>T]  ; IMMEDIATE

: T> 
   TC_?LIMIT
 TH_H-
 -
  ;

[THEN]

: &T_C! ( c addr -- ) 
    T>
 C!
 ;

: &T_C@ (  addr -- c ) 
    T> C@ ;

: &T_! ( c addr -- ) 
    T>
 !
 ;

: &T_+! ( c addr -- ) 
    T> +! ;


: &T_@ (  addr -- c ) 
  T> @ ;

: &T_2! ( c addr -- ) 
    T> 2! ;

: &T_2@ (  addr -- c ) 
    T> 2@ ;

: &T_W! ( c addr -- ) 
    T> W! ;

0 VALUE &T_@?

: &T_W@ (  addr -- c ) 
  DUP TO &T_@?
    T> W@ ;

REQUIRE T_@ _mak/lib/THERE/there.f

: TREAD-FILE  2>R T> 2R> READ-FILE ;
: TWRITE-FILE 2>R T> 2R> WRITE-FILE ;

: T-MINIT
\ 4 HERE 3 AND - ALLOT \ 
\ F7_ED
 ." T-INIT="
 T-ORG
 CODE-SIZE DATA-SIZE +  DUP H.  $20000 +
 ALLOCATE
 THROW  2DUP H. H.
 - dup h. TO TH_H-
 T-ORG T-DP M!
 D-ORG D-DP M!
 T-ORG CODE-SIZE + TO MAX_HERE
;

: T-INIT
  T-MINIT
 T-ORG T> CODE-SIZE 0xFF FILL ;

: RE-ORG ( taddr -- )
  DUP T-ORG T> - TO TH_H- TO T-ORG ;


0xFF CELLS CONSTANT TEB_SIZE

CREATE TEXEC_BUF ' ABORT DUP , , TEB_SIZE ALLOT
CREATE TEXEC_KEY       0 , 0 , TEB_SIZE ALLOT


: &T_EXECUTE
\  F7_ED
  TEXEC_KEY CELL-
  BEGIN CELL+
 2DUP M@ 
 2DUP U>
 ABORT" BAD TEXECUTE"
 =
 UNTIL
	NIP
	TEXEC_KEY -
	TEXEC_BUF 
+
 M@
\ dup rest
\ EXECUTE
 DUP THERE? IF T_EXECUTE BREAK MEXECUTE
;


: RMEM_MODE
['] &T_C@ TO T_C@
['] &T_W@ TO T_W@
['] &T_@  TO T_@
['] &T_2@ TO T_2@
;

: MEM_MODE
['] &T_C! TO T_C!
['] &T_W! TO T_W!
['] &T_!  TO T_!
['] &T_2! TO T_2!
['] &T_EXECUTE TO T_EXECUTE
['] NOOP  TO MAIN_S
  RMEM_MODE

 ;

MEM_MODE

