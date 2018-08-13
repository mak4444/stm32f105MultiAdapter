
0 VALUE COMPORT_ID

: COMM_OPEN
  R/W OPEN-FILE
  THROW TO COMPORT_ID
;

: TO_COM_  ( ADDR LEN -- )
           COMPORT_ID WRITE-FILE DROP
;

\- LT CREATE LT $D C,  $A C,

: TO_COMN ( ADDR LEN -- )
   TO_COM_ LT 1 TO_COM_ ;



: COM-CNT ( -- n )
  COMPORT_ID PORT?

;


: COM-GET
  COMPORT_ID PORT@
;


: UKEY
 BEGIN
    COM-CNT KEY? 0= AND
 UNTIL     COM-GET
 ;

: COM.
 BEGIN
    COM-CNT  KEY? 0= AND
 WHILE
     COM-GET EMIT \ H.
 REPEAT ;

 CREATE UTTT   0  , 0 ,

: UEMIT ( ." U=" DUP H. ) UTTT C! UTTT 1 TO_COM_ \ 1 PAUSE
;


: TO_COM_S
 0 ?DO COUNT UEMIT
 ( 1 PAUSE )  LOOP
 DROP ;


: TO_COM TO_COM_S ;

