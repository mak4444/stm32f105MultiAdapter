
WINAPI: SetCommState           KERNEL32.DLL
WINAPI: GetCommState           KERNEL32.DLL
WINAPI: SetCommTimeouts        KERNEL32.DLL
WINAPI: GetCommTimeouts        KERNEL32.DLL
WINAPI: EscapeCommFunction	KERNEL32.DLL
WINAPI: PurgeComm               KERNEL32.DLL

(*
        int   DCBlength
        int   BaudRate
        int   BinaryBits                \ a 32bit cell of bit fields
      1 bits  fBinary                   \ define the bit fields
      1 bits  fParity
      1 bits  fOutxCtsFlow
      1 bits  fOutxDsrFlow             \ SDI changed for Rts to Dsr
      2 bits  fDtrControl
      1 bits  fDtrSensitivity
      1 bits  fTXContinueOnXoff
      1 bits  fOutX
      1 bits  fInx
      1 bits  fErrorChar
      1 bits  fNull
      2 bits  fRtsControl
      1 bits  fAbortOnError
     17 bits  fDummy
        short wReserved
        short XonLim
        short XoffLim
        byte  ByteSize
        byte  Parity
        byte  StopBits
        byte  XonChar
        byte  XoffChar
        byte  ErrorChar
        byte  EofChar
        byte  EvtChar
        short wReserved1

*)
1	CONSTANT fBinary              \ binary mode, no EOF check   	
2	CONSTANT fParity              \ enable parity checking      
4	CONSTANT fOutxCtsFlow         \ CTS output flow control     
8	CONSTANT fOutxDsrFlow         \ DSR output flow control     
0x10	CONSTANT fDtrControl          \ DTR flow control type       
0x20	CONSTANT fDsrSensitivity      \ DSR sensitivity             
0x40	CONSTANT fTXContinueOnXoff  \ XOFF continues Tx           
0x80	CONSTANT fOutX            \ XON/XOFF out flow control       
0x100	CONSTANT  fInX             \ XON/XOFF in flow control        
0x200	CONSTANT fErrorChar       \ enable error replacement        
0x400	CONSTANT fNull            \ enable null stripping           
0x800	CONSTANT fRtsControl      \ RTS flow control                
0x1000	CONSTANT fAbortOnError    \ abort reads/writes on error     


0
 CELL FIELD DCBlength            \ sizeof(DCB)                 
 CELL FIELD BaudRate             \ current baud rate           
 CELL FIELD BinaryBits          \ a 32bit cell of bit fields
 \ fBinary              \ binary mode, no EOF check   
 \ fParity              \ enable parity checking      
 \ fOutxCtsFlow         \ CTS output flow control     
 \ fOutxDsrFlow         \ DSR output flow control     
 \ fDtrControl          \ DTR flow control type       
 \ fDsrSensitivity      \ DSR sensitivity             

 \ fTXContinueOnXoff  \ XOFF continues Tx           
 \ fOutX            \ XON/XOFF out flow control       
 \ fInX             \ XON/XOFF in flow control        
 \ fErrorChar       \ enable error replacement        
 \ fNull            \ enable null stripping           
 \ fRtsControl      \ RTS flow control                
 \ fAbortOnError    \ abort reads/writes on error     
 \ CELL FIELD fDummy2          \ reserved                        

    2 FIELD wReserved         \ not currently used              
    2 FIELD XonLim            \ transmit XON threshold          
    2 FIELD XoffLim           \ transmit XOFF threshold         
    1 FIELD ByteSize          \ number of bits/byte, 4-8        
    1 FIELD Parity            \ 0-4=no,odd,even,mark,space      
    1 FIELD StopBits          \ 0,1,2 = 1, 1.5, 2               
    1 FIELD XonChar           \ Tx and Rx XON character         
    1 FIELD XoffChar          \ Tx and Rx XOFF character        
  
    1 FIELD ErrorChar         \ error replacement character     
    1 FIELD EofChar           \ end of input character          
    1 FIELD EvtChar           \ received event character        
    2 FIELD wReserved1        \ reserved  do not use
CONSTANT \DCB


0
CELL FIELD  ReadIntervalTimeout
CELL FIELD  ReadTotalTimeoutMultiplier
CELL FIELD  ReadTotalTimeoutConstant
CELL FIELD  WriteTotalTimeoutMultiplier
CELL FIELD  WriteTotalTimeoutConstant
CONSTANT \COMMTIMEOUTS

CREATE CommTimeOuts \COMMTIMEOUTS ALLOT
CREATE CommTimeOuts_S \COMMTIMEOUTS ALLOT

[IFDEF] PPC

-1 CommTimeOuts ReadIntervalTimeout !
 0 CommTimeOuts ReadTotalTimeoutMultiplier !
 0 CommTimeOuts ReadTotalTimeoutConstant !
 1 CommTimeOuts WriteTotalTimeoutMultiplier !
20 CommTimeOuts WriteTotalTimeoutConstant !

[ELSE]

10 CommTimeOuts ReadIntervalTimeout !
 1 CommTimeOuts ReadTotalTimeoutMultiplier !
\ значений этих тайм - аутов вполне хватает для уверенного приема 
\ даже на скорости 110 бод 
100 CommTimeOuts ReadTotalTimeoutConstant !
\ используется в данном случае как время ожидания посылки 
0 CommTimeOuts WriteTotalTimeoutMultiplier !
0 CommTimeOuts WriteTotalTimeoutConstant !

[THEN]

CREATE LPDCB \DCB ALLOT
CREATE LPDCB_S \DCB ALLOT
\       LPDCB \DCB ERASE

0 VALUE COMPORT_ID

0x40000000 CONSTANT FILE_FLAG_OVERLAPPED

: COMM_OPEN
  DROP >R
  0
\ FILE_FLAG_OVERLAPPED 
  FILE_ATTRIBUTE_ARCHIVE ( template attrs )
  OPEN_EXISTING
  0 ( secur )
  0 ( share )  
  R/W ( access=fam )
  R> ( filename )
  CreateFileA DUP -1 = IF GetLastError ELSE 0 THEN
  THROW TO COMPORT_ID ;

 S" COM.LOG" R/W CREATE-FILE THROW VALUE COMM_LOGid


\ DCB       LPDCB DCBlength !
\ 153600       LPDCB BaudRate  !
\ 115000       LPDCB BaudRate  !
\  76800       LPDCB BaudRate  !
  56000       LPDCB BaudRate  !
\  38400       LPDCB BaudRate  !
\  19200       LPDCB BaudRate  !
\   9600       LPDCB BaudRate  !
\   4800       LPDCB BaudRate  !
8          LPDCB ByteSize C!
1          LPDCB StopBits C!
0          LPDCB   Parity C!

1 CONSTANT PURGE_TXABORT
2 CONSTANT PURGE_RXABORT
8 CONSTANT PURGE_RXCLEAR
4 CONSTANT PURGE_TXCLEAR


: COMM-SET
         LPDCB COMPORT_ID  SetCommState	0=  THROW
  CommTimeOuts COMPORT_ID  SetCommTimeouts	0=  THROW
\ Устанавливаем размеры внутренних буферов драйвера (лучше больше) 64К
\	0x10000 0x10000  COMPORT_ID SetupComm 0= THROW
\  PURGE_TXABORT PURGE_RXABORT OR PURGE_RXCLEAR OR PURGE_TXCLEAR OR 
\  COMPORT_ID  PurgeComm  0= THROW
;

: FLLL
1110 CommTimeOuts ReadIntervalTimeout !
1110 CommTimeOuts ReadTotalTimeoutMultiplier !
\ значений этих тайм - аутов вполне хватает для уверенного приема 
\ даже на скорости 110 бод 
1110 CommTimeOuts ReadTotalTimeoutConstant !
\ используется в данном случае как время ожидания посылки 
1110 CommTimeOuts WriteTotalTimeoutMultiplier !
1110 CommTimeOuts WriteTotalTimeoutConstant !

  CommTimeOuts COMPORT_ID  SetCommTimeouts	0=  THROW
;

: LLL  FLLL BYE ;

: COMM-SET_S
         LPDCB_S COMPORT_ID  SetCommState		0=  THROW
  CommTimeOuts_S COMPORT_ID  SetCommTimeouts	0=  THROW
;

: TO_COM_  ( ADDR LEN -- )
\ 1 \  EV_RXCHAR 
\ 4 \ EV_TXEMPTY
\  COMPORT_ID SetCommMask 0= THROW
           COMPORT_ID
 WRITE-FILE
 DROP ;

: TO_COMN ( ADDR LEN -- )
   TO_COM_ LT 1 TO_COM_ ;

CREATE SSS 42 ALLOT

0 VALUE VCOM-CNT

: COM-CNT 
  VCOM-CNT 0=
  IF
\ 1 \  EV_RXCHAR 
\ 4 \ EV_TXEMPTY
\  COMPORT_ID SetCommMask 0= THROW
 SSS 1 COMPORT_ID READ-FILE DROP TO VCOM-CNT
  THEN VCOM-CNT
;

: COMPORT_ERR0  ( n -- )
    DUP 0= IF GetLastError ELSE DROP 0 THEN 
    THROW
;
\
\ Очистка буферов и прекращение всех обменов
\
: COMPORT_CLEAR ( -- )
    \ COMPORT_ID 0= IF COMPORT_ERRMSG EXIT THEN
    \ COMPORT_CHECK ?EXIT
    PURGE_TXABORT PURGE_RXABORT OR PURGE_RXCLEAR OR PURGE_TXCLEAR OR 
    COMPORT_ID PurgeComm COMPORT_ERR0
;

: COM-LAST SSS C@ ;

: COM-GET
  BEGIN COM-CNT
  UNTIL 0 TO VCOM-CNT COM-LAST
;

: UKEY
 BEGIN
    COM-CNT  KEY? 0= AND
 UNTIL     COM-GET
 ;

: COM.
 BEGIN
    COM-CNT  KEY? 0= AND
 WHILE
     COM-GET H.
 REPEAT ;

 CREATE UTTT   0  , 0 ,

: UEMIT ( ." U=" DUP H. ) UTTT C! UTTT 1 TO_COM_ \ 1 PAUSE
;

 : TO_COM_S 0 DO COUNT UEMIT ( 1 PAUSE )  LOOP DROP ;
 : TO_COM TO_COM_ ;

1 CONSTANT SETXOFF
2 CONSTANT SETXON
3 CONSTANT SETRTS
4 CONSTANT CLRRTS
5 CONSTANT SETDTR
6 CONSTANT CLRDTR
8 CONSTANT SETBREAK
9 CONSTANT CLRBREAK

: +DTR ( -- )    SETDTR COMPORT_ID EscapeCommFunction DROP ;
: -DTR ( -- )    CLRDTR COMPORT_ID EscapeCommFunction DROP ;

: +RTS ( -- )    SETRTS COMPORT_ID EscapeCommFunction DROP ;
: -RTS ( -- )    CLRRTS COMPORT_ID EscapeCommFunction DROP ;

: +BREAK ( -- )  SETBREAK COMPORT_ID EscapeCommFunction DROP ;
: -BREAK ( -- )  CLRBREAK COMPORT_ID EscapeCommFunction DROP ;

: B? \ F7_ED
 SWAP
 BinaryBits
 @
 AND
 H.
 ;

: COM-STAT.
CR ." DCBlength         =" LPDCB DCBlength           ?
CR ." BaudRate          =" LPDCB BaudRate            ?
CR ." BinaryBits        =" LPDCB BinaryBits          @ H.
CR ." fBinary           =" LPDCB fBinary             B?
CR ." fParity           =" LPDCB fParity             B?
CR ." fOutxCtsFlow      =" LPDCB fOutxCtsFlow        B?
CR ." fOutxDsrFlow      =" LPDCB fOutxDsrFlow        B?
CR ." fDtrControl       =" LPDCB fDtrControl         B?
CR ." fDsrSensitivity   =" LPDCB fDsrSensitivity     B?                                           
CR ." fTXContinueOnXoff =" LPDCB fTXContinueOnXoff   B?
CR ." fOutX             =" LPDCB fOutX               B?
CR ." fInX              =" LPDCB fInX                B?
CR ." fErrorChar        =" LPDCB fErrorChar          B?
CR ." fNull             =" LPDCB fNull               B?
CR ." fRtsControl       =" LPDCB fRtsControl         B?
CR ." fAbortOnError     =" LPDCB fAbortOnError       B?
\ CR ." fDummy2           =" LPDCB fDummy2             B?
CR ." wReserved         =" LPDCB wReserved              W@ .
CR ." XonLim            =" LPDCB XonLim                 W@ .
CR ." XoffLim           =" LPDCB XoffLim                W@ .
CR ." ByteSize          =" LPDCB ByteSize               C@ .
CR ." Parity            =" LPDCB Parity                 C@ .
CR ." StopBits          =" LPDCB StopBits               C@ .
CR ." XonChar           =" LPDCB XonChar                C@ .
CR ." XoffChar          =" LPDCB XoffChar               C@ .
\ CR ."                   =" LPDCB                         
CR ." ErrorChar         =" LPDCB ErrorChar              C@ .
CR ." EofChar           =" LPDCB EofChar                C@ .
CR ." EvtChar           =" LPDCB EvtChar                C@ .
CR ." wReservedC1       =" LPDCB  wReserved1            W@ .

;
\EOF

0 =" LPDCB fDsrSensitivity !
0 =" LPDCB fTXContinueOnXoff !
0 =" LPDCB fOutX             !
0 =" LPDCB fInX              !
0 =" LPDCB fErrorChar        !
0 =" LPDCB fNull             !
0 =" LPDCB fRtsControl       !
0 =" LPDCB fAbortOnError     !
0 =" LPDCB fDummy2           !

