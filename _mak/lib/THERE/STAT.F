

[IFNDEF] ENUM : ENUM ( n -- n+1) DUP CONSTANT 1+ ; [THEN]

1
ENUM #C!
ENUM #C@
ENUM #W!
ENUM #W@
ENUM #!
ENUM #@
ENUM #EXECUTE
ENUM #ERR
ENUM #H.
ENUM #E
ENUM #KEY
ENUM #KEY?
ENUM #CR
ENUM #EXEC
ENUM #START
ENUM #ERROR
ENUM #KS
ENUM #WDOG
ENUM #HERE
VALUE #MAX


0x20000 VALUE T-ORG                \ ��砫�� 楫���� ���� ������ ����
0x21000 VALUE D-ORG                \ ��砫�� 楫���� ���� ������ ������
0x01000 VALUE CODE-SIZE            \ ������ ������ ����
0x01000 VALUE DATA-SIZE            \ ������ ������ ������
VARIABLE T-DP                 \ �����㬥�⠫�� 㪠��⥫� ����
VARIABLE D-DP                 \ 楫���� 㪠��⥫� ������

\ T-ORG CODE-SIZE +
0 VALUE MAX_HERE

\ DIS-OPT

: [F] ALSO FORTH ; IMMEDIATE
: [P]  PREVIOUS ; IMMEDIATE

: TC_?LIMIT
   DUP 
 MAX_HERE
   U< 
 0= 
 if  
\ ABORT" The code limit has reached!"
 ." The code limit has reached!"
\ f7_ed
 then
 ;
