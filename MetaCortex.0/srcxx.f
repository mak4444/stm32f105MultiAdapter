\ 0x08020000 CONSTANT FW_TADR
 0x08000000 CONSTANT FW_TADR

CREATE MSECADRTAB

0x08000000 , \ Sector 0, 16 Kbytes
0x08004000 , \ Sector 1, 16 Kbytes
0x08008000 , \ Sector 2, 16 Kbytes
0x0800C000 , \ Sector 3, 16 Kbytes
0x08010000 , \ Sector 4, 64 Kbytes
0x08020000 , \ Sector 5, 128 Kbytes
0x08040000 , \ Sector 6, 128 Kbytes
0x08060000 , \ Sector 7, 128 Kbytes
0x08080000 , \ Sector 8, 128 Kbytes
0x080A0000 , \ Sector 9, 128 Kbytes
0x080C0000 , \ Sector 10, 128 Kbytes
0x080E0000 , \ Sector 11, 128 Kbytes

\  Base address of the Flash sectors Bank 2 
0x08100000 , \ Sector 0, 16 Kbytes
0x08104000 , \ Sector 1, 16 Kbytes
0x08108000 , \ Sector 2, 16 Kbytes
0x0810C000 , \ Sector 3, 16 Kbytes
0x08110000 , \ Sector 4, 64 Kbytes
0x08120000 , \ Sector 5, 128 Kbytes
0x08140000 , \ Sector 6, 128 Kbytes
0x08160000 , \ Sector 7, 128 Kbytes
0x08180000 , \ Sector 8, 128 Kbytes
0x081A0000 , \ Sector 9, 128 Kbytes
0x081C0000 , \ Sector 10, 128 Kbytes
0x081E0000 , \ Sector 11, 128 Kbytes

7 CONSTANT RESET_SECTOR

RESET_SECTOR 8 *  CONSTANT  RESET_SEGMENT

MSECADRTAB RESET_SECTOR  CELLS + @ CONSTANT  RESET_ADDR

0 VALUE ROM_END

 [DEFINED] GUI-CONSOLE
 [IF]  : FJB GUI-CONSOLE::FlushJetBuf ;
 [ELSE] : FJB ;
 [THEN]      

REQUIRE TC_?LIMIT _mak/lib/THERE/STAT.F

0x20020000  TO D-ORG		\ Начальный целевой адрес области данных

[IFNDEF] \+

: \+    POSTPONE [DEFINED] 0= IF POSTPONE \ THEN ; IMMEDIATE
: \-    POSTPONE [DEFINED]    IF POSTPONE \ THEN ; IMMEDIATE

[THEN]      

