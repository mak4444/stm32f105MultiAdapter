
	Wortbirne6 0 "GET_MAIN"
	B GET_MAIN
	Wortbirne6 0 "GET_&UBAUDR"
	B GET_andUBAUDR
	Wortbirne6 0 "GET_MEID"
	B GET_MEID
	Wortbirne6 0 "GET_MENUID"
	B GET_MENUID
	Wortbirne6 0 "GET_VAR_END"
	B GET_VAR_END
	Wortbirne6 0 "GET_VAR_BIG"
	B GET_VAR_BIG
	Wortbirne6 0 "GET_HOOKEMIT"
	B GET_HOOKEMIT
	Wortbirne6 0 "GET_HOOKEMITQ"
	B GET_HOOKEMITQ
	Wortbirne6 0 "GET_HOOKKEY"
	B GET_HOOKKEY
	Wortbirne6 0 "GET_HOOKKEYQ"
	B GET_HOOKKEYQ
	Wortbirne6 0 "GET_HLD"
	B GET_HLD
	Wortbirne6 0 "GET_BASE"
	B GET_BASE
	Wortbirne6 0 "GET_PAD"
	B GET_PAD
	Wortbirne6 0 "GET_CONTEXT"
	B GET_CONTEXT
	Wortbirne6 0 "GET_CURRENT"
	B GET_CURRENT
	Wortbirne6 0 "GET_TIB"
	B GET_TIB
	Wortbirne6 0 "GET_NTIB"
	B GET_NTIB
	Wortbirne6 0 "GET_ATIB"
	B GET_ATIB
	Wortbirne6 0 "GET_IN"
	B GET_IN
	Wortbirne6 0 "GET_STATE"
	B GET_STATE
	Wortbirne6 0 "GET_DP"
	B GET_DP
	Wortbirne6 0 "GET_HANDLER"
	B GET_HANDLER
	Wortbirne6 0 "GET_SP0"
	B GET_SP0
	Wortbirne6 0 "GET_LAST"
	B GET_LAST
	Wortbirne6 0 "VAR_END"
	push {lr};BL DOCONST
	.word 0x2000848C 
	Wortbirne6 0 "FFMAIN"
	B FFMAIN
	Wortbirne6 0 "FMAIN"
	B FMAIN
	Wortbirne6 0 "MEID."
	B MEIDdot
	Wortbirne6 0 "<MAIN>"
	B lessMAINgreat
	Wortbirne6 0 "MMENU"
	B MMENU
	Wortbirne6 0 "mmmemu"
	B mmmemu
	Wortbirne6 0 "CHMEHU"
	B CHMEHU
	Wortbirne6 0 "KEYW"
	B KEYW
	Wortbirne6 0 "KEY0"
	B KEY0
	Wortbirne6 0 "TTTIME"
	push {lr};BL DOCONST
	.word 0x20008488 
	Wortbirne6 0 "CURUP"
	B CURUP
	Wortbirne6 0 ">MENU"
	B greatMENU
	Wortbirne6 0 "MENUS<"
	B MENUSless
	Wortbirne6 0 "MESAID"
	push {lr};BL DOCONST
	.word 0x20008484 
	Wortbirne6 0 "MENUW<"
	B MENUWless
	Wortbirne6 0 "MENU<"
	B MENUless
	Wortbirne6 0 "MENUID"
	push {lr};BL DOCONST
	.word 0x20008480 
	Wortbirne6 0 "SLCAN_LOOP"
	B SLCAN_LOOP
	Wortbirne6 0 "SLCANSPIN"
	B SLCANSPIN
	Wortbirne6 0 "SLCANGETLINE"
	B SLCANGETLINE
	Wortbirne6 0 "LPOS"
	push {lr};BL DOCONST
	.word 0x2000847C 
	Wortbirne6 0 "LINEBUF"
	push {lr};BL DOCONST
	.word 0x20008288 
	Wortbirne6 0 "USB<>UART"
	B USBlessgreatUART
	Wortbirne6 0 "VAR_ORIG_SET"
	B VAR_ORIG_SET
	Wortbirne6 0 "REST"
	B REST
	Wortbirne6 0 "SAVE"
	B SAVE
	Wortbirne6 0 "TRANS"
	B TRANS
	Wortbirne6 0 "-usb"
	B subusb
	Wortbirne6 0 "+usb"
	B addusb
	Wortbirne6 0 "usb-io"
	B usbsubio
	Wortbirne6 0 "usb-emit?"
	B usbsubemitque
	Wortbirne6 0 "CAN!"
	B CANsave
	Wortbirne6 0 "UBR!"
	B UBRsave
	Wortbirne6 0 "UBR_SET"
	B UBR_SET
	Wortbirne6 0 "USART2"
	push {lr};BL DOCONST
	.word 0x40004400 
	Wortbirne6 0 "ZCOUNT"
	B ZCOUNT
	Wortbirne6 0 "SCAN"
	B SCAN
	Wortbirne6 0 "[CHAR]_"
	B xCHARy_
	Wortbirne6 0 "CHAR_"
	B CHAR_
	Wortbirne6 0 "VARIABLE_"
	B VARIABLE_
	Wortbirne6 0 "CREATE_"
	B CREATE_
	Wortbirne6 0 "CONSTANT_"
	B CONSTANT_
	Wortbirne6 0 "V_DOCONST"
	push {lr};BL DOCONST
	.word 0x20008280 
	Wortbirne6 0 "V_DOVAR"
	push {lr};BL DOCONST
	.word 0x20008278 
	Wortbirne6 0 "POSTPONE_"
	B POSTPONE_
	Wortbirne6 0 "[COMPILE]_"
	B xCOMPILEy_
	Wortbirne6 0 "."_"
	B dotdtic_
	Wortbirne6 0 "S"_"
	B Sdtic_
	Wortbirne6 0 "SLIT,_"
	B SLITcom_
	Wortbirne6 0 "\_"
	B sl_
	Wortbirne6 0 ".("
	B dotc
	Wortbirne6 0 "(_"
	B c_
	Wortbirne6 0 "+LOOP_"
	B addLOOP_
	Wortbirne6 0 "LOOP_"
	B LOOP_
	Wortbirne6 0 "+LOOP,"
	B addLOOPcom
	Wortbirne6 0 "LOOP,"
	B LOOPcom
	Wortbirne6 0 "DO_"
	B DO_
	Wortbirne6 0 "?DO_"
	B queDO_
	Wortbirne6 0 "LEAVE_"
	B LEAVE_
	Wortbirne6 0 "RESOLVE-LEAVE_"
	B RESOLVEsubLEAVE_
	Wortbirne6 0 "REPEAT_"
	B REPEAT_
	Wortbirne6 0 "WHILE_"
	B WHILE_
	Wortbirne6 0 "ELSE_"
	B ELSE_
	Wortbirne6 0 "CS-SWAP"
	B CSsubSWAP
	Wortbirne6 0 "THEN_"
	B THEN_
	Wortbirne6 0 "IF_"
	B IF_
	Wortbirne6 0 "AHEAD_"
	B AHEAD_
	Wortbirne6 0 "AGAIN_"
	B AGAIN_
	Wortbirne6 0 "UNTIL_"
	B UNTIL_
	Wortbirne6 0 "BEGIN_"
	B BEGIN_
	Wortbirne6 0 "BRANCH,_"
	B BRANCHcom_
	Wortbirne6 0 "?BRANCH,_"
	B queBRANCHcom_
	Wortbirne6 0 "?BRANCH.F,_"
	B queBRANCHdotFcom_
	Wortbirne6 0 "BEQ.W,_"
	B BEQdotWcom_
	Wortbirne6 0 "FAR?"
	B FARque
	Wortbirne6 0 "BXX.W,_"
	B BXXdotWcom_
	Wortbirne6 0 "BXX.N"
	B BXXdotN
	Wortbirne6 0 "DUMP"
	B DUMP
	Wortbirne6 0 "PTYPE"
	B PTYPE
	Wortbirne6 0 ">PRT"
	B greatPRT
	Wortbirne6 0 ".0"
	B dot0
	Wortbirne6 0 ";_"
	B end_
	Wortbirne6 0 "?COMP"
	B queCOMP
	Wortbirne6 0 ":_"
	B dcoma_
	Wortbirne6 0 "NEST,_"
	B NESTcom_
	Wortbirne6 0 "EXIT,_"
	B EXITcom_
	Wortbirne6 0 "[_"
	B x_
	Wortbirne6 0 "]_"
	B y_
	Wortbirne6 0 "[']_"
	B xticy_
	Wortbirne6 0 "'_"
	B tic_
	Wortbirne6 0 "HEADER_"
	B HEADER_
	Wortbirne6 0 "IMMEDIATE_"
	B IMMEDIATE_
	Wortbirne6 0 "SHEADER_"
	B SHEADER_
	Wortbirne6 0 "SHEADER_0"
	B SHEADER_0
	Wortbirne6 0 "S",_"
	B Sdticcom_
	Wortbirne6 0 "HALIGN_"
	B HALIGN_
	Wortbirne6 0 "HEADER,_"
	B HEADERcom_
	Wortbirne6 0 "LINK,"
	B LINKcom
	Wortbirne6 0 "LAST"
	push {lr};BL DOCONST
	.word 0x20008270 
	Wortbirne6 0 "LALIGNED_"
	B LALIGNED_
	Wortbirne6 0 "HALIGNED_"
	B HALIGNED_
	Wortbirne6 0 "ALIGN_"
	B ALIGN_
	Wortbirne6 0 "ALIGNED_"
	B ALIGNED_
	Wortbirne6 0 "ORDER_"
	B ORDER_
	Wortbirne6 0 "VOC-NAME."
	B VOCsubNAMEdot
	Wortbirne6 0 "ID."
	B IDdot
	Wortbirne6 0 "SET-ORDER"
	B SETsubORDER
	Wortbirne6 0 "GET-ORDER"
	B GETsubORDER
	Wortbirne6 0 "GET-CURRENT"
	B GETsubCURRENT
	Wortbirne6 0 "SET-CURRENT"
	B SETsubCURRENT
	Wortbirne6 0 "HELLO"
	B HELLO
	Wortbirne6 0 "XXX"
	B XXX
	Wortbirne6 0 "QUIT"
	B QUIT
	Wortbirne6 0 "OK."
	B OKdot
	Wortbirne6 0 "SP0"
	push {lr};BL DOCONST
	.word 0x2000826C 
	Wortbirne6 0 "INTERPRET_"
	B INTERPRET_
	Wortbirne6 0 "?STACK"
	B queSTACK
	Wortbirne6 0 "?SLITERAL"
	B queSLITERAL
	Wortbirne6 0 "?SLITERAL0"
	B queSLITERAL0
	Wortbirne6 0 ">NUMBER"
	B greatNUMBER
	Wortbirne6 0 "THROW"
	B THROW
	Wortbirne6 0 "CATCH"
	B CATCH
	Wortbirne6 0 "HANDLER"
	push {lr};BL DOCONST
	.word 0x20008268 
	Wortbirne6 0 "DIGIT"
	B DIGIT
	Wortbirne6 0 "LITERAL_"
	B LITERAL_
	Wortbirne6 0 "2LITERAL_"
	B _2LITERAL_
	Wortbirne6 0 "COMPILE,_"
	B COMPILEcom_
	Wortbirne6 0 "NCOMPILE,"
	B NCOMPILEcom
	Wortbirne6 0 "LIT,_"
	B LITcom_
	Wortbirne6 0 "LMOVWT,"
	B LMOVWTcom
	Wortbirne6 0 "C,_"
	B Ccom_
	Wortbirne6 0 "W,_"
	B Wcom_
	Wortbirne6 0 ",_"
	B com_
	Wortbirne6 0 "HERE_"
	B HERE_
	Wortbirne6 0 "ALLOT_"
	B ALLOT_
	Wortbirne6 0 "STATE"
	push {lr};BL DOCONST
	.word 0x20008264 
	Wortbirne6 0 "SFIND"
	B SFIND
	Wortbirne6 0 "2NIP"
	B _2NIP
	Wortbirne6 0 "SEARCH-WORDLIST"
	B SEARCHsubWORDLIST
	Wortbirne6 0 "NAME>F"
	B NAMEgreatF
	Wortbirne6 0 "NAME>"
	B NAMEgreat
	Wortbirne6 0 "UCOMPARE"
	B UCOMPARE
	Wortbirne6 0 "UPC"
	B UPC
	Wortbirne6 0 "COMPARE"
	B COMPARE
	Wortbirne6 0 "REFILL"
	B REFILL
	Wortbirne6 0 "QUERY"
	B QUERY
	Wortbirne6 0 "PARSE_"
	B PARSE_
	Wortbirne6 0 "SkipUpTo"
	B SkipUpTo
	Wortbirne6 0 "PARSE-NAME"
	B PARSEsubNAME
	Wortbirne6 0 "ParseWord"
	B ParseWord
	Wortbirne6 0 "SkipWord"
	B SkipWord
	Wortbirne6 0 "OnNotDelimiter"
	B OnNotDelimiter
	Wortbirne6 0 "SkipDelimiters"
	B SkipDelimiters
	Wortbirne6 0 "OnDelimiter"
	B OnDelimiter
	Wortbirne6 0 "GetChar"
	B GetChar
	Wortbirne6 0 "PeekChar"
	B PeekChar
	Wortbirne6 0 "CharAddr"
	B CharAddr
	Wortbirne6 0 "IsDelimiter"
	B IsDelimiter
	Wortbirne6 0 "EndOfChunk"
	B EndOfChunk
	Wortbirne6 0 "SOURCE!"
	B SOURCEsave
	Wortbirne6 0 "SOURCE"
	B SOURCE
	Wortbirne6 0 ">IN"
	push {lr};BL DOCONST
	.word 0x20008260 
	Wortbirne6 0 "ATIB"
	push {lr};BL DOCONST
	.word 0x2000825C 
	Wortbirne6 0 "#TIB"
	push {lr};BL DOCONST
	.word 0x20008258 
	Wortbirne6 0 "TIB"
	push {lr};BL DOCONST
	.word 0x2000818C 
	Wortbirne6 0 "ACCEPT"
	B ACCEPT
	Wortbirne6 0 "WORDS_"
	B WORDS_
	Wortbirne6 0 "PPP"
	B PPP
	Wortbirne6 0 "COTT"
	B COTT
	Wortbirne6 0 "DEFINITIONS_"
	B DEFINITIONS_
	Wortbirne6 0 "FORTH_"
	B FORTH_
	Wortbirne6 0 "CONTEXT-SIZE"
	push {lr};BL DOCONST
	.word 0x10 
	Wortbirne6 0 "CR"
	B CR
	Wortbirne6 0 "H."
	B Hdot
	Wortbirne6 0 "U."
	B Udot
	Wortbirne6 0 "."
	B dot
	Wortbirne6 0 "S>D"
	B SgreatD
	Wortbirne6 0 "D."
	B Ddot
	Wortbirne6 0 "SPACE"
	B SPACE
	Wortbirne6 0 "(D.)"
	B cDdotcend
	Wortbirne6 0 "SIGN"
	B SIGN
	Wortbirne6 0 "#>"
	B ngreat
	Wortbirne6 0 "#S"
	B nS
	Wortbirne6 0 "#"
	B n
	Wortbirne6 0 "<#"
	B lessn
	Wortbirne6 0 "HOLD"
	B HOLD
	Wortbirne6 0 "DECIMAL_"
	B DECIMAL_
	Wortbirne6 0 "HEX_"
	B HEX_
	Wortbirne6 0 "PAD"
	push {lr};BL DOCONST
	.word 0x2000817C 
	Wortbirne6 0 "HLD"
	push {lr};BL DOCONST
	.word 0x20008078 
	Wortbirne6 0 "zzz"
	B zzz
	Wortbirne6 0 "XXX"
	push {lr};BL DOCONST
	.word 0x55 
	Wortbirne6 0 "VXXC"
	push {lr};BL DOCONST
	.word 0x2000804C 
	Wortbirne6 0 "VXXX"
	push {lr};BL DOCONST
	.word 0x20008048 
	Wortbirne6 0 "TYPE"
	B TYPE
	Wortbirne6 0 "(S")"
	B cSdticcend
	Wortbirne6 0 "IALIGNED"
	B IALIGNED
	Wortbirne6 0 "HALIGNED"
	B HALIGNED
	Wortbirne6 0 "DABS"
	B DABS
	Wortbirne6 0 "HH."
	B HHdot
	Wortbirne6 0 "KEY?"
	B KEYque
	Wortbirne6 0 "KEY"
	B KEY
	Wortbirne6 0 "EMIT"
	B EMIT
	Wortbirne6 0 "HOOK-KEY?"
	push {lr};BL DOCONST
	.word 0x2000800C 
	Wortbirne6 0 "HOOK-EMIT?"
	push {lr};BL DOCONST
	.word 0x20008008 
	Wortbirne6 0 "UMOD"
	B UMOD
	Wortbirne6 0 "U/"
	B Ursl
	Wortbirne6 0 "UM/MOD"
	B UMrslMOD
	Wortbirne6 0 "D0="
	B D0equ
	Wortbirne6 0 "D2*"
	B D2mul
	Wortbirne6 0 "DOCONST"
	B DOCONST
	Wortbirne6 0 "DOCREATE"
	B DOCREATE
	Wortbirne6 0 "(LEAVE)"
	B cLEAVEcend
	Wortbirne6 0 "(+LOOP)"
	B caddLOOPcend
	Wortbirne6 0 "(LOOP)"
	B cLOOPcend
	Wortbirne6 0 "(?DO)"
	B cqueDOcend
	Wortbirne6 0 "(DO)"
	B cDOcend
	Wortbirne6 0 "K"
	B K
	Wortbirne6 0 "J"
	B J
	Wortbirne6 0 "I"
	B I
	Wortbirne6 0 "CAN_BR_SET"
	B CAN_BR_SET
	Wortbirne6 0 "UBAUDR!"
	B UBAUDRsave
	Wortbirne6 0 "SLCAN_DECODE"
	B SLCAN_DECODE
	Wortbirne6 0 "CAN_CNT"
	B CAN_CNT
	Wortbirne6 0 "SLCAN_RXSPIN"
	B SLCAN_RXSPIN
	Wortbirne6 0 "FLASH-ERASE"
	B FLASHsubERASE
	Wortbirne6 0 "FLASH!"
	B FLASHsave
	Wortbirne6 0 "HHP"
	B HHP
	Wortbirne6 0 "usb-key"
	B usbsubkey
	Wortbirne6 0 "usb-key?"
	B usbsubkeyque
	Wortbirne6 0 "usb-emit"
	B usbsubemit
	Wortbirne6 0 "SERIAL-KEY"
	B SERIALsubKEY
	Wortbirne6 0 "SERIAL-KEY?"
	B SERIALsubKEYque
	Wortbirne6 0 "SERIAL-EMIT?"
	B SERIALsubEMITque
	Wortbirne6 0 "SERIAL-EMIT"
	B SERIALsubEMIT
	Wortbirne6 0 "CMOVE>"
	B CMOVEgreat
	Wortbirne6 0 "W2CMOVE"
	B W2CMOVE
	Wortbirne6 0 "CMOVE"
	B CMOVE
	Wortbirne6 0 "WFILL"
	B WFILL
	Wortbirne6 0 "FILL"
	B FILL
	Wortbirne6 0 "BOUNDS"
	B BOUNDS
	Wortbirne6 0 "COUNT"
	B COUNT
	Wortbirne6 0 "BIT"
	B BIT
	Wortbirne6 0 "<<"
	B lessless
	Wortbirne6 0 "LSHIFT"
	B LSHIFT
	Wortbirne6 0 ">>"
	B greatgreat
	Wortbirne6 0 "RSHIFT"
	B RSHIFT
	Wortbirne6 0 "ARSHIFT"
	B ARSHIFT
	Wortbirne6 0 "PICK"
	B PICK
	Wortbirne6 0 "-ROT"
	B subROT
	Wortbirne6 0 "ROT"
	B ROT
	Wortbirne6 0 "NIP"
	B NIP
	Wortbirne6 0 "TUCK"
	B TUCK
	Wortbirne6 0 "2OVER"
	B _2OVER
	Wortbirne6 0 "OVER"
	B OVER
	Wortbirne6 0 "2DRRR"
	B _2DRRR
	Wortbirne6 0 "2DROP"
	B _2DROP
	Wortbirne6 0 "2SWAP"
	B _2SWAP
	Wortbirne6 0 "SWAP"
	B SWAP
	Wortbirne6 0 "2DUP"
	B _2DUP
	Wortbirne6 0 "DUP"
	B DUP
	Wortbirne6 0 "?DUP"
	B queDUP
	Wortbirne6 0 "M*"
	B Mmul
	Wortbirne6 0 "UM*"
	B UMmul
	Wortbirne6 0 "U/MOD"
	B UrslMOD
	Wortbirne6 0 "/MOD"
	B rslMOD
	Wortbirne6 0 "MOD"
	B MOD
	Wortbirne6 0 "/"
	B rsl
	Wortbirne6 0 "*"
	B mul
	Wortbirne6 0 "EVEN"
	B EVEN
	Wortbirne6 0 "2/"
	B _2rsl
	Wortbirne6 0 "2*"
	B _2mul
	Wortbirne6 0 "CELLS"
	B CELLS
	Wortbirne6 0 "1-"
	B _1sub
	Wortbirne6 0 "2-"
	B _2sub
	Wortbirne6 0 "CELL-"
	B CELLsub
	Wortbirne6 0 "CHAR+"
	B CHARadd
	Wortbirne6 0 "1+"
	B _1add
	Wortbirne6 0 "2+"
	B _2add
	Wortbirne6 0 "CELL+"
	B CELLadd
	Wortbirne6 0 "DU<"
	B DUless
	Wortbirne6 0 "D-"
	B Dsub
	Wortbirne6 0 "-"
	B sub
	Wortbirne6 0 "D+"
	B Dadd
	Wortbirne6 0 "+"
	B add
	Wortbirne6 0 "DNEGATE"
	B DNEGATE
	Wortbirne6 0 "NEGATE"
	B NEGATE
	Wortbirne6 0 "NOT"
	B NOT
	Wortbirne6 0 "INVERT"
	B INVERT
	Wortbirne6 0 "BIT@"
	B BITload
	Wortbirne6 0 "CXOR!"
	B CXORsave
	Wortbirne6 0 "XOR!"
	B XORsave
	Wortbirne6 0 "COR!"
	B CORsave
	Wortbirne6 0 "HBIS!"
	B HBISsave
	Wortbirne6 0 "WOR!"
	B WORsave
	Wortbirne6 0 "OR!"
	B ORsave
	Wortbirne6 0 "BIS!"
	B BISsave
	Wortbirne6 0 "XOR"
	B XOR
	Wortbirne6 0 "OR"
	B OR
	Wortbirne6 0 "AND!"
	B ANDsave
	Wortbirne6 0 "CANDC!"
	B CANDCsave
	Wortbirne6 0 "HBIC!"
	B HBICsave
	Wortbirne6 0 "WANDC!"
	B WANDCsave
	Wortbirne6 0 "ANDC!"
	B ANDCsave
	Wortbirne6 0 "BIC!"
	B BICsave
	Wortbirne6 0 "ANDC"
	B ANDC
	Wortbirne6 0 "AND"
	B AND
	Wortbirne6 0 "U<"
	B Uless
	Wortbirne6 0 "U>"
	B Ugreat
	Wortbirne6 0 "0<"
	B _0less
	Wortbirne6 0 "<"
	B less
	Wortbirne6 0 ">"
	B great
	Wortbirne6 0 "0<>"
	B _0lessgreat
	Wortbirne6 0 "<>"
	B lessgreat
	Wortbirne6 0 "0="
	B _0equ
	Wortbirne6 0 "="
	B equ
	Wortbirne6 0 "0MAX"
	B _0MAX
	Wortbirne6 0 "UMAX"
	B UMAX
	Wortbirne6 0 "MAX"
	B MAX
	Wortbirne6 0 "UMIN"
	B UMIN
	Wortbirne6 0 "ABS"
	B ABS
	Wortbirne6 0 "MIN"
	B MIN
	Wortbirne6 0 "1-!"
	B _1subsave
	Wortbirne6 0 "1+!"
	B _1addsave
	Wortbirne6 0 "C+!"
	B Caddsave
	Wortbirne6 0 "+!"
	B addsave
	Wortbirne6 0 "2@"
	B _2load
	Wortbirne6 0 "2!"
	B _2save
	Wortbirne6 0 "ON"
	B ON
	Wortbirne6 0 "OFF"
	B OFF
	Wortbirne6 0 "0!"
	B _0save
	Wortbirne6 0 "4C!"
	B _4Csave
	Wortbirne6 0 "!"
	B save
	Wortbirne6 0 "REG!"
	B REGsave
	Wortbirne6 0 "2C!"
	B _2Csave
	Wortbirne6 0 "W!"
	B Wsave
	Wortbirne6 0 "H!"
	B Hsave
	Wortbirne6 0 "WREG!"
	B WREGsave
	Wortbirne6 0 "C!"
	B Csave
	Wortbirne6 0 "4C@"
	B _4Cload
	Wortbirne6 0 "@"
	B load
	Wortbirne6 0 "REG@"
	B REGload
	Wortbirne6 0 "2C@"
	B _2Cload
	Wortbirne6 0 "W@"
	B Wload
	Wortbirne6 0 "H@"
	B Hload
	Wortbirne6 0 "WREG@"
	B WREGload
	Wortbirne6 0 "C@"
	B Cload
	Wortbirne6 0 "EXECUTE"
	B EXECUTE
	Wortbirne6 0 "@EXECUTE"
	B loadEXECUTE
	Wortbirne6 0 "PERFORM"
	B PERFORM
	Wortbirne6 0 "SP!"
	B SPsave
	Wortbirne6 0 "SP@"
	B SPload