.syntax unified
.cpu cortex-m3
.thumb


@ Makros zum Aufbau des Dictionarykette.
@ Macros for building dictionary chain.
.macro Wortbirne6 Flags, Name

        .p2align 1        @ Auf gerade Adressen ausrichten  Align to even locations

        .hword \Flags     @ Flags setzen, diesmal 2 Bytes ! Wir haben Platz und Ideen :-)  Flag field, 2 bytes, space for ideas left !
9:      .word 9f+4        @ Link einfügen  Insert Link

        .byte 8f - 7f     @ Länge des Namensfeldes berechnen  Calculate length of name field
7:      .ascii "\Name"    @ Namen anfügen  Insert name string
8:      .p2align 1        @ 1 Bit 0 - Wieder gerade machen  Realign

.endm

.macro setvar Name,Vval
	. = o_d_forth + \Name
	.word \Vval
.endm

.macro bssvar Name,Vval
	.word \Vval
.endm

.macro bssvarz Name,Vval
	.word \Vval
	.word 0,0,0,0 //  , 0,0,0,0, 0,0,0,0, 0,0,0,0
.endm


@	.global QWERTY
@QWERTY:

	.global ForthWords
PForthWords:

	Wortbirne6 0 "SLCAN_LOOP"
	B SLCAN_LOOP
	Wortbirne6 0 "CAN_BR_SET"
	b CAN_BR_SET
	Wortbirne6 0 "CAN!"
	b CANsave

	Wortbirne6 0 "MENUID"
	B GET_MENUID
	Wortbirne6 0 "MENU<"
	B MENUless
	Wortbirne6 0 ">MENU"
	B greatMENU
	Wortbirne6 0 "CURUP"
	B CURUP
	Wortbirne6 0 "CHMEHU"
	B CHMEHU
	Wortbirne6 0 "MMENU"
	B MMENU

	Wortbirne6 0 "REST"
	B REST

	Wortbirne6 0 "SAVE"
	B SAVE

	.global RAMIMG
	Wortbirne6 0 "RAMIMG"
RAMIMG:
	push {lr};BL DOCONST
	.word __flash_buf

	Wortbirne6 0 "MEID"
	B GET_MEID

	Wortbirne6 0 "VAR_ORIG_SET"
	B VAR_ORIG_SET

	Wortbirne6 0 "FLASH!"
	B FLASHsave

	Wortbirne6 0 "FLASH-ERASE"
	B FLASHsubERASE

	Wortbirne6 0 "flash_unlock"
	B flash_unlock

	Wortbirne6 0 "flash_lock"
	B flash_lock

	Wortbirne6 0 "VARIABLE"
	B VARIABLE_
	Wortbirne6 0 "CREATE"
	B CREATE_
	Wortbirne6 0 "CONSTANT"
	B CONSTANT_

	Wortbirne6 0 "SLIT,"
	B SLITcom_

	Wortbirne6 1 "S\""
	B Sdtic_

	Wortbirne6 1 ".\""
	B dotdtic_

	Wortbirne6 1 "\\"
	B sl_

	Wortbirne6 1 "("
	B c_
	Wortbirne6 1 ".("
	B dotc
	Wortbirne6 0 "PARSE"
	B PARSE_
	Wortbirne6 1 "IF"
	B IF_
	Wortbirne6 1 "ELSE"
	B ELSE_
	Wortbirne6 0 "CS-SWAP"
	B CSsubSWAP
	Wortbirne6 1 "THEN"
	B THEN_
	Wortbirne6 1 "AHEAD"
	B AHEAD_
	Wortbirne6 1 "AGAIN"
	B AGAIN_
	Wortbirne6 1 "UNTIL"
	B UNTIL_
	Wortbirne6 1 "BEGIN"
	B BEGIN_
	Wortbirne6 1 "WHILE"
	B WHILE_
	Wortbirne6 1 "REPEAT"
	B REPEAT_

	Wortbirne6 1 "+LOOP"
	B addLOOP_
	Wortbirne6 1 "LOOP"
	B LOOP_
	Wortbirne6 1 "DO"
	B DO_
	Wortbirne6 1 "?DO"
	B queDO_
	Wortbirne6 1 "LEAVE"
	B LEAVE_


	Wortbirne6 1 ";"
	B end_
	Wortbirne6 0 ":"
	B dcoma_
	Wortbirne6 0 "NEST,"
	B NESTcom_
	Wortbirne6 0 "EXIT,_"
	B EXITcom_
	Wortbirne6 1 "["
	B x_
	Wortbirne6 0 "]"
	B y_

	Wortbirne6 0 "DUMP"
	B DUMP
	Wortbirne6 0 "DEFINITIONS"
	B DEFINITIONS_
	Wortbirne6 0 "COTT"
	B COTT
	Wortbirne6 0 "FFMAIN"
	B FFMAIN
	Wortbirne6 0 "FMAIN"
	B FMAIN
	Wortbirne6 0 "<MAIN>"
	B lessMAINgreat
	Wortbirne6 0 "TRANS"
	B TRANS
	Wortbirne6 0 "-usb"
	B subusb
	Wortbirne6 0 "+usb"
	B addusb
	Wortbirne6 0 "usb-io"
	B usbsubio
	Wortbirne6 0 "usb-emit"
	B usbsubemit
	Wortbirne6 0 "usb-emit?"
	B usbsubemitque
	Wortbirne6 0 "usb-key"
	B usbsubkey
	Wortbirne6 0 "usb-key?"
	B usbsubkeyque

	Wortbirne6 0 "UBAUDR!"
	B UBAUDRsave
	Wortbirne6 0 "UBR_SET"
	B UBR_SET

	Wortbirne6 0 "UBR!"
	B UBRsave

	Wortbirne6 0 "&UBAUDR"
	B GET_andUBAUDR

	.global TXBUF
	Wortbirne6 0 "TXBUF"
TXBUF:
	push {lr};BL DOCONST
	.word txbuf

	.global TXBUF
	Wortbirne6 0 "RC-END"
RC_END:
	push {lr};BL DOCONST
	.word __fini_array_end

	.global SAVE_Q
	Wortbirne6 0 "SAVE_Q"
SAVE_Q:
	STR	r6, [R7, #-4]!
	ldr r0, = __first_run_flg
	ldr r6,[r0]
	bx	lr	
.ltorg


	Wortbirne6 0 "SLCAN_RXSPIN"
	B SLCAN_RXSPIN

	Wortbirne6 0 "SLCAN_RXSPIN"
	B SLCAN_RXSPIN

	Wortbirne6 0 "CAN_CNT"
	B CAN_CNT

	Wortbirne6 0 "FLASH-ACR"
	push {lr};BL DOCONST
	.word 0x40022000
	Wortbirne6 0 "FLASH"
	push {lr};BL DOCONST
	.word 0x40022000
	Wortbirne6 0 "RCC-APB1ENR"
	push {lr};BL DOCONST
	.word 0x4002101C
	Wortbirne6 0 "RCC-CFGR"
	push {lr};BL DOCONST
	.word 0x40021004
	Wortbirne6 0 "RCC-CR"
	push {lr};BL DOCONST
	.word 0x40021000
	Wortbirne6 0 "RCC"
	push {lr};BL DOCONST
	.word 0x40021000
	Wortbirne6 0 "USART1-BRR"
	push {lr};BL DOCONST
	.word 0x40013808
	Wortbirne6 0 "USART1"
	push {lr};BL DOCONST
	.word 0x40013800
/*
	Wortbirne6 0 "hwid"
	B hwid
	Wortbirne6 0 "bit"
	B bit
*/
	Wortbirne6 0 "[']"
	B xticy_
	Wortbirne6 1 "[CHAR]"
	B xCHARy_

	Wortbirne6 0 "CHAR"
	B CHAR_

	Wortbirne6 0 "'"
	B tic_
	Wortbirne6 0 "HEADER"
	B HEADER_
	Wortbirne6 0 "IMMEDIATE"
	B IMMEDIATE_
	Wortbirne6 0 "SHEADER"
	B SHEADER_
	Wortbirne6 0 "SHEADER_0"
	B SHEADER_0
	Wortbirne6 0 "S\","
	B Sdticcom_
	Wortbirne6 0 "HALIGN"
	B HALIGN_
	Wortbirne6 0 "HEADER,"
	B HEADERcom_
	Wortbirne6 0 "LINK,"
	B LINKcom

	Wortbirne6 0 "VAR_BIG"
	B GET_VAR_BIG
	Wortbirne6 0 "VAR_END"
	B GET_VAR_END

	Wortbirne6 0 "LAST"
	B GET_LAST
	Wortbirne6 0 "LALIGNED"
	B LALIGNED_
	Wortbirne6 0 "HALIGNED"
	B HALIGNED_
	Wortbirne6 0 "ALIGN"
	B ALIGN_
	Wortbirne6 0 "ALIGNED"
	B ALIGNED_
	Wortbirne6 0 "ORDER"
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
	Wortbirne6 0 "SP0"
	B GET_SP0
	Wortbirne6 0 "INTERPRET"
	B INTERPRET_
	Wortbirne6 0 "?STACK"
	B queSTACK
	Wortbirne6 0 "?SLITERAL"
	B queSLITERAL
	Wortbirne6 0 ">NUMBER"
	B greatNUMBER
	Wortbirne6 0 "THROW"
	B THROW
	Wortbirne6 0 "CATCH"
	B CATCH
	Wortbirne6 0 "HANDLER"
	B GET_HANDLER
	Wortbirne6 0 "DIGIT"
	B DIGIT
	Wortbirne6 0 "LITERAL"
	B LITERAL_
	Wortbirne6 0 "2LITERAL"
	B _2LITERAL_
	Wortbirne6 0 "COMPILE,"
	B COMPILEcom_
	Wortbirne6 1 "[COMPILE]"
	B xCOMPILEy_
	Wortbirne6 1 "POSTPONE"
	B POSTPONE_
	Wortbirne6 0 "NCOMPILE,"
	B NCOMPILEcom
	Wortbirne6 0 "LIT,"
	B LITcom_
	Wortbirne6 0 "LMOVWT,"
	B LMOVWTcom
	Wortbirne6 0 "C,"
	B Ccom_
	Wortbirne6 0 "W,"
	B Wcom_
	Wortbirne6 0 ","
	B com_
	Wortbirne6 0 "HERE"
	B HERE_
	Wortbirne6 0 "ALLOT"
	B ALLOT_
	Wortbirne6 0 "DP"
	B GET_DP
	Wortbirne6 0 "STATE"
	B GET_STATE
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
	Wortbirne6 0 "OK."
	B OKdot
	Wortbirne6 0 "QUERY"
	B QUERY
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
	B GET_IN
	Wortbirne6 0 "ATIB"
	B GET_ATIB
	Wortbirne6 0 "#TIB"
	B GET_NTIB
	Wortbirne6 0 "TIB"
	B GET_TIB
	Wortbirne6 0 "ACCEPT"
	B ACCEPT
	Wortbirne6 0 "WORDS"
	B WORDS_
	Wortbirne6 0 "PPP"
	B PPP
	Wortbirne6 0 "FORTH"
	B FORTH_
	Wortbirne6 0 "CURRENT"
	B GET_CURRENT
	Wortbirne6 0 "CONTEXT"
	B GET_CONTEXT
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
	Wortbirne6 0 "DECIMAL"
	B DECIMAL_
	Wortbirne6 0 "HEX"
	B HEX_
	Wortbirne6 0 "PAD"
	B GET_PAD
	Wortbirne6 0 "BASE"
	B GET_BASE
	Wortbirne6 0 "HLD"
	B GET_HLD
	Wortbirne6 0 "zzz"
	B zzz
	Wortbirne6 0 "XXX"
	push {lr};BL DOCONST
	.word 0x55
	Wortbirne6 0 "TYPE"
	B TYPE
	Wortbirne6 0 "(S\")"
	B cSdticcend
	Wortbirne6 0 "IALIGNED"
	B IALIGNED
	Wortbirne6 0 "HALIGNED"
	B HALIGNED
	Wortbirne6 0 "DABS"
	B DABS
	Wortbirne6 0 "HH."
	B HHdot
	Wortbirne6 0 "KEY"
	B KEY
	Wortbirne6 0 "KEY?"
	B KEYque
	Wortbirne6 0 "EMIT"
	B EMIT
	Wortbirne6 0 "HOOK-KEY?"
	B GET_HOOKKEYQ
	Wortbirne6 0 "HOOK-KEY"
	B GET_HOOKKEY
	Wortbirne6 0 "HOOK-EMIT?"
	B GET_HOOKEMITQ
	Wortbirne6 0 "HOOK-EMIT"
	B GET_HOOKEMIT
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
	Wortbirne6 0 "HHP"
	B HHP
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
	Wortbirne6 0 "K"
	B K
	Wortbirne6 0 "J"
	B J
	Wortbirne6 0 "I"
	B I
	Wortbirne6 0 "(?DO)"
	B cqueDOcend
	Wortbirne6 0 "(DO)"
	B cDOcend
	Wortbirne6 0 "(+LOOP)"
	B caddLOOPcend
	Wortbirne6 0 "(LOOP)"
	B cLOOPcend
	Wortbirne6 0 "BOUNDS"
	B BOUNDS
	Wortbirne6 0 "COUNT"
	B COUNT
	Wortbirne6 0 "BIT"
	B BIT
	Wortbirne6 0 "<<"
	B lessless
	Wortbirne6 0 "LSHIFT"
	B lessless
	Wortbirne6 0 ">>"
	B greatgreat
	Wortbirne6 0 "RSHIFT"
	B greatgreat
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
	Wortbirne6 0 "DROP"
	B DgreatS
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
	B CHARadd
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
	B NOT
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
	B HBISsave
	Wortbirne6 0 "OR!"
	B ORsave
	Wortbirne6 0 "BIS!"
	B ORsave
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
	B HBICsave
	Wortbirne6 0 "ANDC!"
	B ANDCsave
	Wortbirne6 0 "BIC!"
	B ANDCsave
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
	B OFF
	Wortbirne6 0 "4C!"
	B _4Csave
	Wortbirne6 0 "!"
	B save
	Wortbirne6 0 "REG!"
	B save
	Wortbirne6 0 "2C!"
	B _2Csave
	Wortbirne6 0 "W!"
	B Wsave
	Wortbirne6 0 "H!"
	B Wsave
	Wortbirne6 0 "WREG!"
	B Wsave
	Wortbirne6 0 "C!"
	B Csave
	Wortbirne6 0 "4C@"
	B _4Cload
	Wortbirne6 0 "@"
	B load
	Wortbirne6 0 "REG@"
	B load
	Wortbirne6 0 "2C@"
	B _2Cload
	Wortbirne6 0 "W@"
	B Wload
	Wortbirne6 0 "H@"
	B Wload
	Wortbirne6 0 "WREG@"
	B Wload
	Wortbirne6 0 "C@"
	B Cload
	Wortbirne6 0 "EXECUTE"
	B EXECUTE
	Wortbirne6 0 "EXECCC"
	B EXECUTE
	Wortbirne6 0 "@EXECUTE"
	B loadEXECUTE
	Wortbirne6 0 "PERFORM"
	B loadEXECUTE
	Wortbirne6 0 "RP!"
	B SPsave
	Wortbirne6 0 "RP@"
	B RPload
	Wortbirne6 0 "SP!"
	B RPsave
	Wortbirne6 0 "SP@"
	B SPload
	Wortbirne6 0 ">R>R"
	B greatRgreatR
	Wortbirne6 0 "2>R"
	B _2greatR
	Wortbirne6 0 "2R@"
	B _2Rload
	Wortbirne6 0 "2R>"
	B _2Rgreat

	Wortbirne6 0 ">R"
	B greatR

	Wortbirne6 0 "R>"
	B Rgreat
	.global COLD
	Wortbirne6 0 "COLD"
COLD:
	ldr	r7, = sp_steck0 // sp_buff + 0x12c0
	mov r0, #0
	ldr	sp, [r0]
	B FMAIN
.ltorg

	.global ORIGIMG
	Wortbirne6 0 "ORIGIMG"
ORIGIMG:
	push {lr};BL DOCONST
	.word origin_data_forth


9:      .word 0  @ Link einfügen  Insert Link
        .hword 0     @ Flags setzen, diesmal 2 Bytes ! Wir haben Platz und Ideen :-)  Flag field, 2 bytes, space for ideas left !

        .byte 8f - 7f     @ Länge des Namensfeldes berechnen  Calculate length of name field
7:      .ascii "Name"    @ Namen anfügen  Insert name string
8:      .p2align 1        @ 1 Bit 0 - Wieder gerade machen  Realign
	B	NOOP

ForthWords:
	.word PForthWords+6

	.global GetForthWords
GetForthWords:
	STR	r6, [R7, #-4]!
	ldr r6, = PForthWords+6
	bx	lr
	bx	lr
.ltorg

	ldr	pc, = 0x12345679
.ltorg

.include "FVARS.S"

//	.global bcc_forth
//bcc_forth:

.p2align 1
	.global origin_data_forth
origin_data_forth:
o_d_forth:

	.include "SRC/bssvar.s"

	.global	sp_buff
	.section	.bss
	.align	2
sp_buff:
	.space	0x12c0
sp_steck0:
	.space	8

