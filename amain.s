	.cpu cortex-m4
//	.file	"iii.c"
	.text
	.section	.text.tstadd,"ax",%progbits
	.align	1
	.global	tstadd
	.thumb
	.syntax unified
	.thumb_func
	.fpu fpv4-sp-d16
	.type	tstadd, %function

	.global	fimg
tstadd:
fimg:
.include "SRC/PRIMITIVES.4_AL"
.include "SRC/loop.4_AL"

	.global	tstsub
tstsub:
.include "SRC/HPROC.4_AL"
.include "SRC/USB.4_AL" 
.include "SRC/menu.4_AL"
	.global	FORTH_MAIN
FORTH_MAIN:
.include "SRC/main.4_AL"
.include "SRC/getvar.4_AL"

//	MOVW	R5, #:lower16:sp_buff
//	MOVT	R5, #:upper16:sp_buff


