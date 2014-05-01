.DATA
.CODE
WHAT .CONST x84

	BRnzp x76
	BRzp #-4

	;; simple

BEGIN

	CONST R1 , 1	; Hello
	ADD R1, R1, #2; Hello
	ADD R2, R1, 0xAB		;hello
	SUB R1, R2, R1
	;; hey
	NOP

	.FALIGN

END