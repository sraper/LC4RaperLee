.DATA
.CODE
WHAT .CONST x84

	;; simple

BEGIN

	CONST R1 , 1	; Hello
	ADDI R1, R1, #2; Hello
	ADDI R2, R1, xAB		;hello
	SUB R1, R2, R1
	;; hey
	NOP

END