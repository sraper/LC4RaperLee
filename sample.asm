
	CONST R2, #0    ; Initialize C to 0
	CONST R1, #4
	CONST R0, #6

LOOP

	CMPI R1, #0	; Compare B to 0
	BRnz END	; if (B <= 0) Branch to the end

	ADD R2, R2, R0	; C = C + A
	ADD R1, R1, #-1	; B =  B - 1

END