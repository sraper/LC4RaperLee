;; Edit 10-15-12 by Jasmine Lee
	;; Square Root Program
	;; Input A : The number whose square root is to be computed
	;; Output B: The approximate square root - the largest integer such that  B*B <= A
	;; A = R0
	;; B = R1
	;; C = R2 Temporary memory for B*B

.CODE		; This is a code segment
.ADDR  0x0000	; Start filling in instructions at address 0x00

	CONST R1, #0    ; Initializa B to 0

	CMPI R0 #0	; Compare A to 0
	BRn DECREMENT	; If A < 0 go to DECREMENT
LOOP
	MUL R2, R1, R1	; C = B*B
	CMP R2, R0	; compare C to A
	BRp DECREMENT	; If B*B > A then jump to DECREMENT
	ADD R1, R1, #1	; B = B + 1
	JMP LOOP
DECREMENT
	ADD R1, R1, #-1	; B = B - 1
END
	NOP	