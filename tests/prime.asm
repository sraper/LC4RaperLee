;; Edit 10-19-12 by Jasmine Lee
;; Prime Program
;; Input A: Any Real Number
;; Output PRIME_FLAG: 1- number is prime; 0- number is not prime
	;; R0 = A
	;; R1 = PRIME_FLAG
	;; R2 = B
	;; R3 = temporary storage for B*B
	;; R4 = temporary storage for A % B

;; Start of the code section
	.CODE
	.ADDR x0000		; Start the code at address 0x0000

	CONST R0, #9
INIT

	CMPI R0, #1		; compare R0 to 1
	BRnz NOT_PRIME		; if R0 <= 1 then go to NOT_PRIME

	CONST R2, #2		; initialize B to 2
	CONST R1, #1		; set PRIME_FLAG = 1
TEST
	MUL R3, R2, R2		; store B*B in R3
	CMP R3, R0		; compare B*B to A
	BRp END			; if B*B > A then go to END
LOOP
	MOD R4 R0 R2		; R4 = A % B
	CMPI R4, #0		; compare (A % B) to 0
	BRz NOT_PRIME		; if (A % B) == 0 then the number is not prime
BODY
	ADD R2, R2, #1		; increment B by 1
	JMP TEST
NOT_PRIME
	CONST R1, #0		; set PRIME_FLAG = 0 (number is not prime)
END
	NOP