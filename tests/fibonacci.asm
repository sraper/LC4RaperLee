;; Edit 10-19-12 by Jasmine Lee
;; Fibonacci Program
;; Output: the first 20 numbers in the fibonacci sequence
;; R0 is the pointer to memory address
;; R1 = F_i
;; R2 = i (loop counter)
;; R3 = F_(i_2)
;; R4 = F_(i-1)

	;; This is the data section
	.DATA
	.ADDR x4000		; Start the data at address 0x4000
	
global_array
	.FILL #0		; Put F_0 = 0 in x4000
	.FILL #1		; Put F_1 = 1 in x4001

	;; Start of the code section
	.CODE
	.ADDR x0000		; Start the code at address 0x0000
INIT
	LEA R0, global_array	; R0 contains the address of the data
	CONST R1, #0		; initialize F_i to 0
	CONST R2, #2 		; initialize i to 2
	JMP TEST
BODY
	LDR R3, R0, #0		; Load F_(i-2) into R3
	ADD R0, R0, #1		; increment address
	LDR R4, R0, #0		; Load F_(i-1) into R4
	ADD R1 R3 R4		; update value of F_i in R1
	ADD R0, R0, #1 		; increment address
	STR R1 R0 #0		; store value of F_i in memory
	ADD R0, R0, #-1		; decrement the address
	ADD R2, R2, #1		; increment the loop counter i
TEST
	CMPI R2, #20		; check if the loop counter is 20 yet
	BRn BODY		; execute body if i is not 20 yet
END
	NOP