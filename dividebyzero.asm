
;; Edit 10-12-10 by Paul Gurniak - removed user heap/stack setup template.  Will put it back once it is covered in class.

.CODE ;; This section contains code.
.ADDR x0000 ;; Specify address to start putting the code

;; Demo counter starts here.  Organized into two functions, a main function where PennSim starts, which indefinitely calls an increment function that adds the value of R1 (1 by default) to R0.

	CONST R0, #2
	CONST R1, #-1
	DIV R2, R0, R1
END
	NOP