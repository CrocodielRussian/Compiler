.global _start
_start:
	addi    sp, sp, -32
    sd      s0, 24(sp)
    addi    s0, sp, 32
	li a5, 1                            
	sw a5, -20(s0)
	j .L2
.L3:
	lw a5, -20(s0)
	sw a5, -24(s0)
	li a5, 1
	lw a4, -24(s0)
	addw a5, a4, a5
	sw a5, -20(s0)
.L2:
	lw a5, -20(s0)
	sw a5, -24(s0)
	li a5, 2
	lw a4, -24(s0)
	blt a5, a4, .L3
	addi sp, sp, 80
	li a0, 0
	mv a5, a0
	li a7, 93
	ecall