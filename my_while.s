.section .data



.text
.global _start
_start:
	addi sp, sp, -32
	sd s0, 24(sp)
	addi s0, sp, 32

	sw zero, -24(s0)
	li a5, 11
	sw a5, -20(s0)

	call .while

	li a7, 93
	li a0, 0
	ecall

.while:
	lw a5, -24(s0)
	sext.w a4, a5
	li a5, 5
	ble a4, a5, .while_loop
	li a5, 0
	mv a0, a5
	ld s0, 24(sp)
	ret

.while_loop:
	lw a5, -24(s0)
	addiw a5, a5, 1
	sw a5, -24(s0)
	lw a5, -20(s0)
	addiw a5, a5, 21
	sw a5, -20(s0)
