.global _start
_start:
addi sp, sp, -64
sd s0, 48(sp)
addi s0, sp, 64
# START CODE
li a5, 10
sd a5, -24(s0)
lw a5, -24(s0)
sd a5, -32(s0)
li a5, 20
ld a4, -32(s0)
sgt a5, a4, a5
beq a5, zero, .if_1_else

li a5, 10
sd a5, -24(s0)
j .L1

.if_1_else:

li a5, 40
ld a4, -24(s0)
addw a5, a4, a5
sd a5, -24(s0)
j .L1

.L1:
# END CODE
addi sp, sp, 64
li a0, 0
mv a5, a0
li a7, 93
ecall
