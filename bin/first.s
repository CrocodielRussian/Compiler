.global _start
 _start:
addi sp, sp, -32
sd s0, 24(sp)
addi s0, sp,32


li a4, 10
sw a4, -20(s0)
li a4, 20
sw a4, -24(s0)
li a4, 30
sw a4, -28(s0)
li a5, zero
ld a4, -20(s0)
addi a5, a5, a4
ld a4, -24(s0)
addi a5, a5, a4
ld a4, -28(s0)
addi a5, a5, a4
sw a5, -32(s0)