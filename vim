.global _start
_start:
addi sp, sp, -32
sd s0, 24(sp)
addi s0, sp, 32
# START CODE
li a5, 10
sw a5, -20(s0)
li a5, 10
sw a5, -24(s0)
j .while_1_condition

.while_1_loop:

j .while_2_condition

.while_2_loop:

lw a5, -24(s0)
sw a5, -28(s0)
li a5, 2
lw a4, -28(s0)
div a5, a4, a5
sw a5, -28(s0)
li a5, 0
lw a4, -28(s0)
sne a5, a4, a5
xori a5, a5, 1
beq a5, zero, .if_1_else

li a5, 2
lw a4, -24(s0)
addw a5, a4, a5
sw a5, -24(s0)
j .L1

.if_1_else:

li a5, 4
lw a4, -24(s0)
addw a5, a4, a5
sw a5, -24(s0)
j .L1

.L1:
j .while_2_condition

.while_2_condition:
lw a5, -24(s0)
sw a5, -28(s0)
li a5, 20
lw a4, -28(s0)
slt a5, a4, a5
bne a5, zero, .while_2_loop
li a5, 1
lw a4, -20(s0)
addw a5, a4, a5
sw a5, -20(s0)
j .while_1_condition

.while_1_condition:
lw a5, -20(s0)
sw a5, -28(s0)
li a5, 10
lw a4, -28(s0)
sgt a5, a4, a5
bne a5, zero, .while_1_loop
# END CODE
addi sp, sp, 32
li a0, 0
mv a5, a0
li a7, 93
ecall
