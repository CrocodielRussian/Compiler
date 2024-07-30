.data
.equ STDOUT, 1
.equ STDIN, 0
buffer:    .space 1  # Reserve 1 byte of space for the input

.text
.global print_int
# args[a0 - int to print]
print_int:
    addi sp, sp, -40        # create stack space
    sd s0, 32(sp)           # store frame pointer
    addi s0, sp, 40         # new frame pointer

    la t6, STDOUT           # chache output stream

    li t0, 0                # init sign bit
    li t1, 10               # divisor and new-line char
    addi t2, s0, -16        # t2: string[n] 
    add a1, zero, t2        # a1: string[0] currently string[n]
  
    addi t3, zero, '\n'     # '\n' char
    sb t3, 0(a1)            # store '\n'
  
    bge a0, zero, print_int.cached_number
    # a0 < 0 --> get absolute a
    li t0, 1                # set sign-bit to 1
    xori a0, a0, -1
    addi a0, a0, 1          # num = abs(num)

print_int.cached_number:
    remu         t3, a0, t1       # num % 10
    addi         t3, t3, '0'       # convert to ascii
    addi         a1, a1, -1       # decrement start pointer
    sb           t3, 0(a1)        # store value
    divu         a0, a0, t1       # num /= 10
    blt          zero, a0, print_int.cached_number    # if num > 0 loop

    beq          t0, zero, print_int.print
    li           t3, '-'
    addi         a1, a1, -1
    sb           t3, 0(a1)        # store char '-'

print_int.print:
    sub          t4, t2, a1
    addi         a2, t4, 1        # len =  string[n] - string[0] + 1
    li           a0, STDOUT
    li           a7, 64           # code of RISC-V (64-bits) syscall 'write'
    ecall                         

    ld           s0, 32(sp)       # restore frame pointer
    addi         sp, sp, 40       # free stack space
    ret   

.global read_int
read_int:
	addi	sp, sp, -40
	sd	ra, 32(sp)
	sd	fp, 24(sp)
	addi	fp, sp, 40
	li	a0, 0
	sd	a0, -24(fp)
	li	a0, 0
	sd	a0, -32(fp)
	call	read_char
	mv	a1, a0
	li	a0, '-'
	sub	a2, a1, a0
	seqz	a2, a2
	li	a0, '+'
	sub	a0, a1, a0
	seqz	a0, a0
	or	a0, a0, a2
	beq	a0, zero, .read_int.not_sign
	sd	a2, -32(fp)
	call	read_char
	li	a2, 48
	sub	a1, a0, a2
	j	.read_int.next_step
.read_int.not_sign:
	li	a0, 48
	sub	a1, a1, a0
	j	.read_int.next_step 
.read_int.next_step:
	j	.read_int.check_symbol
.read_int.read_next:
	ld	a0, -24(fp)
	li	a2, 10
	mul	a0, a2, a0
	add	a0, a1, a0
	sd	a0, -24(fp)
	call	read_char
	addi a0, a0, -48
	mv a1, a0
	j	.read_int.check_symbol
.read_int.check_symbol:
	li	a0, 0
	sgt	a0, a0, a1
	xori	a0, a0, 1
	li	a2, 9
	sgt	a2, a1, a2
	xori	a2, a2, 1
	and	a0, a0, a2
	bne	a0, zero, .read_int.read_next 
	ld	a0, -32(fp)
	beq	a0, zero, .not_negative
	li	a0, -1
	ld	a2, -24(fp)
	mul	a0, a2, a0
	sd	a0, -24(fp)
	j	.read_int.close_step
.not_negative:
	j	.read_int.close_step
.read_int.close_step:
	ld	a0, -24(fp)
	ld	ra, 32(sp)
	ld	fp, 24(sp)
	addi	sp, sp, 40
	ret 

# args[a0 - output stream, a1 - char to put]
.global putchar
putchar:
    addi sp, sp, -16    # create stack space
    sd s0, 8(sp)        # store frame pointer
    addi s0, sp, 16     # new frame pointer

    sd a1, 0(s0)        # save char on stack

    mv a1, s0           # load char stack address
    li a2, 1            # choose also 1 byte to write
    li a7, 64           # code of RISC-V (64-bits) syscall 'write'
    ecall

    ld s0, 8(sp)        # restore frame pointer
    addi sp, sp, 16     # free stack space
    ret

.global read_char
read_char:
    li a0, 0          # File descriptor 0 is STDIN
    la a1, buffer     # Load the address of the buffer into a1
    li a2, 1          # Number of bytes to read


    # Perform the read system call
    li a7, 63         # System call number for read (63 in RISC-V)
    ecall

    # Check if the read was successful
    bltz a0, error    # If a0 is negative, an error occurred

    # Load the byte from the buffer into a register
    lb a0, 0(a1)      # Load the byte from the buffer into t0
    ret

error:
    # Handle the error (e.g., print an error message)
    # Exit with an error code
    li a0, 1          # Exit code 1
    li a7, 93         # System call number for exit (93 in RISC-V)
    ecall             # Make the system call
