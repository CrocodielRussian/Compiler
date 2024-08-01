set history save
set architecture riscv:rv64
set sysroot /usr/riscv64-linux-gnu
target remote :8089
tui new-layout example {-horizontal regs 1 asm 2} 2 status 0 cmd 1
layout example
