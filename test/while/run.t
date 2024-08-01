  $  qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./simple_while.exe
  0
  1
  2
  3
  4
  5
  $  qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./while_with_break.exe
  0
  1
  2
  $  qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./fibonacci.exe
  5
  $  qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./factorial.exe
  720
  $  qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./inner_while.exe
  $ echo $?
  0
