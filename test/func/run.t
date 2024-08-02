  $  qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./simple_func.exe 
  30
  $  qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./recursion.exe 
  15
  $  qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./buffer_of_result_func.exe 
  40
  $  qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./print_int.exe 
  0
  10
  -10
  10002221
  -10002221
  $ bash -c "echo -e '11\n20' |  qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./read_int.exe"
  11
  20
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./several_args_in_func.exe
  66

