ASMC = riscv64-linux-gnu-as
ASMLD = riscv64-linux-gnu-ld

COMPILE_FLAGS = -march=rv64gc

SRC_DIR = lang/
TARGET_DIR = target/
PROGRAM_NAME = program.exe
GDB_ADDRESS=1234

all:
	dune exec compiler
	mkdir -p $(TARGET_DIR)
	make run

clear:
	rm -rf $(TARGET_DIR)
	mkdir -p $(TARGET_DIR)


$(TARGET_DIR)main.o: $(SRC_DIR)main.s
	$(ASMC) $(COMPILE_FLAGS) $(SRC_DIR)main.s -o $(TARGET_DIR)main.o

$(TARGET_DIR)stdlib: $(SRC_DIR)stdlib/io.s
	mkdir -p $(TARGET_DIR)stdlib
	make $(TARGET_DIR)stdlib/io.o

$(TARGET_DIR)stdlib/io.o:
	$(ASMC) $(COMPILE_FLAGS) $(SRC_DIR)stdlib/io.s -o $(TARGET_DIR)stdlib/io.o

program: $(TARGET_DIR)main.o $(TARGET_DIR)stdlib
	$(ASMLD) $(TARGET_DIR)main.o $(TARGET_DIR)stdlib/io.o -o $(PROGRAM_NAME)

run: program
	qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./$(PROGRAM_NAME)

debug: clear program
	qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 -g $(GDB_ADDRESS) ./$(PROGRAM_NAME)

gdb: program
	gdb-multiarch $(PROGRAM_NAME)
