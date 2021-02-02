demander.txt: demander.out
	riscv64-elf-objdump -d $< -S | bat
	riscv64-elf-elf2hex --bit-width 32 --input $< --output $@

demander.out: start.o demander.o
	riscv64-elf-ld -m elf32lriscv -o $@ -T linker.ld $^

start.o: start.s
	riscv64-elf-g++ -c $< -march=rv32i -mabi=ilp32 -o $@ -nostdlib -O3

demander.o: demander.cc demander_api.hpp
	riscv64-elf-g++ -c $< -march=rv32i -mabi=ilp32 -o $@ -nostdlib -O3

clean:
	rm -rf demander.txt *.out *.o

