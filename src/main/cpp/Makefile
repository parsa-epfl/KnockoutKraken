all.out: demand_page.cc
	riscv64-elf-g++ $< -march=rv32i -mabi=ilp32 -o $@ -nostdlib -O3

demand_page.cc: demander_api.hpp

clean:
	rm -rf all.out