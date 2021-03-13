ASM_NAME=$1
aarch64-linux-gnu-gcc -c $ASM_NAME.s -o executables/$ASM_NAME.o
aarch64-linux-gnu-objcopy -O binary executables/$ASM_NAME.o executables/$ASM_NAME.bin
