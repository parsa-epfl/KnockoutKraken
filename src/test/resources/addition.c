#define STR(x) #x
#define XSTR(s) STR(s)
#define magic_inst(val) __asm__ __volatile__ ( "hint " XSTR(val) " \n\t" );

void main(int argc, char *argv[]) {
    // use register for 64bit register instruction
    register long x = 10;
    register long y = 11;
    register long z = 12;
    register long tmp = x;
    
    magic_inst(95);
    tmp = tmp + 10;
    tmp = tmp + 11;
    tmp = tmp + 12;
    tmp = tmp + 13;
    tmp = tmp + 14;
    tmp = tmp + 15;
    tmp = tmp + 16;
    tmp = tmp + 17;
    tmp = tmp + 1;
    magic_inst(109);
    int ans = tmp;
    return;
}
