#include <stdio.h>
#include <assert.h>
#include <strings.h> 

#define STR(x)  #x
#define XSTR(s) STR(s)
#define magic_inst(val) __asm__ __volatile__ ( "hint " XSTR(val) " \n\t"  )

#define DO_QFLEX_OP(op) magic_inst(QFLEX_OP); magic_inst(op)

#define DEVTEROFLEX_OP    (94)

#define DEVTEROFLEX_FLOW_START (90)
#define DEVTEROFLEX_FLOW_STOP  (91)
#define DO_DEVTEROFLEX_OP(op) magic_inst(DEVTEROFLEX_OP); magic_inst(op)

int main(){
  FILE *f = fopen(__FILE__, "r");

  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_START);

  char big_buffer[2048];

  bzero(big_buffer, 2048);

  assert(fread(big_buffer, 1, 2048, f) > 0);

  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_STOP);

  puts("The content: ");
  puts(big_buffer);

  fclose(f);
  return 0;

}