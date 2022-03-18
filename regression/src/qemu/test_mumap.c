#define _GNU_SOURCE

#include <fcntl.h>
#include <assert.h>
#include <sys/mman.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>


#define STR(x)  #x
#define XSTR(s) STR(s)
#define magic_inst(val) __asm__ __volatile__ ( "hint " XSTR(val) " \n\t"  )

#define DO_QFLEX_OP(op) magic_inst(QFLEX_OP); magic_inst(op)

#define DEVTEROFLEX_OP    (94)

#define DEVTEROFLEX_FLOW_START (90)
#define DEVTEROFLEX_FLOW_STOP  (91)
#define DO_DEVTEROFLEX_OP(op) magic_inst(DEVTEROFLEX_OP); magic_inst(op)

int main(){
  int id = open(__FILE__, O_RDONLY);
  char *x_array = mmap(NULL, 2048, PROT_READ, MAP_PRIVATE, id, 0);
  puts(x_array);

  x_array = mremap((void *)x_array, 2048, 65536, MREMAP_MAYMOVE);

  mprotect((void *)x_array, 65536, PROT_WRITE);

  x_array[0] = '/';

  puts(x_array);

  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_START);
  munmap((void *)x_array, 65536);
  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_STOP);

  close(id);

  return 0;
}

