#define _GNU_SOURCE

#include <fcntl.h>
#include <sys/mman.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include "devteroflex.h"

int main(){
  int id = open(DUMMY_FILE_DIR, O_RDONLY);

  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_START);
  char *x_array = mmap(NULL, PAGE_SIZE/2, PROT_READ, MAP_PRIVATE, id, 0);
  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_STOP);

  do_assert(x_array[0] == '#');
  puts(x_array);
  x_array = mremap((void *)x_array, PAGE_SIZE/2, PAGE_SIZE*16, MREMAP_MAYMOVE);
  mprotect((void *)x_array, PAGE_SIZE*16, PROT_WRITE);
  x_array[0] = '/';
  puts(x_array);
  do_assert(x_array[0] == '/');
  munmap((void *)x_array, PAGE_SIZE*16);
  close(id);

  printf("Success mmap!\n");
  return 0;
}

