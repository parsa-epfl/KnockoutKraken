#define _GNU_SOURCE

#include <stdbool.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <stdint.h>

#include "devteroflex.h"

int main() {

  int src_array[PAGE_SIZE] = {0};
  for(int i = 0; i < PAGE_SIZE; i++) {
    src_array[i] = i;
  }
 
  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_START);
  int *dst_array = mmap(NULL, PAGE_SIZE*sizeof(int), PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  for(int i = 0; i < PAGE_SIZE; i++) {
    dst_array[i] = src_array[i];
  }
  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_STOP);

  bool failed = false;
  for (int i = 0; i < PAGE_SIZE; ++i) {
    if (dst_array[i] != src_array[i]) {
      printf("%03i:fpga_array[%i] != host_array[%i]\n", i, dst_array[i], src_array[i]);
      failed = true; 
    }
  }

  if(failed) {
    assert(false);
  }

  munmap((void *)dst_array, PAGE_SIZE*4);
  printf("Success cache_page_evict!\n");
  return 0;
}
