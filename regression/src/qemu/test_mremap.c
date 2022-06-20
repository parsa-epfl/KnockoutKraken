#define _GNU_SOURCE

#include <stdbool.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <stdint.h>
#include <string.h>

#include "devteroflex.h"

static void handler(int sig, siginfo_t *si, void *unused){
  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_STOP);
  printf("Hit unexpected segmentation fault\n");
  do_assert(false);
}

int main(){
  struct sigaction sa;
  sa.sa_flags = SA_SIGINFO;
  sigemptyset(&sa.sa_mask);
  sa.sa_sigaction = handler;
  sigaction(SIGSEGV, &sa, NULL);

  int id = open(DUMMY_FILE_DIR, O_RDONLY);
  // char *x_array = mmap(NULL, PAGE_SIZE, PROT_WRITE, MAP_PRIVATE, -1, 0);
  char *x_array = mmap(NULL, PAGE_SIZE, PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  for(int c = 0; c < PAGE_SIZE; c++) {
    x_array[c] = rand() % 256;
  }
  mprotect((void *) x_array, PAGE_SIZE, PROT_READ);

  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_START);
  char first_char = x_array[0];
  char page_end = x_array[PAGE_SIZE-1];
  uint64_t last_addr = (uint64_t) x_array;
  x_array = mremap((void *)x_array, PAGE_SIZE, 2*PAGE_SIZE, MREMAP_MAYMOVE);
  uint64_t next_addr = (uint64_t) x_array;
  do_assert(x_array > 0);
  do_assert(last_addr != next_addr);
  char first_char_remap = x_array[0];
  char page_end_remap = x_array[PAGE_SIZE-1];
  char last_char_remap = x_array[PAGE_SIZE];
  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_STOP);

  printf("first %c -> %c; page_end %c -> %c; last_remap %c\n", 
        first_char, first_char_remap, page_end, page_end_remap, last_char_remap);
  do_assert(first_char == first_char_remap);
  do_assert(page_end == page_end_remap);
  do_assert(last_char_remap == 0);
  munmap((void *)x_array, 2*PAGE_SIZE);
  close(id);

  printf("Success mremap!\n");
  return 0;
}

