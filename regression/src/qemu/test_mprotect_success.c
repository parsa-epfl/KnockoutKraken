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

static void handler(int sig, siginfo_t *si, void *unused) {
  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_STOP);
  printf("Hit unexpected segmentation fault\n");
  do_assert(false);
}

int main() {
  struct sigaction sa;
  sa.sa_flags = SA_SIGINFO;
  sigemptyset(&sa.sa_mask);
  sa.sa_sigaction = handler;
  sigaction(SIGSEGV, &sa, NULL);

  int id = open(DUMMY_FILE_DIR, O_RDONLY);
  char *x_array = mmap(NULL, PAGE_SIZE/2, PROT_READ, MAP_PRIVATE, id, 0);
  puts(x_array);
  x_array = mremap((void *)x_array, PAGE_SIZE/2, PAGE_SIZE*16, MREMAP_MAYMOVE);

  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_START);

  mprotect((void *)x_array, PAGE_SIZE*16, PROT_WRITE);
  x_array[0] = '/';

  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_STOP);

  do_assert(x_array[0] == '/');
  munmap((void *)x_array, PAGE_SIZE*16);
  close(id);
  printf("Success mprotect_success!\n");
  return 0;
}
