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

static int hit_segfault = 0;
static char *x_array;

static void handler(int sig, siginfo_t *si, void *unused){
  uint64_t addr = (uint64_t) si->si_addr & ~PAGE_MASK;
  mprotect((void *) addr, PAGE_SIZE, PROT_WRITE);
  hit_segfault++;
}

int main() {
  struct sigaction sa;
  sa.sa_flags = SA_SIGINFO;
  sigemptyset(&sa.sa_mask);
  sa.sa_sigaction = handler;
  sigaction(SIGSEGV, &sa, NULL);

  int id = open(DUMMY_FILE_DIR, O_RDONLY);
  x_array = mmap(NULL, PAGE_SIZE, PROT_READ, MAP_PRIVATE, id, 0);
  puts(x_array);

  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_START);
  x_array[0] = '/';
  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_STOP);

  do_assert(x_array[0] == '/');
  do_assert(hit_segfault == 1);
  munmap((void *) x_array, PAGE_SIZE);
  close(id);
  printf("Success mprotect_segfault!\n");
  return 0;
}

