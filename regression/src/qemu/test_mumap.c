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

static void handler(int sig, siginfo_t *si, void *unused) {
  uint64_t addr = (uint64_t) si->si_addr & ~PAGE_MASK;
  uint64_t *addr_ptr = (uint64_t *) addr;
  *addr_ptr = (uint64_t) mmap(NULL, PAGE_SIZE, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  ((char *)addr_ptr)[0] = 'X';
  hit_segfault++;
}

int main() {
  struct sigaction sa;
  sa.sa_flags = SA_SIGINFO;
  sigemptyset(&sa.sa_mask);
  sa.sa_sigaction = handler;
  sigaction(SIGSEGV, &sa, NULL);

  int id = open(DUMMY_FILE_DIR, O_RDONLY);
  char *x_array = mmap(NULL, PAGE_SIZE, PROT_READ, MAP_PRIVATE, id, 0);

  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_START);
  munmap((void *)x_array, PAGE_SIZE);
  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_STOP);

  char readSegfault = x_array[0];
  do_assert(readSegfault == 'X');
  do_assert(hit_segfault == 1);
  close(id);

  printf("Success unmap!\n");
  return 0;
}
