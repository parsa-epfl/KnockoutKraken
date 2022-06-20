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

static void handler(int sig, siginfo_t *si, void *unused){
  // round to 4K
  uint64_t addr = (uint64_t) si->si_addr & ~PAGE_MASK;
  mprotect((void *) addr, PAGE_SIZE, PROT_WRITE);
  hit_segfault++;
}

int main(){
  srand(0);

  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_START);

  struct sigaction sa;
  sa.sa_flags = SA_SIGINFO;
  sigemptyset(&sa.sa_mask);
  sa.sa_sigaction = handler;
  sigaction(SIGSEGV, &sa, NULL);


  char values[3] = {0};
  int  addr[3] = {0};

  char *x_array = mmap(NULL, PAGE_SIZE, PROT_READ, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);

  bool a0_equal_0 = x_array[0] == 0;

  x_array = mremap((void *)x_array, PAGE_SIZE, PAGE_SIZE*16, MREMAP_MAYMOVE);

  bool a0_equal_0_postremap = x_array[0] == 0;

  for (int i = 0; i < 3; ++i){
    int addr1 = rand() % PAGE_SIZE*16;
    int addr2 = rand() % PAGE_SIZE*16;
    x_array[addr1] = rand() % 256 + x_array[addr2];
    addr[i] = addr1;
    values[i] = x_array[addr1];
  }

  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_STOP);
  // Test whenever check passed

  do_assert(a0_equal_0);
  do_assert(a0_equal_0_postremap);
  do_assert(hit_segfault > 0);
  for (int i =0; i<3; i++) {
    do_assert(x_array[addr[i]] == values[i]);
  }

  munmap((void *)x_array, PAGE_SIZE*16);
  printf("All tests passed, with %i SEGFAULTS\n", hit_segfault);

  printf("Success mem_svc_segfault_full!\n");
  return 0;
}
