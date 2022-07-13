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

typedef int func(int, int);

static int hit_segfault = 0;

// Allow change permissions for PROT_EXEC 
static void handler(int sig, siginfo_t *si, void *unused){
  uint64_t addr = (uint64_t) si->si_addr & ~PAGE_MASK;
  mprotect((void *) addr, PAGE_SIZE, PROT_EXEC);
  hit_segfault++;
}

static int simpleFunc(int value, int blank) {
  int ret = value + 1;
  return ret;
}

static int simpleFunc2(int value, int blank) {
  int ret = value + 2;
  return ret;
}

static int simpleFunc3(int value, int blank) {
  int ret = value + 3;
  return ret;
}

int main(){
  struct sigaction sa;
  sa.sa_flags = SA_SIGINFO;
  sigemptyset(&sa.sa_mask);
  sa.sa_sigaction = handler;
  sigaction(SIGSEGV, &sa, NULL);

  void *page_buffer = mmap(NULL, PAGE_SIZE, PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  void *func_ptr = &simpleFunc;
  ((uint8_t *)page_buffer)[PAGE_SIZE/2] = 254;
  memcpy(page_buffer, func_ptr, PAGE_SIZE/2);

  mprotect(page_buffer, PAGE_SIZE, PROT_READ);

  func* f_normal = (func *) func_ptr;
  func* f_segfault = (func *) page_buffer;

  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_START);

  int ret_normal = f_normal(12, 5);
  do_assert(hit_segfault == 0);

  uint8_t ret_int0 = ((uint8_t *)page_buffer)[PAGE_SIZE/2];
  int ret_segfault = f_segfault(25, 5);
  do_assert(hit_segfault == 1);

  mprotect(page_buffer, PAGE_SIZE, PROT_WRITE);
  memcpy(page_buffer, &simpleFunc2, PAGE_SIZE/2);
  mprotect(page_buffer, PAGE_SIZE, PROT_READ);
  uint8_t ret_int1 = ((uint8_t *)page_buffer)[PAGE_SIZE/2];
  int ret_segfault2 = f_segfault(25, 5);
  do_assert(hit_segfault == 2);

  mprotect(page_buffer, PAGE_SIZE, PROT_WRITE);
  memcpy(page_buffer, &simpleFunc3, PAGE_SIZE/2);
  mprotect(page_buffer, PAGE_SIZE, PROT_EXEC);
  uint8_t ret_int2 = ((uint8_t *)page_buffer)[PAGE_SIZE/2];
  int ret_nofault3 = f_segfault(25, 5);
  do_assert(hit_segfault == 2);
 

  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_STOP);

  // Test whenever check passed
  do_assert(ret_normal == 13);
  do_assert(ret_segfault == 26);
  do_assert(ret_segfault2 == 27);
  do_assert(ret_nofault3 == 28);
  do_assert(ret_int0 == 254);
  do_assert(ret_int1 == 254);
  do_assert(ret_int2 == 254);

  munmap((void *) page_buffer, PAGE_SIZE);
  printf("All tests passed, with %i SEGFAULTS\n", hit_segfault);

  printf("Success inst_fetch_segfault!\n");
  return 0;
}
