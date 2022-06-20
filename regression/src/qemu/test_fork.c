#include "devteroflex.h"
#include <unistd.h>

int argx = 100;
int main() {
  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_START);
  pid_t pid = fork();
  if (pid == 0){
    argx += 1;
  } else {
    argx += 2;
  }
  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_STOP);
  printf("PID[%i] stopped now\n", pid);
  if(pid == 0) {
    do_assert(argx == 101);
  } else {
    do_assert(argx == 102);
  }
 
  printf("Success fork PID[%i]!\n", pid);
  return 0;
}

