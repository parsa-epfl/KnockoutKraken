#include <unistd.h>
#include "devteroflex.h"

int main() {
  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_START);
  int pid = getpid();
  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_STOP);
  do_assert(pid > 0);
  printf("Success get_pid: PID[%i]\n", pid);
  return 0;
}

