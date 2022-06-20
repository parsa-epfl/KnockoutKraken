#include "devteroflex.h"
#include <unistd.h>

int main() {
  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_START);
  usleep(500);
  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_STOP);
  printf("Success sleep!\n");
  return 0;
}
