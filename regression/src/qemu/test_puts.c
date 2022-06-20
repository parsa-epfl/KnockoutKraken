#include "devteroflex.h"
#include <stdio.h>

int main() {
  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_START);
  puts("svc puts, hello world!\n");
  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_STOP);
  puts("Done executing, should have printed 'svc puts, hello world!' before\n");
  printf("Success puts!\n");
  return 0;
}

