#include <stdio.h>
#include "devteroflex.h"

int main() {
  FILE *f = fopen(DUMMY_FILE_DIR, "r");
  char x = getc(f);
  do_assert(x == '#' && "Expect to get the char #.");

  puts("File opened! Now we will close it.");

  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_START);
  int ret = fclose(f);
  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_STOP);

  do_assert(ret == 0);
  printf("Success fclose!\n");
  return 0;
}
