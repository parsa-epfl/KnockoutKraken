#include "devteroflex.h"
#include <sys/mman.h>

int main(){
  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_START);
  FILE *f = fopen(DUMMY_FILE_DIR, "r");
  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_STOP);

  char x = getc(f);
  do_assert(x == '#' && "Expect to get the char #.");

  fclose(f);

  printf("Success fopen!\n");
  return 0;
}
