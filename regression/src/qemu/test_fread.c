#include "devteroflex.h"

int main(){
  FILE *f = fopen(DUMMY_FILE_DIR, "r");

  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_START);

  char x = getc(f);

  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_STOP);

  do_assert(x == '#' && "Expected to get the char #.\n");

  fclose(f);

  puts("Success fread!\n");
  return 0;
}

