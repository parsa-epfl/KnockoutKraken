
#include <stdint.h>
#include "devteroflex.h"

static int global_bar = 32;
int foo(int bar) {
  bar++;
  global_bar = bar;
  return bar;
}

int main() {
  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_START);
  uint32_t inst_buffer[512] = {0};

  uint32_t *cast = (uint32_t *) &foo;
  for(int i = 0; i < 8; ++i){
    inst_buffer[i] = cast[i];
  }

  int bar = 43;
  int func_bar = foo(bar);

  DO_DEVTEROFLEX_OP(DEVTEROFLEX_FLOW_STOP);

  uint32_t *cast_func_verify = (uint32_t *) &foo;
  for(int i = 0; i < 8; ++i){
    assert(inst_buffer[i] == cast_func_verify[i]);
  }

  assert(func_bar == global_bar);
  assert(global_bar == bar + 1);

  printf("Success test_read_inst_page!\n");
  return 0;
}
