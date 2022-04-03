#include "../../fpga_interface.h"

#include "asm_helpers.h"

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

void initArchState(
  DevteroflexArchState *state, 
  uint64_t pc
) {
  for (int i = 0; i < 32; ++i) {
    state->xregs[i] = i;
  }
  //state->sp = 0;
  state->pc = pc;
  state->flags = 0;
  state->icountRegs = 0;
}

void initState_pressure_ldp_stp(
  DevteroflexArchState *state,
  uint64_t array_size,
  uint64_t array_ld_base_addr, 
  uint64_t array_st_base_addr
) {
  state->xregs[3] = array_ld_base_addr;
  state->xregs[4] = array_st_base_addr;
  state->xregs[5] = array_size;
}

void initState_set_flag(
  DevteroflexArchState *state,
  uint64_t set_flag_addr,
  uint64_t get_flag_addr
) {
  state->xregs[2] = set_flag_addr;
  state->xregs[3] = get_flag_addr;
}

void initState_ldst_all_sizes_pair(
  DevteroflexArchState *state,
  uint64_t mem_addr_ld,
  uint64_t mem_addr_st,
  uint64_t step_size
) {
  state->xregs[2] = mem_addr_ld;
  state->xregs[3] = mem_addr_st;
  state->xregs[4] = step_size;
};

void initState_infinite_loop(
  DevteroflexArchState *state,
  bool loop
) {
  if(loop) {
    state->xregs[0] = 0;
  } else {
    state->xregs[0] = 1;
  }
};

void initState_simple_inst(
  DevteroflexArchState *state,
  uint64_t reg1,
  uint64_t reg2
) {
  state->xregs[0] = reg1;
  state->xregs[1] = reg2;
};

void initState_exception_br(DevteroflexArchState *state) {
  state->pc = 0x0;
  state->xregs[0] = 0x2; // triger exception because of branch address
};

void initState_exception_svc(DevteroflexArchState *state) {
  state->pc = 0x4;
};
