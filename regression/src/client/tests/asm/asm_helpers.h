#include "../../fpga_interface.h"

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

void initState_pressure_ldp_stp(
  DevteroflexArchState *state,
	uint64_t array_base_addr, 
	uint64_t array_size,
	uint64_t array_step
);

void initState_set_flag(
  DevteroflexArchState *state,
  uint64_t set_flag_addr,
  uint64_t get_flag_addr
);

void initState_ldst_all_sizes_pair(
  DevteroflexArchState *state,
  uint64_t mem_addr_ld,
  uint64_t mem_addr_st,
  uint64_t step_size
);

void initState_infinite_loop(
  DevteroflexArchState *state,
  bool loop
);

void initState_simple_inst(
  DevteroflexArchState *state,
  uint64_t reg1,
  uint64_t reg2
);

void initState_exception_br(DevteroflexArchState *state);
void initState_exception_svc(DevteroflexArchState *state);
void initState_select_sort(DevteroflexArchState *state, uint32_t asid, uint64_t pc, uint64_t addr);
