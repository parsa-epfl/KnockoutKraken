#include <cstdio>
#include <cstdlib>
extern "C" {
#include "../fpga.h"
#include "../fpga_interface.h"
#include "asm/asm_helpers.h"
}

#include "../test-helpers.hh"

TEST_CASE("matrix-multiply"){
  // 1. load the binary
  INFO("Prepare the binary");
  FILE *f = fopen("../src/client/tests/asm/executables/matrix_multiply.bin", "rb");
  uint8_t instruction_page[4096];
  REQUIRE(f != nullptr);
  fread(instruction_page, 1, 4096, f);
  fclose(f);
  // 2. prepare the map table: a, b, and o.
  uint8_t a[32][4096];
  uint8_t b[32][4096];
  uint8_t o[32][4096];

  // they are randomly initialized.
  for(int thread_idx = 0; thread_idx < 32; ++thread_idx){
    for (int pe = 0; pe < 4096; ++pe){
      a[thread_idx][pe] = rand();
      b[thread_idx][pe] = rand();
      o[thread_idx][pe] = rand();
    }
  }
  
  // 3. prepare the architecture state.
  DevteroflexArchState state;
  initArchState(&state, 0);
  // state.asid = 0;
  // 4. Run the experiment
  // 5. compare the result.
}