#include "fpga.h"
#include "fpga_interface.h"

#define GET_asid(th) ((th + 1) << 4)
void initArchState(ArmflexArchState *state, uint64_t pc);


// Page initialization
void makeDeadbeefPage(uint8_t *pages, size_t bytes);
void makeZeroPage(uint8_t *page);
void advanceTicks(const FPGAContext *c, int ticks);
