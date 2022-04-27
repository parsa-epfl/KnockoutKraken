extern "C" {
#include "../fpga.h"
#include "../fpga_interface.h"
}

#include "../test-helpers.hh"

TEST_CASE("transplant-data-and-request-order") {
  FPGAContext c;
  DevteroflexArchState state;
  DevteroflexArchState stateOut;
  initFPGAContextAndPage(1, &c);
  initArchState(&state, 0xC000);
  DevteroflexArchState differentState;
  initArchState(&differentState, 0x4000);
  MessageFPGA msg;

  transplantPushAndWait(&c, 0, &state);
  transplantPushAndWait(&c, 1, &state);
  transplantPushAndWait(&c, 2, &state);
  transplantPushAndWait(&c, 3, &state);
  transplantPushAndWait(&c, 4, &state);
  transplantPushAndWait(&c, 5, &state);
  transplantPushAndWait(&c, 6, &state);
  transplantPushAndWait(&c, 7, &differentState);
  transplantStart(&c, 7);

  mmuMsgGet(&c, &msg);

  REQUIRE(msg.vpn == (differentState.pc >> 12));
  REQUIRE(msg.asid == 0x10);

  transplantForceTransplant(&c, 7);
  
  uint32_t pending;
  transplantWaitTillPending(&c, &pending);

  REQUIRE(pending & 1 << 7);
  transplantGetState(&c, 7, &stateOut);
  REQUIRE(differentState.pc == stateOut.pc);

  releaseFPGAContext(&c);
}