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
  initArchState(&state, rand());
  MessageFPGA msg;

  mmuRegisterTHID2ASID(&c, 0, 0x10);
  transplantPushState(&c, 0, &state);
  transplantPushState(&c, 1, &state);
  transplantPushState(&c, 2, &state);
  transplantPushState(&c, 3, &state);
  transplantPushState(&c, 4, &state);
  transplantPushState(&c, 5, &state);
  transplantPushState(&c, 6, &state);
  transplantPushState(&c, 7, &state);
  transplantStart(&c, 3);

  mmuMsgGet(&c, &msg);

  REQUIRE(msg.vpn == state.pc);
  REQUIRE(msg.asid == 0x10);

  transplantForceTransplant(&c, 3);
  
  uint32_t pending;
  transplantWaitTillPending(&c, &pending);

  REQUIRE(pending & 1 << 3);
  transplantGetState(&c, 3, &stateOut);
  REQUIRE(state.pc == stateOut.pc);

  releaseFPGAContext(&c);
}