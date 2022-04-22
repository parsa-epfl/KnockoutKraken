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

  mmuRegisterTHID2ASID(&c, 7, 0x10);
  transplantPushState(&c, 0, &state);
  transplantPushState(&c, 1, &state);
  transplantPushState(&c, 2, &state);
  transplantPushState(&c, 3, &state);
  transplantPushState(&c, 4, &state);
  transplantPushState(&c, 5, &state);
  transplantPushState(&c, 6, &state);
  transplantPushState(&c, 7, &differentState);
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