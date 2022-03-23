extern "C" {
#include "../fpga.h"
#include "../fpga_interface.h"
}

#include "../test-helpers.hh"

TEST_CASE("transplant-in") {
  FPGAContext c;
  DevteroflexArchState state;
  initFPGAContextAndPage(1, &c);
  initArchState(&state, rand());

  int ret = 0;
  const int th = 0;

  REQUIRE(transplantPushState(&c, th, (uint64_t *) &state) == 0);

  // ---- Assert that correct state is was pushed
  DevteroflexArchState stateTransplant;
  REQUIRE(transplantGetState(&c, th, (uint64_t *) &stateTransplant) == 0);
  requireStateIsIdentical(state, stateTransplant);

  releaseFPGAContext(&c);
}

TEST_CASE("transplant-transplants") {
  FPGAContext c;
  initFPGAContextAndPage(1, &c);

  DevteroflexArchState state;
  uint64_t page_inst_paddr = c.ppage_base_addr; // first page PA.
  initArchState(&state, rand());

  int ret = 0;
  const int th = 0;
  const uint32_t asid = GET_asid(th);

  // ---- Push base page
  INFO("Push page");
  dramPagePush(&c, page_inst_paddr, page);
  MessageFPGA miss_reply; 
  INFO("Push Instruction page");
  makeMissReply(INST_FETCH, -1, asid, state.pc, page_inst_paddr, &miss_reply); // No thread is registered.
  mmuMsgSend(&c, &miss_reply);
  advanceTicks(&c, 500); // wait for the result to be synced. (Create 500 cycles interval from simulator side).

  // ---- Push thread state
  INFO("Push state and register asid")
  transplantRegisterAndPush(&c, th, asid, &state);

  // ---- Assert that correct state is was pushed
  INFO("Assert state is identical after pushing")
  DevteroflexArchState stateTransplant;
  transplantGetState(&c, th, (uint64_t *) &stateTransplant);
  requireStateIsIdentical(state, stateTransplant);

  // ---- Start execution
  INFO("Start exuection")
  transplantStart(&c, th);
  // Page fault message here

  // ---- Let's query the transplants, we should have a transplant pending
  uint32_t pending_threads = 0;
  size_t iterations = 0;
  INFO("Wait till pending transplant")
  while(!pending_threads) {
    ret = transplantPending(&c, &pending_threads);
    assert(!ret);
    iterations++;
    advanceTicks(&c, 100);
    REQUIRE(iterations < 1000);
  }
  REQUIRE(pending_threads != 0);
  REQUIRE(pending_threads & 1 << th);

  // ---- Assert that state was not modified
  // ---- Now assert no instruction was executed
  INFO("Bring back state")
  transplantUnregisterAndPull(&c, th, &stateTransplant);
  INFO("Assert state hasn't changed")
  requireStateIsIdentical(state, stateTransplant);
 
  // ---- Let's query message MMU queue. It should be empty.
  INFO("Check that no MMU message is pending")
  REQUIRE(!mmuMsgHasPending(&c));

  releaseFPGAContext(&c);
}