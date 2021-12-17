#include "test-helpers.hh"
#include "fpga.h"
#include "fpga_interface.h"

void requireStateIsIdentical(const ArmflexArchState &state1,
                             const ArmflexArchState &state2) {
  REQUIRE(state1.pc == state2.pc);
  REQUIRE(GET_NZCV(state1.nzcv) == GET_NZCV(state2.nzcv));
  REQUIRE(state1.sp == state2.sp);
  for (int i = 0; i < 32; ++i) {
    REQUIRE(state1.xregs[i] == state2.xregs[i]);
  }
}

void initArchState(ArmflexArchState *state, uint64_t pc) {
  for (int i = 0; i < 32; ++i) {
    state->xregs[i] = i;
  }
  state->nzcv = 0;
  state->sp = 0;
  state->pc = pc;
}

void synchronizePage(FPGAContext *ctx, int asid, uint8_t *page, uint64_t vaddr,
                     uint64_t paddr, bool expect_modified) {
  INFO("Synchronize page");
  MessageFPGA evict_request;
  makeEvictRequest(asid, vaddr, &evict_request);
  sendMessageToFPGA(ctx, &evict_request, sizeof(evict_request));

  // Let's query message. It should send an eviction message
  INFO("1. Query Eviction Notification");
  MessageFPGA message;
  queryMessageFromFPGA(ctx, (uint8_t *)&message);

  REQUIRE(message.type == sEvictNotify);
  REQUIRE(message.asid == asid);
  REQUIRE(message.vpn_hi == VPN_GET_HI(vaddr));
  REQUIRE(message.vpn_lo == VPN_GET_LO(vaddr));

  // Let's query message. It should send an eviction completed message
  INFO("2. Query Eviction Notification Complete");
  queryMessageFromFPGA(ctx, (uint8_t *)&message);

  REQUIRE(message.type == sEvictDone);
  REQUIRE(message.asid == asid);
  REQUIRE(message.vpn_hi == VPN_GET_HI(vaddr));
  REQUIRE(message.vpn_lo == VPN_GET_LO(vaddr));
  REQUIRE(message.EvictDone.ppn == GET_PPN_FROM_PADDR(paddr));
  REQUIRE(message.EvictDone.permission == DATA_STORE);
  REQUIRE(message.EvictDone.modified == expect_modified);

  fetchPageFromFPGA(ctx, paddr, page);
}

void makeDeadbeefPage(uint8_t *pages, size_t bytes) {
  uint32_t *page_word = (uint32_t *)pages;
  for (int word = 0; word < bytes / 4; word++) {
    page_word[word] = 0xDEADBEEF;
  }
}

void makeZeroPage(uint8_t *page) {
  uint32_t *page_word = (uint32_t *)page;
  for (int word = 0; word < PAGE_SIZE / 4; word++) {
    page_word[word] = 0xFFFFFFFF;
  }
}

void advanceTicks(const FPGAContext *c, int ticks) {
  writeSimCtrl(c, cycleStep, ticks);
}
