#include "test-helpers.hh"
#include "fpga.h"
#include "fpga_interface.h"
#include <cstring>

void requireStateIsIdentical(const DevteroflexArchState &state1,
                             const DevteroflexArchState &state2) {
  REQUIRE(state1.pc == state2.pc);
  // REQUIRE(state1.sp == state2.sp);
  REQUIRE(FLAGS_GET_NZCV(state1.flags) == FLAGS_GET_NZCV(state2.flags));
  for (int i = 0; i < 32; ++i) {
    REQUIRE(state1.xregs[i] == state2.xregs[i]);
  }
}

void synchronizePage(FPGAContext *ctx, int asid, uint8_t *page, uint64_t vaddr,
                     uint64_t paddr, bool expect_modified) {
  INFO("Synchronize page");
  MessageFPGA evict_request;
  makeEvictRequest(asid, vaddr, &evict_request);
  mmuMsgSend(ctx, &evict_request);

  // Let's query message. It should send an eviction message
  INFO("1. Query Eviction Notification");
  MessageFPGA message;
  mmuMsgGetForce(ctx, &message);

  REQUIRE(message.type == sEvictNotify);
  REQUIRE(message.asid == asid);
  REQUIRE(message.vpn_hi == VPN_GET_HI(vaddr));
  REQUIRE(message.vpn_lo == VPN_GET_LO(vaddr));
  REQUIRE(message.EvictNotif.ppn == GET_PPN_FROM_PADDR(paddr));

  INFO("2. Query Eviction Notification Complete");
  mmuMsgGetForce(ctx, &message);

  REQUIRE(message.type == sEvictDone);
  REQUIRE(message.asid == asid);
  REQUIRE(message.vpn_hi == VPN_GET_HI(vaddr));
  REQUIRE(message.vpn_lo == VPN_GET_LO(vaddr));
  REQUIRE(message.EvictDone.ppn == GET_PPN_FROM_PADDR(paddr));
  REQUIRE(message.EvictDone.permission == DATA_STORE);
  REQUIRE(message.EvictDone.modified == expect_modified);

  // Let's query message. It should send an eviction completed message
  INFO("3. Fetch physical page from FPGA")
  dramPagePull(ctx, paddr, page);
}

void checkPagePerWord(uint8_t *page_expect, uint8_t *page_actual) {
  uint64_t *page_expect64 = (uint64_t *) page_expect;
  uint64_t *page_actual64 = (uint64_t *) page_actual;
  for(size_t word = 0; word < PAGE_SIZE/8; word++) {
      REQUIRE(page_expect64[word] == page_actual64[word]);
  }
}

void initFPGAContextAndPage(int num_threads, FPGAContext *c) {
  REQUIRE(initFPGAContext(c) == 0); 
  memset(page, 0xAB, PAGE_SIZE);
}

