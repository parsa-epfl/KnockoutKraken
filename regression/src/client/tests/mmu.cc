extern "C" {
  #include "../fpga.h"
  #include "../fpga_interface.h"
  #include "asm/asm_helpers.h"
}
#include <unistd.h>

#include "../test-helpers.hh"

TEST_CASE("multiple-pages-in-a-row") {
  FPGAContext ctx;
  REQUIRE(initFPGAContext(&ctx) == 0);
  MessageFPGA pf_reply;
  uint32_t asid = 10;
  uint64_t vaddr = 0x50000;
  uint64_t paddr = 0x20000;
  makeMissReply(DATA_STORE, -1, asid, vaddr, paddr, &pf_reply);
  for(int i = 0; i < 10; i++)
    mmuMsgSend(&ctx, &pf_reply);

  releaseFPGAContext(&ctx);
}

TEST_CASE("push-page-ppn-overflow", "aws-only") {
  FPGAContext ctx;
  REQUIRE(initFPGAContext(&ctx) == 0);
  MessageFPGA msg;
  uint32_t asid = 10;
  uint64_t vaddr = 1ULL << 34;
  uint64_t paddr = 1ULL << 33; // Overflow 32 bits
  makeMissReply(DATA_STORE, -1, asid, vaddr, paddr, &msg);
  uint64_t missReply_paddr = ((uint64_t) msg.MissReply.ppn) << 12;
  REQUIRE(paddr == missReply_paddr);
  mmuMsgSend(&ctx, &msg);

  INFO("Synchronize page");
  makeEvictRequest(asid, vaddr, &msg);
  mmuMsgSend(&ctx, &msg);

  // Let's query message. It should send an eviction message
  INFO("1. Query Eviction Notification");
  mmuMsgGet(&ctx, &msg);

  REQUIRE(msg.type == sEvictNotify);
  REQUIRE(msg.asid == asid);
  REQUIRE((msg.vpn << 12) == vaddr);
  REQUIRE(msg.EvictNotif.ppn == GET_PPN_FROM_PADDR(paddr));
  uint64_t evictNotif_paddr = ((uint64_t) msg.EvictNotif.ppn) << 12;
  REQUIRE(paddr == evictNotif_paddr);

  INFO("2. Query Eviction Notification Complete");
  mmuMsgGet(&ctx, &msg);

  REQUIRE(msg.type == sEvictDone);
  REQUIRE(msg.asid == asid);
  REQUIRE((msg.vpn << 12) == vaddr);
  REQUIRE(msg.EvictDone.ppn == GET_PPN_FROM_PADDR(paddr));
  REQUIRE(msg.EvictDone.permission == DATA_STORE);
  REQUIRE(msg.EvictDone.modified == false);
  uint64_t evictDone_paddr = ((uint64_t) msg.EvictDone.ppn) << 12UL;
  REQUIRE(paddr == evictDone_paddr);

  releaseFPGAContext(&ctx);
}

TEST_CASE("MMU-push-and-evict-pte"){
  FPGAContext c;
  initFPGAContextAndPage(1, &c);
  int th = 1;
  int asid = GET_asid(th);
  uint64_t va = rand() << 10;

  uint64_t page_inst_paddr = c.ppage_base_addr; // first page PA.

  // Fill the page buffer
  dramPagePush(&c, page_inst_paddr, page);

  MessageFPGA miss_reply; 
  makeMissReply(INST_FETCH, -1, asid, va, page_inst_paddr, &miss_reply);
  mmuMsgSend(&c, &miss_reply);

  advanceTicks(&c, 500);

  // now, page is in the DRAM now.
  // Let's get it back.
  MessageFPGA evict_request;
  makeEvictRequest(asid, va, &evict_request);
  mmuMsgSend(&c, &evict_request);

  // Let's query message. It should send an eviction message
  MessageFPGA msg;
  REQUIRE(mmuMsgGet(&c, &msg) == 0);

  REQUIRE(msg.type == sEvictNotify);
  REQUIRE(msg.asid == asid);
  REQUIRE(msg.vpn_hi == VPN_GET_HI(va));
  REQUIRE(msg.vpn_lo == VPN_GET_LO(va));

  // Then there is a done message
  REQUIRE(mmuMsgGet(&c, &msg) == 0);

  REQUIRE(msg.type == sEvictDone);
  REQUIRE(msg.asid == asid);
  REQUIRE(msg.vpn_hi == VPN_GET_HI(va));
  REQUIRE(msg.vpn_lo == VPN_GET_LO(va));

  releaseFPGAContext(&c);
}

TEST_CASE("Push-page-and-read-back", "aws-only"){
  FPGAContext c;
  initFPGAContextAndPage(1, &c);
  uint64_t page_inst_paddr = c.ppage_base_addr; // first page PA.


  INFO("Consistency check of DRAM");
  for(int i = 0; i < PAGE_SIZE; ++i){
    page[i] = rand();
  }
  writeAXI(&c, 1 << 27, page, PAGE_SIZE);
  advanceTicks(&c, 500);
  uint8_t buffer_first[PAGE_SIZE] = {0};
  readAXI(&c, 1 << 27, buffer_first, PAGE_SIZE);
  for(int i = 0; i < PAGE_SIZE; ++i){
    REQUIRE(buffer_first[i] == page[i]);
  }


  int asid = 0;

  INFO("Send page to FPGA");
  for(int i = 0; i < PAGE_SIZE; ++i){
    page[i] = rand();
  }
  dramPagePush(&c, page_inst_paddr, page);
  MessageFPGA pf_reply;
  makeMissReply(INST_FETCH, -1, asid, 0, page_inst_paddr, &pf_reply);
  mmuMsgSend(&c, &pf_reply);

  advanceTicks(&c, 500);

  INFO("Fetch page from FPGA DRAM");
  uint8_t new_page[PAGE_SIZE] = {0};
  dramPagePull(&c, page_inst_paddr, new_page);

  INFO("Compare page");
  for(int i = 0; i < PAGE_SIZE; ++i){
    REQUIRE(new_page[i] == page[i]);
  }

  releaseFPGAContext(&c);
}

