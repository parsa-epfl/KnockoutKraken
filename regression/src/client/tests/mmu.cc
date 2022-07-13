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
  makeEvictRequest(asid, vaddr, false, true, &msg);
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
  makeEvictRequest(asid, va, true, true, &evict_request);
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


TEST_CASE("flush-i-and-d-tlb") {
  FPGAContext ctx;
  DevteroflexArchState state;
  REQUIRE(initFPGAContext(&ctx) == 0);
  initArchState(&state, 0x0);
  int asid = 0x10;

  INFO("Load instruction")
  int paddr = ctx.ppage_base_addr;
  uint8_t page[PAGE_SIZE] = {0}; 
  makeDeadbeefPage(page, PAGE_SIZE);
  ((uint32_t *) page)[0] = 0xF940001F; // ldr     xzr, [x0]
  ((uint32_t *) page)[1] = 0x0;        // Trigger transplant
  uint64_t vaddr = 0x000000ABCD080;
  state.xregs[0] = vaddr;
  state.xregs[31] = 0xABCDABCDABCDABC;

  MessageFPGA pf_reply;

  INFO("Push instruction page");
  dramPagePush(&ctx, paddr, page);
  makeMissReply(INST_FETCH, -1, asid, state.pc, paddr, &pf_reply);
  mmuMsgSend(&ctx, &pf_reply);

  INFO("Push data page");
  ((uint64_t *) page)[(vaddr & 0xFFF)/8] = 0xB00B5B00B5B0A00;
  dramPagePush(&ctx, paddr + PAGE_SIZE, page);
  makeMissReply(INST_FETCH, -1, asid, vaddr, paddr + PAGE_SIZE, &pf_reply);
  mmuMsgSend(&ctx, &pf_reply);

  INFO("Push and start state");
  state.asid = asid;
  transplantPushAndSinglestep(&ctx, 0, &state);

  INFO("Advance");
  advanceTicks(&ctx, 200);

  INFO("Check now execution stopped");
  uint32_t pending_threads = 0;
  transplantPending(&ctx, &pending_threads);
  REQUIRE(pending_threads);

  INFO("Check transplant");
  transplantGetState(&ctx, 0, &state);
  INFO("Check undef")
  REQUIRE(!FLAGS_GET_IS_UNDEF(state.flags));
  INFO("Check exception")
  REQUIRE(!FLAGS_GET_IS_EXCEPTION(state.flags));
  INFO("Check address");
  REQUIRE(state.xregs[0] == vaddr);
  INFO("Check exception");

  INFO("Push and start state on Data Page");
  state.pc = GET_PAGE_MASK(vaddr);
  transplantPushAndSinglestep(&ctx, 0, &state);

  INFO("Advance");
  advanceTicks(&ctx, 200);

  INFO("Check now execution stopped");
  pending_threads = 0;
  transplantPending(&ctx, &pending_threads);
  REQUIRE(pending_threads);

  INFO("Check transplant");
  transplantGetState(&ctx, 0, &state);
  INFO("Check undef")
  REQUIRE(!FLAGS_GET_IS_UNDEF(state.flags));
  INFO("Check exception")
  REQUIRE(!FLAGS_GET_IS_EXCEPTION(state.flags));
  INFO("Check address");
  REQUIRE(state.xregs[0] == vaddr);
  INFO("Check exception");

  INFO("Evict page");
  synchronizePage(&ctx, asid, page, vaddr, true, paddr + PAGE_SIZE, false);

  INFO("Push and start state on Data Again");
  state.pc = GET_PAGE_MASK(vaddr);
  transplantPushAndSinglestep(&ctx, 0, &state);

  advanceTicks(&ctx, 200);

  INFO("Expect Page Fault");
  expectPageFault(&ctx, asid, GET_PAGE_MASK(vaddr), INST_FETCH);

  INFO("Push data page");
  makeMissReply(INST_FETCH, -1, asid, vaddr, paddr + PAGE_SIZE, &pf_reply);
  mmuMsgSend(&ctx, &pf_reply);

  advanceTicks(&ctx, 200);

  REQUIRE(!mmuMsgHasPending(&ctx));

  INFO("Check transplant");
  transplantGetState(&ctx, 0, &state);
  INFO("Check undef")
  REQUIRE(!FLAGS_GET_IS_UNDEF(state.flags));
  INFO("Check exception")
  REQUIRE(!FLAGS_GET_IS_EXCEPTION(state.flags));
  INFO("Check address");
  REQUIRE(state.xregs[0] == vaddr);

  releaseFPGAContext(&ctx);
}
