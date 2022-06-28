#define CATCH_CONFIG_MAIN

#include "catch.hpp"

extern "C" {
  #include "fpga.h"
  #include "fpga_interface.h"
}
#include <unistd.h>

#include "test-helpers.hh"

TEST_CASE("basic-transplant-with-initial-page-fault"){
  FPGAContext c;
  DevteroflexArchState state;
  initFPGAContextAndPage(1, &c);
  initArchState(&state, rand());

  int th = 3;
  uint32_t asid = GET_asid(th);
  state.asid = asid;

  // initialization.
  transplantPushAndWait(&c, th, &state);
  transplantStart(&c, th);

  // Let's query message. It should be a page fault.
  MessageFPGA msg;
  REQUIRE(mmuMsgGet(&c, &msg) == 0);


  REQUIRE(msg.type == sPageFaultNotify);
  REQUIRE(msg.asid == asid);
  REQUIRE(msg.vpn_lo == VPN_GET_LO(state.pc));
  REQUIRE(msg.vpn_hi == VPN_GET_HI(state.pc));
  REQUIRE(msg.PageFaultNotif.permission == INST_FETCH);

  releaseFPGAContext(&c);
}



TEST_CASE("execute-instruction-with-context-in-dram"){
  // load binary file
  INFO("Load binary file");
  FILE *f = fopen("../src/client/tests/asm/executables/a.bin", "rb");
  REQUIRE(f != nullptr);
  REQUIRE(fread(page, 1, 4096, f) != 0);
  fclose(f);

  // setup context
  INFO("Setup context");
  FPGAContext c;
  initFPGAContextAndPage(1, &c);

  DevteroflexArchState state;
  uint64_t page_paddr = c.ppage_base_addr; // first page PA.
  uint64_t page_inst_paddr = page_paddr;
  uint64_t page_data_paddr = page_paddr+PAGE_SIZE;
  uint64_t pc = 0x40000000;
  initArchState(&state, pc);

  uint32_t thid = 3;
  uint32_t asid = GET_asid(thid);

  // Push Instruction page and Data page
  INFO("Push Instruction Page");
  dramPagePush(&c, page_inst_paddr, page);
  MessageFPGA pf_reply;
  makeMissReply(INST_FETCH, -1, asid, pc, page_inst_paddr, &pf_reply);
  mmuMsgSend(&c, &pf_reply);

  INFO("Push Data Page");
  uint64_t mem_addr = 0;
  uint32_t memory_page[PAGE_SIZE/4] = {0};
  for(int word = 0; word < PAGE_SIZE/4; word++) {
    memory_page[word] = 0xDEADBEEF;
  }
  dramPagePush(&c, page_data_paddr, memory_page);
  makeMissReply(DATA_STORE, -1, asid, mem_addr, page_data_paddr, &pf_reply);
  mmuMsgSend(&c, &pf_reply);

  INFO("Compare Instruction Page");
  uint8_t check_page_buffer[PAGE_SIZE] = {0};
  dramPagePull(&c, page_inst_paddr, check_page_buffer);
  for(int i = 0; i < PAGE_SIZE; ++i){
    REQUIRE(check_page_buffer[i] == page[i]);
  }
  dramPagePull(&c, page_data_paddr, check_page_buffer);
  uint8_t *data_page = (uint8_t *)memory_page;
  for(int i = 0; i < PAGE_SIZE; ++i){
    REQUIRE(check_page_buffer[i] == data_page[i]);
  }

  // prepare for transplant.
  INFO("Transplant state to FPGA");
  state.asid = asid;
  REQUIRE(transplantPushAndWait(&c, thid, &state) == 0);

  // start execution
  INFO("Start execution");
  REQUIRE(transplantStart(&c, thid) == 0);
 
  // First page fault here, it's instruction page.
  // Transplant back?
  INFO("Check thread state");

  uint32_t pending_threads = 0;
  while(!pending_threads) {
    REQUIRE(transplantPending(&c, &pending_threads) == 0);
    advanceTicks(&c, 100);
  }
  REQUIRE((pending_threads & (1 << thid)) != 0);

  // make it back
  INFO("Transplant thread back");
  transplantGetState(&c, thid, &state);

  // check context
  INFO("Check context");
  REQUIRE(state.xregs[0] == 10);
  REQUIRE(state.xregs[1] == 0);
  REQUIRE(state.pc == pc + 4 * 6);

  // fetch the page back.
  INFO("Send Page Eviction Request");
  MessageFPGA evict_request;
  makeEvictRequest(asid, mem_addr, &evict_request);
  mmuMsgSend(&c, &evict_request);
 
  // Let's query message. It should send an eviction message
  INFO("Query Eviction Notification");
  MessageFPGA msg;
  mmuMsgGet(&c, &msg);

  REQUIRE(msg.type == sEvictNotify);
  REQUIRE(msg.asid == asid);
  REQUIRE(msg.vpn_hi == VPN_GET_HI(0L));
  REQUIRE(msg.vpn_lo == VPN_GET_LO(0L));

  // Let's query message. It should send an eviction completed message
  INFO("Query Eviction Notification Complete");
  mmuMsgGet(&c, &msg);

  REQUIRE(msg.type == sEvictDone);
  REQUIRE(msg.asid == asid);
  REQUIRE(msg.vpn_hi == VPN_GET_HI(0L));
  REQUIRE(msg.vpn_lo == VPN_GET_LO(0L));
  REQUIRE(msg.EvictNotif.modified);
  REQUIRE(msg.EvictNotif.ppn == GET_PPN_FROM_PADDR(page_data_paddr));
  REQUIRE(msg.EvictNotif.permission == DATA_STORE);

  uint32_t data_buffer[PAGE_SIZE/4];
  INFO("Fetch page from FPGA");
  dramPagePull(&c, page_data_paddr, &data_buffer);
  // CHECK its value
  REQUIRE(data_buffer[0] == 10);

  releaseFPGAContext(&c);
}

TEST_CASE("execute-instruction") {
  // load binary file
  INFO("Load binary file");
  FILE *f = fopen("../src/client/tests/asm/executables/a.bin", "rb");
  REQUIRE(f != nullptr);
  REQUIRE(fread(page, 1, 4096, f) != 0);
  fclose(f);

  // setup context
  INFO("Setup context");
  FPGAContext c;
  initFPGAContextAndPage(1, &c);
  uint64_t page_inst_paddr = c.ppage_base_addr; // first page PA.
  uint64_t page_data_paddr = page_inst_paddr + PAGE_SIZE;
  DevteroflexArchState state;
  uint64_t pc = 0x40000000;
  initArchState(&state, pc);
  pmuStartCounting(&c);

  // prepare for transplant.
  INFO("Transplant state to FPGA");
  uint32_t thid = 3;
  uint32_t asid = GET_asid(thid);
  state.asid = asid;
  REQUIRE(transplantPushAndWait(&c, thid, &state) == 0);

  // start execution
  INFO("Start execution");
  REQUIRE(transplantStart(&c, thid) == 0);

  // First page fault here, it's instruction page.
  INFO("FPGA requires instruction page");
  MessageFPGA msg;
  mmuMsgGet(&c, &msg);

  INFO("Check page fault request");
  REQUIRE(msg.type == sPageFaultNotify);
  REQUIRE(msg.asid == asid);
  REQUIRE(msg.vpn_lo == VPN_GET_LO(state.pc));
  REQUIRE(msg.vpn_hi == VPN_GET_HI(state.pc));
  REQUIRE(msg.PageFaultNotif.permission == INST_FETCH);

  // Reply with the correct page.
  INFO("Send instruction page to FPGA");
  dramPagePush(&c, page_inst_paddr, page);
  MessageFPGA pf_reply;
  makeMissReply(INST_FETCH, thid, asid, pc, page_inst_paddr, &pf_reply);
  mmuMsgSend(&c, &pf_reply);

  // Check if the page is there.

  INFO("Compare Instruction Page");
  uint8_t check_page_buffer[PAGE_SIZE] = {0};
  dramPagePull(&c, page_inst_paddr, check_page_buffer);
  for(int i = 0; i < PAGE_SIZE; ++i){
    REQUIRE(check_page_buffer[i] == page[i]);
  }

  // Page fault again for data. 
  INFO("FPGA requires data page");
  REQUIRE(mmuMsgGet(&c, &msg) == 0);

  INFO("Check page fault request");
  REQUIRE(msg.type == sPageFaultNotify);
  REQUIRE(msg.asid == asid);
  REQUIRE(msg.vpn_lo == VPN_GET_LO(0L));
  REQUIRE(msg.vpn_hi == VPN_GET_HI(0L));
  REQUIRE(msg.PageFaultNotif.permission == DATA_STORE);

  // If so, reply with a empty page.
  INFO("Send data page to FPGA");
  dramPagePush(&c, page_data_paddr, zero_page);
  makeMissReply(DATA_STORE, thid, asid, 0, page_data_paddr, &pf_reply);
  mmuMsgSend(&c, &pf_reply);

  // Wait for Transplant back
  INFO("Check thread state");
  uint32_t pending_threads = 0;
  while(!pending_threads) {
    REQUIRE(transplantPending(&c, &pending_threads) == 0);
    advanceTicks(&c, 100);
  }
  REQUIRE((pending_threads & (1 << thid)) != 0);

  // make it back
  INFO("Transplant thread back");
  transplantGetState(&c, thid, &state);

  pmuStopCounting(&c);
  printPMUCounters(&c);

  // check context
  INFO("Check context regs");
  REQUIRE(state.xregs[0] == 10);
  REQUIRE(state.xregs[1] == 0);
  INFO("Check context PC");
  REQUIRE(state.pc == pc + 4 * 6);

  INFO("Get back page");
  uint8_t data_buffer[4096];
  synchronizePage(&c, asid, data_buffer, 0, page_data_paddr, true);
  // CHECK its value
  INFO("Check final result");
  REQUIRE(data_buffer[0] == 10);

  releaseFPGAContext(&c);
}

