#define CATCH_CONFIG_MAIN

#include "catch.hpp"

extern "C" {
  #include "fpga.h"
  #include "fpga_interface.h"
}
#include <unistd.h>

#include "test-helpers.hh"

uint8_t page[PAGE_SIZE] = {0};
uint8_t zero_page[PAGE_SIZE] = {0};

TEST_CASE("multiple-pages-in-a-row") {
  FPGAContext ctx;
  REQUIRE(initFPGAContext(&ctx) == 0);
  QEMUMissReply pf_reply;
  uint32_t asid = 10;
  uint64_t vaddr = 0x50000;
  uint64_t paddr = 0x20000;
  makeMissReply(DATA_STORE, -1, asid, vaddr, paddr, &pf_reply);
  for(int i = 0; i < 10; i++)
    sendMessageToFPGA(&ctx, &pf_reply, sizeof(pf_reply));

  releaseFPGAContext(&ctx);
}

static void initFPGAContextAndPage(int num_threads, FPGAContext *c) {
  REQUIRE(initFPGAContext(c) == 0); 
  memset(page, 0xAB, PAGE_SIZE);
}

TEST_CASE("transplant-in"){
  FPGAContext c;
  ArmflexArchState state;
  initFPGAContextAndPage(1, &c);
  initArchState(&state, rand());

  int ret = 0;
  const int th = 0;

  REQUIRE(transplant_pushState(&c, th, (uint64_t *) &state, ARMFLEX_TOT_REGS) == 0);

  // ---- Assert that correct state is was pushed
  ArmflexArchState stateTransplant;
  REQUIRE(transplant_getState(&c, th, (uint64_t *) &stateTransplant, ARMFLEX_TOT_REGS) == 0);
  requireStateIsIdentical(state, stateTransplant);

  releaseFPGAContext(&c);
}

TEST_CASE("MMU-push-and-evict-pte"){
  FPGAContext c;
  initFPGAContextAndPage(1, &c);
  int th = 1;
  int pid = GET_PID(th);
  uint64_t va = rand() << 10;

  uint64_t page_inst_paddr = c.base_address.page_base; // first page PA.

  // Fill the page buffer
  pushPageToFPGA(&c, page_inst_paddr, page);

  QEMUMissReply miss_reply; 
  makeMissReply(INST_FETCH, -1, pid, va, page_inst_paddr, &miss_reply);
  sendMessageToFPGA(&c, &miss_reply, sizeof(miss_reply));

  usleep(1e5); // wait for the result to be synced. (Create 500 cycles interval from simulator side).

  // now, page is in the DRAM now.
  // Let's get it back.
  QEMUPageEvictRequest evict_request;
  makeEvictRequest(pid, va, &evict_request);
  sendMessageToFPGA(&c, &evict_request, sizeof(evict_request));
  //usleep(1e6); // wait for the eviction to be complete.

  // Let's query message. It should send an eviction message
  uint32_t msg_buffer[16] = {0};
  REQUIRE(queryMessageFromFPGA(&c, (uint8_t *)msg_buffer) == 0);

  PageEvictNotification *evict_notify = (PageEvictNotification *) msg_buffer;
  REQUIRE(evict_notify->type == sEvictNotify);
  REQUIRE(evict_notify->pid == pid);
  REQUIRE(evict_notify->vpn_hi == VPN_GET_HI(va));
  REQUIRE(evict_notify->vpn_lo == VPN_GET_LO(va));

  // Then there is a done message
  REQUIRE(queryMessageFromFPGA(&c, (uint8_t *)msg_buffer) == 0);

  evict_notify = (PageEvictNotification *) msg_buffer;
  REQUIRE(evict_notify->type == sEvictDone);
  REQUIRE(evict_notify->pid == pid);
  REQUIRE(evict_notify->vpn_hi == VPN_GET_HI(va));
  REQUIRE(evict_notify->vpn_lo == VPN_GET_LO(va));

  releaseFPGAContext(&c);
}

TEST_CASE("Push-page-and-read-back", "aws-only"){
  FPGAContext c;
  initFPGAContextAndPage(1, &c);
  uint64_t page_inst_paddr = c.base_address.page_base; // first page PA.


  INFO("Consistency check of DRAM");
  for(int i = 0; i < PAGE_SIZE; ++i){
    page[i] = rand();
  }
  writeAXI(&c, 1 << 27, page, PAGE_SIZE);
  usleep(1e6);
  uint8_t buffer_first[PAGE_SIZE] = {0};
  readAXI(&c, 1 << 27, buffer_first, PAGE_SIZE);
  for(int i = 0; i < PAGE_SIZE; ++i){
    REQUIRE(buffer_first[i] == page[i]);
  }


  int pid = 0;

  INFO("Send page to FPGA");
  for(int i = 0; i < PAGE_SIZE; ++i){
    page[i] = rand();
  }
  pushPageToFPGA(&c, page_inst_paddr, page);
  QEMUMissReply pf_reply;
  makeMissReply(INST_FETCH, -1, pid, 0, page_inst_paddr, &pf_reply);
  sendMessageToFPGA(&c, &pf_reply, sizeof(pf_reply));

  usleep(1e6);

  INFO("Fetch page from FPGA DRAM");
  uint8_t new_page[PAGE_SIZE] = {0};
  fetchPageFromFPGA(&c, page_inst_paddr, new_page);

  INFO("Compare page");
  for(int i = 0; i < PAGE_SIZE; ++i){
    REQUIRE(new_page[i] == page[i]);
  }

  releaseFPGAContext(&c);
}

TEST_CASE("basic-transplant-with-initial-page-fault"){
  FPGAContext c;
  ArmflexArchState state;
  initFPGAContextAndPage(1, &c);
  initArchState(&state, rand());

  int th = 3;
  uint32_t pid = GET_PID(th);

  // initialization.
  registerAndPushState(&c, th, pid, &state);
  registerThreadWithProcess(&c, th, pid);
  // transplant_pushState(&c, th, (uint64_t *)&state, ARMFLEX_TOT_REGS);
  transplant_start(&c, th);

  // Let's query message. It should be a page fault.
  uint32_t buffer[16] = {0};
  REQUIRE(queryMessageFromFPGA(&c, (uint8_t *)buffer) == 0);

  PageFaultNotification *page_fault = (PageFaultNotification *)buffer;

  REQUIRE(page_fault->type == sPageFaultNotify);
  REQUIRE(page_fault->permission == INST_FETCH);
  REQUIRE(page_fault->pid == pid);
  REQUIRE(page_fault->vpn_lo == VPN_GET_LO(state.pc));
  REQUIRE(page_fault->vpn_hi == VPN_GET_HI(state.pc));

  releaseFPGAContext(&c);
}

TEST_CASE("transplant-transplants"){
  FPGAContext c;
  initFPGAContextAndPage(1, &c);

  ArmflexArchState state;
  uint64_t page_inst_paddr = c.base_address.page_base; // first page PA.
  initArchState(&state, rand());

  int ret = 0;
  const int th = 0;
  const uint32_t pid = GET_PID(th);

  // ---- Push base page
  pushPageToFPGA(&c, page_inst_paddr, page);
  QEMUMissReply miss_reply; 
  makeMissReply(INST_FETCH, -1, pid, state.pc, page_inst_paddr, &miss_reply); // No thread is registered.
  sendMessageToFPGA(&c, &miss_reply, sizeof(miss_reply));
  usleep(1e6); // wait for the result to be synced. (Create 500 cycles interval from simulator side).

  // ---- Push thread state
  registerAndPushState(&c, th, pid, &state);

  // ---- Assert that correct state is was pushed
  ArmflexArchState stateTransplant;
  transplant_getState(&c, th, (uint64_t *) &stateTransplant, ARMFLEX_TOT_REGS);
  requireStateIsIdentical(state, stateTransplant);

  // ---- Start execution
  transplant_start(&c, th);
  // Page fault message here

  // ---- Let's query the transplants, we should have a transplant pending
  uint32_t pending_threads = 0;
  size_t iterations = 0;
  while(!pending_threads) {
    ret = transplant_pending(&c, &pending_threads);
    assert(!ret);
    iterations++;
    usleep(1e4);
    REQUIRE(iterations < 1000);
  }
  REQUIRE(pending_threads != 0);
  REQUIRE(pending_threads & 1 << th);

  // ---- Assert that state was not modified
  // ---- Now assert no instruction was executed
  transplantBack(&c, th, &stateTransplant);
  requireStateIsIdentical(state, stateTransplant);
 
  // ---- Let's query message MMU queue. It should be empty.
  uint32_t queue_state = 1;
  REQUIRE(checkRxMessageQueue(&c, &queue_state) == 0);
  REQUIRE(queue_state == 0);

  releaseFPGAContext(&c);
}

TEST_CASE("execute-instruction-with-context-in-dram"){
  // instruction page
  uint8_t inst_page[4096];
  // load binary file
  INFO("Load binary file");
  FILE *f = fopen("../src/client/tests/asm/executables/a.bin", "rb");
  REQUIRE(f != nullptr);
  REQUIRE(fread(inst_page, 1, 4096, f) != 0);
  fclose(f);

  // setup context
  INFO("Setup context");
  FPGAContext c;
  initFPGAContextAndPage(1, &c);

  ArmflexArchState state;
  uint64_t page_paddr = c.base_address.page_base; // first page PA.
  uint64_t page_inst_paddr = page_paddr;
  uint64_t page_data_paddr = page_paddr+PAGE_SIZE;
  uint64_t pc = 0x40000000;
  initArchState(&state, pc);

  uint32_t thread_id = 3;
  uint32_t pid = GET_PID(thread_id);

  // Push Instruction page and Data page
  INFO("Push Instruction Page");
  pushPageToFPGA(&c, page_inst_paddr, inst_page);
  QEMUMissReply pf_reply;
  makeMissReply(INST_FETCH, -1, pid, pc, page_inst_paddr, &pf_reply);
  sendMessageToFPGA(&c, &pf_reply, sizeof(pf_reply));

  INFO("Push Data Page");
  uint64_t mem_addr = 0;
  uint32_t memory_page[PAGE_SIZE/4] = {0};
  for(int word = 0; word < PAGE_SIZE/4; word++) {
    memory_page[word] = 0xDEADBEEF;
  }
  pushPageToFPGA(&c, page_data_paddr, memory_page);
  makeMissReply(DATA_STORE, -1, pid, mem_addr, page_data_paddr, &pf_reply);
  sendMessageToFPGA(&c, &pf_reply, sizeof(pf_reply));

  INFO("Compare Instruction Page");
  uint8_t check_page_buffer[PAGE_SIZE] = {0};
  fetchPageFromFPGA(&c, page_inst_paddr, check_page_buffer);
  for(int i = 0; i < PAGE_SIZE; ++i){
    REQUIRE(check_page_buffer[i] == inst_page[i]);
  }
  fetchPageFromFPGA(&c, page_data_paddr, check_page_buffer);
  uint8_t *data_page = (uint8_t *)memory_page;
  for(int i = 0; i < PAGE_SIZE; ++i){
    REQUIRE(check_page_buffer[i] == data_page[i]);
  }

  // prepare for transplant.
  INFO("Transplant state to FPGA");
  REQUIRE(registerAndPushState(&c, thread_id, pid, &state) == 0);

  // start execution
  INFO("Start execution");
  REQUIRE(transplant_start(&c, thread_id) == 0);
 
  // First page fault here, it's instruction page.
  // Transplant back?
  INFO("Check thread state");

  uint32_t pending_threads = 0;
  while(!pending_threads) {
    REQUIRE(queryThreadState(&c, &pending_threads) == 0);
    usleep(1e5);
  }
  REQUIRE((pending_threads & (1 << thread_id)) != 0);

  // make it back
  INFO("Transplant thread back");
  transplantBack(&c, thread_id, &state);

  // check context
  INFO("Check context");
  REQUIRE(state.xregs[0] == 10);
  REQUIRE(state.xregs[1] == 0);
  REQUIRE(state.pc == pc + 4 * 6);

  // fetch the page back.
  INFO("Send Page Eviction Request");
  QEMUPageEvictRequest evict_request;
  makeEvictRequest(pid, mem_addr, &evict_request);
  sendMessageToFPGA(&c, &evict_request, sizeof(evict_request));
 
  // Let's query message. It should send an eviction message
  INFO("Query Eviction Notification");
  uint32_t buffer[16] = {0};
  queryMessageFromFPGA(&c, (uint8_t *)buffer);

  PageEvictNotification *evict_notify = (PageEvictNotification *) buffer;
  REQUIRE(evict_notify->type == sEvictNotify);
  REQUIRE(evict_notify->pid == pid);
  REQUIRE(evict_notify->vpn_hi == VPN_GET_LO(0));
  REQUIRE(evict_notify->vpn_lo == VPN_GET_LO(0));

  // Let's query message. It should send an eviction completed message
  INFO("Query Eviction Notification Complete");
  queryMessageFromFPGA(&c, (uint8_t *)buffer);

  evict_notify = (PageEvictNotification *) buffer;
  REQUIRE(evict_notify->type == sEvictDone);
  REQUIRE(evict_notify->pid == pid);
  REQUIRE(evict_notify->vpn_hi == VPN_GET_LO(0));
  REQUIRE(evict_notify->vpn_lo == VPN_GET_LO(0));
  REQUIRE(evict_notify->modified);
  REQUIRE(evict_notify->ppn == GET_PPN_FROM_PADDR(page_data_paddr));
  REQUIRE(evict_notify->permission == DATA_STORE);

  // fetch the page. It should in the page buffer.
  uint32_t data_buffer[PAGE_SIZE/4];
  INFO("Fetch page from FPGA");
  fetchPageFromFPGA(&c, page_data_paddr, &data_buffer);
  // CHECK its value
  REQUIRE(data_buffer[0] == 10);

  //usleep(3e6);

  releaseFPGAContext(&c);
}

TEST_CASE("execute-instruction") {
  // instruction page
  uint8_t inst_page[4096];
  // load binary file
  INFO("Load binary file");
  FILE *f = fopen("../src/client/tests/asm/executables/a.bin", "rb");
  REQUIRE(f != nullptr);
  REQUIRE(fread(inst_page, 1, 4096, f) != 0);
  fclose(f);

  // setup context
  INFO("Setup context");
  FPGAContext c;
  initFPGAContextAndPage(1, &c);
  uint64_t page_inst_paddr = c.base_address.page_base; // first page PA.
  uint64_t page_data_paddr = page_inst_paddr+PAGE_SIZE;
  ArmflexArchState state;
  uint64_t pc = 0x40000000;
  initArchState(&state, pc);

  // prepare for transplant.
  INFO("Transplant state to FPGA");
  uint32_t thread_id = 3;
  uint32_t pid = GET_PID(thread_id);
  REQUIRE(registerAndPushState(&c, thread_id, pid, &state) == 0);

  // start execution
  INFO("Start execution");
  REQUIRE(transplant_start(&c, thread_id) == 0);

  // First page fault here, it's instruction page.
  INFO("FPGA requires instruction page");
  uint32_t buffer[16] = {0};
  queryMessageFromFPGA(&c, (uint8_t *)buffer);

  INFO("Check page fault request");
  PageFaultNotification *page_fault = (PageFaultNotification *)buffer;
  REQUIRE(page_fault->type == sPageFaultNotify);
  REQUIRE(page_fault->permission == INST_FETCH);
  REQUIRE(page_fault->pid == pid);
  REQUIRE(page_fault->vpn_lo == VPN_GET_LO(state.pc));
  REQUIRE(page_fault->vpn_hi == VPN_GET_HI(state.pc));

  // Reply with the correct page.
  INFO("Send instruction page to FPGA");
  pushPageToFPGA(&c, page_inst_paddr, inst_page);
  QEMUMissReply pf_reply;
  makeMissReply(INST_FETCH, thread_id, pid, pc, page_inst_paddr, &pf_reply);
  sendMessageToFPGA(&c, &pf_reply, sizeof(pf_reply));

  // Check if the page is there.

  INFO("Compare Instruction Page");
  uint8_t check_page_buffer[PAGE_SIZE] = {0};
  fetchPageFromFPGA(&c, page_inst_paddr, check_page_buffer);
  for(int i = 0; i < PAGE_SIZE; ++i){
    REQUIRE(check_page_buffer[i] == inst_page[i]);
  }

  // Page fault again for data. 
  INFO("FPGA requires data page");
  REQUIRE(queryMessageFromFPGA(&c, (uint8_t *)buffer) == 0);

  INFO("Check page fault request");
  REQUIRE(page_fault->type == sPageFaultNotify);
  REQUIRE(page_fault->permission == DATA_STORE);
  REQUIRE(page_fault->pid == pid);
  REQUIRE(page_fault->vpn_lo == VPN_GET_LO(0l));
  REQUIRE(page_fault->vpn_hi == VPN_GET_HI(0l));

  // If so, reply with a empty page.
  INFO("Send data page to FPGA");
  pushPageToFPGA(&c, page_data_paddr, zero_page);
  makeMissReply(DATA_STORE, thread_id, pid, 0, page_data_paddr, &pf_reply);
  sendMessageToFPGA(&c, &pf_reply, sizeof(pf_reply));

  // Wait for Transplant back
  INFO("Check thread state");
  uint32_t pending_threads = 0;
  while(!pending_threads) {
    REQUIRE(queryThreadState(&c, &pending_threads) == 0);
    usleep(1e5);
  }
  REQUIRE((pending_threads & (1 << thread_id)) != 0);

  // make it back
  INFO("Transplant thread back");
  transplantBack(&c, thread_id, &state);

  // check context
  INFO("Check context");
  REQUIRE(state.xregs[0] == 10);
  REQUIRE(state.xregs[1] == 0);
  REQUIRE(state.pc == pc + 4 * 6);

  uint8_t data_buffer[4096];
  synchronizePage(&c, pid, data_buffer, 0, page_data_paddr, true);
  // CHECK its value
  INFO("Check final result");
  REQUIRE(data_buffer[0] == 10);

  //usleep(3e6);

  releaseFPGAContext(&c);
}

