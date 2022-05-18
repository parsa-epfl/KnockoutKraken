#include <cstdio>
#include <cstdlib>
extern "C" {
#include "../fpga.h"
#include "../fpga_interface.h"
#include "asm/asm_helpers.h"
}

#include "../test-helpers.hh"

#define MAX_THREAD_COUNT 32

static void select_sort_x_threads(size_t thidN) {
  // 1. Run the experiment
  FPGAContext c;
  initFPGAContextAndPage(thidN, &c);
 
  // 2. load the binary
  uint8_t instruction_page[PAGE_SIZE];
  makeDeadbeefPage(instruction_page, PAGE_SIZE);
  INFO("Prepare the binary");
  FILE *f = fopen("../src/client/tests/asm/executables/select-sort.bin", "rb");
  REQUIRE(f != nullptr);
  fread(instruction_page, 1, PAGE_SIZE, f);
  fclose(f);

  // 3. Prepare the map table: each instruction has 1 page
  uint8_t dataPages[MAX_THREAD_COUNT][PAGE_SIZE];

  // they are randomly initialized.
  for(int thread_idx = 0; thread_idx < thidN; ++thread_idx){
    for (int pe = 0; pe < PAGE_SIZE; ++pe) {
      dataPages[thread_idx][pe] = rand() % 128;
    }
  }
  
  // 3. prepare the architecture state
  DevteroflexArchState state[MAX_THREAD_COUNT];
  uint64_t data_page_pa[MAX_THREAD_COUNT] = {0};
  uint64_t data_page_va[MAX_THREAD_COUNT] = {0};
  uint64_t inst_page_pa[MAX_THREAD_COUNT] = {0};

  for(int thid = 0; thid < thidN; thid++) {
    initArchState(&state[thid], 0);
    data_page_va[thid] = (2*thid) * PAGE_SIZE;
    uint64_t pc = (2*thid+1) * PAGE_SIZE;
    data_page_pa[thid] = c.ppage_base_addr + (2*thid) * PAGE_SIZE;
    inst_page_pa[thid] = c.ppage_base_addr + (2*thid + 1) * PAGE_SIZE;
    initState_select_sort(&state[thid], thid, pc, data_page_va[thid]);
    REQUIRE(transplantPushAndStart(&c, thid, &state[thid]) == 0);
  }
  
  // 5. Setup PME counters
  pmuStartCounting(&c);

  // handle the page fault. In theory, there should be 64 page faults.
  MessageFPGA msg;
  MessageFPGA reply;
  
  INFO("Handling Page Faults");
  uint32_t pageFaults = 0;
  while(pageFaults < thidN * 2) {
    mmuMsgGet(&c, &msg);
    REQUIRE(msg.type == sPageFaultNotify);
    uint32_t thid = msg.PageFaultNotif.thid;
    if(msg.PageFaultNotif.permission == INST_FETCH){
      INFO("Received instruction page fault");
      uint64_t paddr = inst_page_pa[thid];
      REQUIRE(msg.vpn == VPN_ALIGN(state[thid].pc));
      dramPagePush(&c, paddr, instruction_page);
      makeMissReply(INST_FETCH, thid, msg.asid, state[thid].pc, paddr, &reply);
      mmuMsgSend(&c, &reply);
    } else {
      INFO("Received data page fault");
      uint64_t paddr = data_page_pa[thid];
      REQUIRE(msg.vpn == VPN_ALIGN(data_page_va[thid]));
      dramPagePush(&c, paddr, dataPages[thid]);
      makeMissReply(DATA_STORE, thid, msg.asid, data_page_va[thid], paddr, &reply);
      mmuMsgSend(&c, &reply);
    }
    pageFaults++;
  }

  printf("Total page faults: %u\n", pageFaults);

  // 5. wait for the program to be finished.
  size_t iterations = 0;
  uint32_t finished = 0;
  uint32_t pendingThreads = 0;
  while(finished != ((1UL << thidN) - 1)){
    pendingThreads = 0;
    REQUIRE(!mmuMsgHasPending(&c));
    transplantPending(&c, &pendingThreads);
    finished |= pendingThreads;
    printf("Threads completed: %x \n", finished);
    advanceTicks(&c, 1000);
    REQUIRE(iterations++ < 30);
  }

  pmuStopCounting(&c);
  printPMUCounters(&c);

  // 6. check the result.
  char pageFPGA[PAGE_SIZE];
  for(int thid = 0; thid < thidN; ++thid){
    synchronizePage(&c, state[thid].asid, (uint8_t *) pageFPGA, data_page_va[thid], data_page_pa[thid], true);
    // make sure that p is ordered.
    for(int i = 0; i < 15; ++i) {
      //printf("page[%i] <= page[%i]\n", i, i+1);
      REQUIRE(int (pageFPGA[i]) <= int (pageFPGA[i+1]));
    }
  }
}

TEST_CASE("select-sort-1-threads") {
  select_sort_x_threads(2);
}

TEST_CASE("select-sort-2-threads") {
  select_sort_x_threads(2);
}

TEST_CASE("select-sort-15-threads") {
  select_sort_x_threads(15);
}

TEST_CASE("select-sort-16-threads") {
  // TODO: Fails
  select_sort_x_threads(16);
}

// TODO TEST_CASE("select-sort-32-threads")