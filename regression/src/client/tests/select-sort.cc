#include <cstdio>
#include <cstdlib>
extern "C" {
#include "../fpga.h"
#include "../fpga_interface.h"
#include "asm/asm_helpers.h"
}

#include "../test-helpers.hh"

#define MAX_THREAD_COUNT 32

static void select_sort_x_threads(size_t thidN, bool run_1024 = false) {
  // 1. Run the experiment
  FPGAContext c;
  initFPGAContextAndPage(thidN, &c);
 
  // 2. load the binary
  uint8_t instruction_page[PAGE_SIZE];
  makeDeadbeefPage(instruction_page, PAGE_SIZE);
  INFO("Prepare the binary");
  FILE *f = nullptr;
  if (run_1024) {
    f = fopen("../src/client/tests/asm/executables/select-sort-1024.bin", "rb");
  } else {
    f = fopen("../src/client/tests/asm/executables/select-sort.bin", "rb");
  }
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

  // 3.1 Small test: Whether the first page of the DRAM is zero?
  uint8_t first_page[4096];
  dramPagePull(&c, 0, first_page);

  for(int i = 0; i < 4096; ++i){
    if(first_page[i] != 0){
      printf("Warning: detect a non-zero byte in the first page: byte at 0x%x, and its value is 0x%x \n", i, first_page[i]);
    }
    first_page[i] = 0;
  }

  // sync the page back
  dramPagePush(&c, 9, first_page);
  
  // 3.2. prepare the architecture state
  DevteroflexArchState state[MAX_THREAD_COUNT];
  uint64_t data_page_pa[MAX_THREAD_COUNT] = {0};
  uint64_t data_page_va[MAX_THREAD_COUNT] = {0};
  uint64_t inst_page_pa[MAX_THREAD_COUNT] = {0};

  for(int thid = 0; thid < thidN; thid++) {
    initArchState(&state[thid], 0);
    // this is changed, because the first 512bit of FPGA DRAM is not zero, even if it's reset???
    data_page_va[thid] = (4 * thid + 2) * PAGE_SIZE;
    // avoid using the first 512 bits.
    state[thid].asid = thid * 3 + 1;
    uint64_t pc = (4 * thid + 1) * PAGE_SIZE;
    data_page_pa[thid] = c.ppage_base_addr + (4 * thid + 2) * PAGE_SIZE;
    inst_page_pa[thid] = c.ppage_base_addr + (4 * thid + 1) * PAGE_SIZE;
    initState_select_sort(&state[thid], thid, pc, data_page_va[thid]);
    REQUIRE(transplantPushAndStart(&c, thid, &state[thid]) == 0);
    printf("Dispatch thread %d \n", thid);
  }
  
  // 4. Setup PME counters
  pmuStartCounting(&c);

  // handle the page fault. In theory, there should be 64 page faults.
  MessageFPGA msg;
  MessageFPGA reply;
  
  puts("Handling Page Faults");
  uint32_t pageFaults = 0;
  while(pageFaults < thidN * 2) {
    // mmuMsgGet(&c, &msg);
    if (!mmuMsgHasPending(&c)){
      // there should be no instruction pending.
      uint32_t pendingThreads = 0;
      REQUIRE(transplantPending(&c, &pendingThreads) == 0);
      if(pendingThreads != 0){
        uint32_t thid = 0;
        while((pendingThreads & 0x1) == 0){
          thid += 1;
          pendingThreads = pendingThreads >> 1;
        }
        // print the transplant PC
        // I force the transplant.
        DevteroflexArchState local_state;
        REQUIRE(transplantGetState(&c, thid, &local_state) == 0);
        // print what happens.
        printf("Transplant Detected. PC=%lu; transplantType[%lu] \n", local_state.pc, local_state.flags);
        REQUIRE(false);
      }
      continue;
    }
    REQUIRE(mmuMsgGet(&c, &msg) == 0);
    REQUIRE(msg.type == sPageFaultNotify);
    uint32_t thid = msg.PageFaultNotif.thid;
    if(msg.PageFaultNotif.permission == INST_FETCH){
#ifndef AWS_FPGA
      // Log is only enabled for the simulator, since puts itself can influence the FPGA performance.
      puts("Received instruction page fault");
#endif
      uint64_t paddr = inst_page_pa[thid];
      REQUIRE(msg.vpn == VPN_ALIGN(state[thid].pc));
      REQUIRE(dramPagePush(&c, paddr, instruction_page) == 0);
      makeMissReply(INST_FETCH, thid, msg.asid, state[thid].pc, paddr, &reply);
      REQUIRE(mmuMsgSend(&c, &reply) == 0);
    } else {
#ifndef AWS_FPGA
      puts("Received data page fault");
#endif
      uint64_t paddr = data_page_pa[thid];
      REQUIRE(msg.vpn == VPN_ALIGN(data_page_va[thid]));
      REQUIRE(dramPagePush(&c, paddr, dataPages[thid]) == 0);
      makeMissReply(DATA_STORE, thid, msg.asid, data_page_va[thid], paddr, &reply);
      REQUIRE(mmuMsgSend(&c, &reply) == 0);
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
#ifndef AWS_FPGA
    printf("Threads completed: %x \n", finished);
#endif
    advanceTicks(&c, 1000);
  }

  pmuStopCounting(&c);
  printPMUCounters(&c);

  // 6. check the transplant reason.
  for(int thid = 0; thid < thidN; ++thid) {
    transplantGetState(&c, thid, &state[thid]);
    REQUIRE((state[thid].pc & 0xFFF) == 76);
  }
 
  // 7. check the result.
  char pageFPGA[PAGE_SIZE];
  for(int thid = 0; thid < thidN; ++thid){
    synchronizePage(&c, state[thid].asid, (uint8_t *) pageFPGA, data_page_va[thid], data_page_pa[thid], true);
    // make sure that p is ordered.
    for(int i = 0; i < (run_1024 ? 1023 : 15); ++i) {
      //printf("page[%i] <= page[%i]\n", i, i+1);
      REQUIRE(int (pageFPGA[i]) <= int (pageFPGA[i+1]));
    }
  }
}

TEST_CASE("select-sort-1-threads") {
  select_sort_x_threads(1);
}

TEST_CASE("select-sort-1024-elements-1-threads") {
  select_sort_x_threads(1, true);
}

TEST_CASE("select-sort-2-threads") {
  select_sort_x_threads(2);
}

TEST_CASE("select-sort-1024-elements-2-threads") {
  select_sort_x_threads(2, true);
}


TEST_CASE("select-sort-15-threads") {
  select_sort_x_threads(15);
}

TEST_CASE("select-sort-1024-elements-15-threads") {
  select_sort_x_threads(15, true);
}

TEST_CASE("select-sort-16-threads") {
  // TODO: Fails
  select_sort_x_threads(16);
}

// TODO TEST_CASE("select-sort-32-threads")