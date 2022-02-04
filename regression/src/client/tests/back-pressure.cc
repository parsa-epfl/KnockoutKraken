extern "C" {
  #include "../fpga.h"
  #include "../fpga_interface.h"
  #include "asm/asm_helpers.h"
}

#include "../test-helpers.hh"

constexpr int nMaxPagesPerThreads = 1024;

static inline void tagPage(uint8_t *pages, uint64_t thid, size_t nbPages) {
  uint64_t *pages64 = (uint64_t *) pages;
  for(size_t nPage = 0; nPage < nbPages; nPage++) {
    for(size_t word = 0; word < PAGE_SIZE/8; word++) {
      pages64[nPage*(PAGE_SIZE/8) + word] = (thid << 56) | (nPage << 50) | word;
    }
  }
}

static inline void checkPagePerWord(uint8_t *page_expect, uint8_t *page_actual) {
  uint64_t *page_expect64 = (uint64_t *) page_expect;
  uint64_t *page_actual64 = (uint64_t *) page_actual;
  for(size_t word = 0; word < PAGE_SIZE/8; word++) {
      REQUIRE(page_expect64[word] == page_actual64[word]);
  }
}


static int test_stress_memory(
  FPGAContext *ctx, 
  uint32_t thidN, 
  uint32_t nDataPagesPerThread, 
  bool safeCheckTransplants
) {
  INFO("- Init state")
  REQUIRE(thidN <= 32);
  DevteroflexArchState fetchedState;
  uint32_t threadsMask = ((uint64_t) 1 <<  thidN) - 1;
  uint8_t page[PAGE_SIZE] = {0};
  uint8_t deadbeefedPage[PAGE_SIZE] = {0};
  uint64_t pc = 0x3000, vaddr_ld = 0x1000000, vaddr_st = 0xF000000;
  uint64_t page_inst_paddr = 0x10000, 
           page_data_ld_paddr = 0x20000, 
           page_data_st_paddr = page_data_ld_paddr + thidN*nDataPagesPerThread*PAGE_SIZE;


  DevteroflexArchState state[32];
  uint8_t *pages = (uint8_t *) malloc(thidN*nDataPagesPerThread*PAGE_SIZE);
  uint32_t asid[32] = {0};
  uint64_t curr_ld_page_paddr[32] = {0};
  uint64_t curr_ld_page_vaddr[32] = {0};
  uint64_t curr_st_page_paddr[32] = {0};
  uint64_t curr_st_page_vaddr[32] = {0};
  uint32_t curr_ld_page_idx[32] = {0};
  uint32_t curr_st_page_idx[32] = {0};

  makeDeadbeefPage((uint8_t *) pages, thidN*nDataPagesPerThread*PAGE_SIZE);
  makeDeadbeefPage(deadbeefedPage, PAGE_SIZE);
  makeDeadbeefPage(page, PAGE_SIZE);

  INFO("- Get page from binary");
  FILE *f = fopen("../src/client/tests/asm/executables/pressure_ldp_stp.bin", "rb");
  REQUIRE(f != nullptr);
  REQUIRE(fread(page, 1, 4096, f) != 0);
  fclose(f);

  INFO("- Register all the threads");
  for(uint32_t thid = 0; thid < thidN; thid++) {
    printf("Registering thid:%i --\n", thid);
    initArchState(&state[thid], 0);
    asid[thid] = GET_asid(thid);
    curr_ld_page_paddr[thid] = page_data_ld_paddr + thid*nDataPagesPerThread*PAGE_SIZE;
    curr_st_page_paddr[thid] = page_data_st_paddr + thid*nDataPagesPerThread*PAGE_SIZE;
    curr_ld_page_vaddr[thid] = vaddr_ld;
    curr_st_page_vaddr[thid] = vaddr_st;
    curr_ld_page_idx[thid] = 0;
    curr_st_page_idx[thid] = 0;
    tagPage(&pages[thid*nDataPagesPerThread*PAGE_SIZE], thid, nDataPagesPerThread);
    initState_pressure_ldp_stp(
      &state[thid], 
      nDataPagesPerThread*PAGE_SIZE,
      curr_ld_page_vaddr[thid], 
      curr_st_page_vaddr[thid]);
    state[thid].pc = safeCheckTransplants ? pc : pc+4;
    registerAndPushState(ctx, thid, asid[thid], &state[thid]);
  }

  INFO("- Push Instruction Pages to each thread");
  MessageFPGA pf_reply;
  pushPageToFPGA(ctx, page_inst_paddr, page);
  for(uint32_t thid = 0; thid < thidN; thid++) {
    makeMissReply(INST_FETCH, -1, asid[thid], pc, page_inst_paddr, &pf_reply);
    sendMessageToFPGA(ctx, &pf_reply, sizeof(pf_reply));
  }

  uint32_t completed = 0;
  uint32_t pending_threads = 0;
  uint32_t iterations = 0;
  INFO("- Check all the threads transplanted correctly");
  if(safeCheckTransplants) {
    INFO("Start programs");
    for(uint32_t thid = 0; thid < thidN; thid++) {
      REQUIRE(transplant_start(ctx, thid) == 0);
    }
    INFO("Wait for all threads to request transplant");
    while(completed != threadsMask) {
      transplant_pending(ctx, &pending_threads);
      if(pending_threads)  {
        transplant_freePending(ctx, pending_threads);
        completed |= pending_threads;
      }
      advanceTicks(ctx, 1000);
      REQUIRE(iterations < 100);
    }
    INFO("Verify state and push back programs to start benchmark");
    for(uint32_t thid = 0; thid < thidN; thid++) {
      transplantBack(ctx, thid, &fetchedState);
      requireStateIsIdentical(fetchedState, state[thid]);
      state[thid].pc += 4;
    }
  } else {
    INFO("Skipped");
  }

  INFO("- Start programs");
  for(uint32_t thid = 0; thid < thidN; thid++) {
   registerAndPushState(ctx, thid, asid[thid], &state[thid]);
   REQUIRE(transplant_start(ctx, thid) == 0);
  }

  INFO("- Wait program complete");
  completed = 0;
  pending_threads = 0;
  iterations = 0;
  MessageFPGA message;
  while(completed != threadsMask) {
    transplant_pending(ctx, &pending_threads);
    if(pending_threads)  {
      INFO("Detected pending transplants");
      transplant_freePending(ctx, pending_threads);
      completed |= pending_threads;
      printf("Detected pending transplants: 0x%x, curr completed: 0x%x\n", pending_threads, completed);
    }
    if(hasMessagePending(ctx)) {
      getMessagePending(ctx, (uint8_t *) &message);
      REQUIRE(message.type == sPageFaultNotify); // NOTE: running out of entries in a PT set could send another request type
      if(message.PageFaultNotif.permission == DATA_LOAD) {
        INFO("Load page fault detected");
        uint32_t thid = message.PageFaultNotif.thid;
        uint32_t pageIdx = curr_ld_page_idx[thid];
        uint64_t paddr = curr_ld_page_paddr[thid];
        uint64_t vaddr = curr_ld_page_vaddr[thid];
        REQUIRE(message.asid == asid[thid]);
        REQUIRE(message.vpn_hi == VPN_GET_HI(vaddr));
        REQUIRE(message.vpn_lo == VPN_GET_LO(vaddr));
        //printf("Thread[%i]:PAGE_FAULT:VA[%lx]\n", thid, vaddr);
        pushPageToFPGA(ctx, paddr, &pages[thid*nDataPagesPerThread*PAGE_SIZE+pageIdx*PAGE_SIZE]);
        makeMissReply(DATA_LOAD, thid, asid[thid], vaddr, paddr, &pf_reply);
        printf("Thread[%i]:MISS_REPLY:VA[0x%016lx]->PA[0x%016lx]\n", thid, vaddr, paddr);
        sendMessageToFPGA(ctx, &pf_reply, sizeof(pf_reply));
        curr_ld_page_paddr[thid]+=PAGE_SIZE;
        curr_ld_page_vaddr[thid]+=PAGE_SIZE;
        curr_ld_page_idx[thid]++;
      } else if (message.PageFaultNotif.permission == DATA_STORE) {
        INFO("Store page fault detected");
        uint32_t thid = message.PageFaultNotif.thid;
        uint32_t pageIdx = curr_st_page_idx[thid];
        uint64_t paddr = curr_st_page_paddr[thid];
        uint64_t vaddr = curr_st_page_vaddr[thid];
        REQUIRE(message.asid == asid[thid]);
        REQUIRE(message.vpn_hi == VPN_GET_HI(vaddr));
        REQUIRE(message.vpn_lo == VPN_GET_LO(vaddr));
        pushPageToFPGA(ctx, paddr, &pages[thid*nDataPagesPerThread*PAGE_SIZE+pageIdx*PAGE_SIZE]);
        makeMissReply(DATA_STORE, thid, asid[thid], vaddr, paddr, &pf_reply);
        sendMessageToFPGA(ctx, &pf_reply, sizeof(pf_reply));
        curr_st_page_paddr[thid]+=PAGE_SIZE;
        curr_st_page_vaddr[thid]+=PAGE_SIZE;
        curr_st_page_idx[thid]++;
      } else {
        REQUIRE(false); // Program should fit in single page
      }
      advanceTicks(ctx, 1000);
      iterations++;
      REQUIRE(iterations < (1000 * thidN * nDataPagesPerThread));
    }
  }

  INFO("- Verify all store pages");
  for(size_t currPage = 0; currPage < nDataPagesPerThread; currPage++) {
    for(uint32_t thid = 0; thid < thidN; thid++) {
      makeZeroPage(page);
      uint64_t vaddr = vaddr_st + currPage * PAGE_SIZE;
      uint64_t base_paddr = page_data_st_paddr + thid * nDataPagesPerThread * PAGE_SIZE;
      uint64_t paddr = base_paddr + currPage * PAGE_SIZE;
      INFO("Synchronizing Page");
      synchronizePage(ctx, asid[thid], page, vaddr, paddr, true);
      INFO("Checking Page");
      checkPagePerWord(&pages[thid*nDataPagesPerThread*PAGE_SIZE + currPage*PAGE_SIZE], page);
    }
  }
  // */

  return 0;
}

TEST_CASE("test-pressure-ldp-stp-single") {
  FPGAContext ctx;
  REQUIRE(initFPGAContext(&ctx) == 0);
  test_stress_memory(&ctx, 1, 1, true);

  releaseFPGAContext(&ctx);
}

TEST_CASE("test-pressure-ldp-stp-short") {
  FPGAContext ctx;
  REQUIRE(initFPGAContext(&ctx) == 0);
  test_stress_memory(&ctx, 4, 1, true);

  releaseFPGAContext(&ctx);
}

TEST_CASE("test-pressure-ldp-stp-max-thread-short") {
  FPGAContext ctx;
  REQUIRE(initFPGAContext(&ctx) == 0);
  test_stress_memory(&ctx, 32, 1, false);

  releaseFPGAContext(&ctx);
}