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
  REQUIRE(state1.icountBudget == state2.icountBudget);
}

void synchronizePage(FPGAContext *ctx, int asid, uint8_t *page, uint64_t vaddr, bool is_instruction_page,
                     uint64_t paddr, bool expect_modified) {
  int perm = 0;
  if(expect_modified) {
    perm = DATA_STORE;
  } else if(is_instruction_page) {
    perm = INST_FETCH;
  } else {
    perm = DATA_LOAD;
  }

  INFO("Synchronize page");
  MessageFPGA evict_request;
  makeEvictRequest(asid, vaddr, is_instruction_page, true, &evict_request);
  mmuMsgSend(ctx, &evict_request);

  // Let's query message. It should send an eviction message
  INFO("1. Query Eviction Notification");
  MessageFPGA message;
  mmuMsgGet(ctx, &message);

  REQUIRE(message.type == sEvictNotify);
  REQUIRE(message.asid == asid);
  REQUIRE(message.vpn_hi == VPN_GET_HI(vaddr));
  REQUIRE(message.vpn_lo == VPN_GET_LO(vaddr));
  REQUIRE(message.EvictNotif.ppn == GET_PPN_FROM_PADDR(paddr));

  INFO("2. Query Eviction Notification Complete");
  mmuMsgGet(ctx, &message);

  REQUIRE(message.type == sEvictDone);
  REQUIRE(message.asid == asid);
  REQUIRE(message.vpn_hi == VPN_GET_HI(vaddr));
  REQUIRE(message.vpn_lo == VPN_GET_LO(vaddr));
  REQUIRE(message.EvictDone.ppn == GET_PPN_FROM_PADDR(paddr));
  REQUIRE(message.EvictDone.permission == perm);
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

void printPMUCounters(const FPGAContext *ctx){
  uint64_t cyc = pmuTotalCycles(ctx);
  printf("Total cycles: %ld \n", cyc);
  uint64_t cmt = pmuTotalCommitInstructions(ctx);
  printf("Total committed instructions: %ld \n", cmt);
  printf("IPC: %lf CPI: %lf \n", double(cmt) / cyc, double(cyc) / double(cmt));
  puts("----------");

  const char *names[4] = {
    "DCachePenalty:",
    "TLBPenalty:",
    "TransplantPenalty:",
    "PageFaultPenalty:"
  };

  for(int idx = 0; idx < 4; ++idx){
    uint16_t penalties[16];
    REQUIRE(pmuReadCycleCounters(ctx, idx, penalties) == 0);
    puts(names[idx]);
    printf("Raw: ");
    for(int i = 0; i < 16; ++i){
      printf("%d ", penalties[i]);
    }
    puts("");
    uint32_t cnt_sum = 0;
    uint32_t cnt_non_zero = 0;

    if(idx == 2) {
      // for the trapsplant back penalty, we should clear the one that is caused by the last transplant back. 
      penalties[0] = 0;
    }

    for(int i = 0; i < 16; ++i){
      if(penalties[i] != 0){
        cnt_sum += penalties[i];
        cnt_non_zero += 1;
      }
    }
    if(cnt_non_zero != 0) printf("Average: %lf \n", double(cnt_sum) / cnt_non_zero);
    puts("----------");
  }
}

void expectPageFault(const FPGAContext *ctx, uint32_t asid, uint64_t vaddr, int perm) {
  MessageFPGA message;
  mmuMsgGet(ctx, &message);
  INFO("- Check page fault request");
  REQUIRE(message.type == sPageFaultNotify);
  REQUIRE(message.asid == asid);
  REQUIRE(message.vpn == VPN_ALIGN(vaddr));
  CHECK(message.PageFaultNotif.permission == perm);
}  // FPGA requires instruction page.
