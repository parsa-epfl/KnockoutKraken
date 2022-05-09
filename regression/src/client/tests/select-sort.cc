#include <cstdio>
#include <cstdlib>
extern "C" {
#include "../fpga.h"
#include "../fpga_interface.h"
#include "asm/asm_helpers.h"
}

#include "../test-helpers.hh"

TEST_CASE("select-sort-32-threads"){
  // 1. load the binary
  INFO("Prepare the binary");
  FILE *f = fopen("../src/client/tests/asm/executables/select_sort.bin", "rb");
  uint8_t instruction_page[4096];
  REQUIRE(f != nullptr);
  fread(instruction_page, 1, 4096, f);
  fclose(f);
  // 2. prepare the map table: each instruction has 1 pages.
  uint8_t a[32][4096];

  // they are randomly initialized.
  for(int thread_idx = 0; thread_idx < 32; ++thread_idx){
    for (int pe = 0; pe < 4096; ++pe){
      a[thread_idx][pe] = rand();
    }
  }
  
  // 3. prepare the architecture state.
  DevteroflexArchState state;
  initArchState(&state, 0);

  // 4. Run the experiment
  FPGAContext c;
  initFPGAContextAndPage(32, &c);
  pmuStartCounting(&c);
  for(int i = 0; i < 32; ++i){
    state.asid = i;
    state.xregs[0] = (i+1) * 0x1000;
    REQUIRE(transplantPushAndStart(&c, i, &state) == 0);
  }

  // handle the page fault. In theory, there should be 64 page faults.
  uint64_t data_page_pa[32];
  for(int i = 0; i < 64; ++i){
    MessageFPGA msg;
    MessageFPGA reply;
    mmuMsgGet(&c, &msg);
    REQUIRE(msg.type == sPageFaultNotify);
    if(msg.PageFaultNotif.permission == INST_FETCH){
      // They share the same page, but here I just make copies.
      REQUIRE(msg.PageFaultNotif.vpn_hi == 0);
      REQUIRE(msg.PageFaultNotif.vpn_lo == 0);
      dramPagePush(&c, c.ppage_base_addr + i * PAGE_SIZE, instruction_page);
      makeMissReply(
        INST_FETCH, 
        msg.PageFaultNotif.thid, 
        msg.PageFaultNotif.asid, 
        0,
        c.ppage_base_addr + i * PAGE_SIZE, 
        &reply
      );
      mmuMsgSend(&c, &reply);
    } else {
      // It's the data page.
      REQUIRE(msg.PageFaultNotif.vpn_hi == 0);
      REQUIRE(msg.PageFaultNotif.vpn_lo == (msg.PageFaultNotif.asid + 1));
      dramPagePush(&c, c.ppage_base_addr + i * PAGE_SIZE, a[msg.PageFaultNotif.asid]);
      makeMissReply(
        DATA_STORE,
        msg.PageFaultNotif.thid,
        msg.PageFaultNotif.asid,
        (msg.PageFaultNotif.asid + 1) * 0x1000,
        c.ppage_base_addr + i * PAGE_SIZE,
        &reply
      );
      mmuMsgSend(&c, &reply);
      data_page_pa[msg.PageFaultNotif.asid] = c.ppage_base_addr + i * PAGE_SIZE;
    }
  }

  // 5. wait for the program to be finished.
  uint32_t finished = 0;
  while(finished != 0xFFFFFFFF){
    uint32_t pd = 0;
    transplantWaitTillPending(&c, &pd);
    finished |= pd;
  }
  pmuStopCounting(&c);
  printPMUCounters(&c);

  // 6. check the result.
  for(int asid = 0; asid < 32; ++ asid){
    char p[4096];
    synchronizePage(&c, asid, (uint8_t *)p, (asid+1) * 0x1000, data_page_pa[asid], true);
    // make sure that p is ordered.
    for(int i = 0; i < 4095; ++i){
      REQUIRE(p[i] <= p[i+1]);
    }
  }
}