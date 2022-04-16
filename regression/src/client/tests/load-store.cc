#include <cstdio>
extern "C" {
#include "../fpga.h"
#include "../fpga_interface.h"
#include "asm/asm_helpers.h"
}

#include "../test-helpers.hh"
#include <stdbool.h>

static void write_page_block(uint8_t *page, uint8_t *bytes, size_t addr,
                             size_t byteSize) {
  size_t offst = addr & (PAGE_SIZE - 1);
  for (size_t byte = 0; byte < byteSize; byte++)
    page[offst + byte] = bytes[byte];
}

static void init_mem_slot_pair(uint8_t *bytes, uint64_t lsb, uint64_t msb,
                               size_t byteSize) {
  uint8_t *lsb_bytes = (uint8_t *)&lsb;
  uint8_t *msb_bytes = (uint8_t *)&msb;
  for (size_t byte = 0; byte < byteSize; byte++) {
    if (byte < byteSize / 2) {
      bytes[byte] = lsb_bytes[byte];
    } else {
      bytes[byte] = msb_bytes[byte - (byteSize / 2)];
    }
  }
}

static void check_ldst_all_sizes_pair_load(DevteroflexArchState *state,
                                           uint64_t exp1, uint64_t exp2) {
  INFO("Check pair load values")
  REQUIRE(state->xregs[0] == exp1);
  REQUIRE(state->xregs[1] == exp2);
}

static void check_ldst_all_sizes_pair_store(uint8_t *page, uint64_t addr,
                                            uint64_t byteSize, uint64_t exp1,
                                            uint64_t exp2) {
  INFO("Check pair store values")
  size_t offst = addr & (PAGE_SIZE - 1);
  uint8_t *exp1_bytes = (uint8_t *)&exp1;
  uint8_t *exp2_bytes = (uint8_t *)&exp2;
  for (size_t byte = 0; byte < byteSize; byte++) {
    if (byte < byteSize / 2) {
      REQUIRE(page[offst + byte] == exp1_bytes[byte]);
    } else {
      REQUIRE(page[offst + byte] == exp2_bytes[byte - (byteSize / 2)]);
    }
  }
}

static void step_ldst_all_sizes_pair_load(FPGAContext *ctx,
                                          DevteroflexArchState *state,
                                          uint32_t thid, uint32_t asid,
                                          uint64_t exp1, uint64_t exp2) {
  uint32_t pending_threads;
  INFO("Push thread and start");
  transplantRegisterAndPush(ctx, thid, asid, state);
  REQUIRE(transplantStart(ctx, thid) == 0);
  INFO("Wait for transplant and fetch")
  transplantWaitTillPending(ctx, &pending_threads);
  REQUIRE(pending_threads & (1 << thid));
  transplantUnregisterAndPull(ctx, thid, state);
  check_ldst_all_sizes_pair_load(state, exp1, exp2);
  state->pc += 4; // Get next instruction
}

static int test_ldst_all_sizes_pair(FPGAContext *ctx) {
  INFO("- Init state")
  const int thid = 3;
  const int asid = (thid + 1) << 4;
  DevteroflexArchState state;
  uint8_t page[PAGE_SIZE] = {0};
  uint8_t bytes[32] = {0};
  uint64_t page_inst_paddr = 0x10000, page_data_ld_paddr = 0x20000,
           page_data_st_paddr = 0x30000;
  uint64_t pc = 0x2000, vaddr_ld = 0x6000, vaddr_st = 0xA000;
  uint64_t val1 = 0, val2 = 0;
  uint64_t step_size = 0x20;
  state.pc = pc;
  initState_ldst_all_sizes_pair(&state, vaddr_ld, vaddr_st, step_size);

  INFO("- Get page from binary")
  makeDeadbeefPage(page, PAGE_SIZE);
  FILE *f =
      fopen("../src/client/tests/asm/executables/ldst_all_sizes.bin", "rb");
  REQUIRE(f != nullptr);
  REQUIRE(fread(page, 1, 4096, f) != 0);
  fclose(f);

  INFO("- Push Instruction Page");
  MessageFPGA pf_reply;
  dramPagePush(ctx, page_inst_paddr, page);
  makeMissReply(INST_FETCH, -1, asid, pc, page_inst_paddr, &pf_reply);
  mmuMsgSend(ctx, &pf_reply);

  INFO("- Prepare Data page with load values")
  makeDeadbeefPage(page, PAGE_SIZE);
  // init_mem_slot_pair(bytes, 0xAB, 0xBA, 2);
  // write_page_block(page, bytes, vaddr_ld, 2);

  // init_mem_slot_pair(bytes, 0xCDCD, 0xDEDE, 4);
  // write_page_block(page, bytes, vaddr_ld + step_size, 4);

  init_mem_slot_pair(bytes, 0xABABABAB, 0xCDCDCDCD, 8);
  write_page_block(page, bytes, vaddr_ld, 8);

  init_mem_slot_pair(bytes, 0xCCCCAAAA11117777, 0x5555BBBB4444FFFF, 16);
  write_page_block(page, bytes, vaddr_ld + step_size, 16);

  INFO("- Push Data Page");
  dramPagePush(ctx, page_data_ld_paddr, page);
  makeMissReply(DATA_LOAD, -1, asid, vaddr_ld, page_data_ld_paddr, &pf_reply);
  mmuMsgSend(ctx, &pf_reply);

  INFO("- Push Store Page");
  makeDeadbeefPage(page, PAGE_SIZE);
  dramPagePush(ctx, page_data_st_paddr, page);
  makeMissReply(DATA_STORE, -1, asid, vaddr_st, page_data_st_paddr, &pf_reply);
  mmuMsgSend(ctx, &pf_reply);

  INFO("- Register thread")
  mmuRegisterTHID2ASID(ctx, thid, asid);

  // INFO("Verify load pair byte");
  // step_ldst_all_sizes_loads(ctx, &state, 0xAB, 0xBA);
  // INFO("Verify load pair half word");
  // step_ldst_all_sizes_loads(ctx, &state, 0xCDCD, 0xDEDE);
  INFO("- Verify load pair 32b");
  step_ldst_all_sizes_pair_load(ctx, &state, thid, asid, 0xABABABAB,
                                0xCDCDCDCD);
  INFO("- Verify load pair 64b");
  step_ldst_all_sizes_pair_load(ctx, &state, thid, asid, 0xCCCCAAAA11117777,
                                0x5555BBBB4444FFFF);

  INFO("- Fetch back store page")
  makeZeroPage(page);
  synchronizePage(ctx, asid, page, vaddr_st, page_data_st_paddr, true);

  // INFO("Verify store pair byte");
  // check_ldst_all_sizes_stores(page, 0, 8, 0xAB, 0xBA);
  // INFO("Verify store pair half word");
  // check_ldst_all_sizes_stores(page, step_size, 16, 0xCDCD, 0xDEDE);
  INFO("- Verify store pair 32b");
  check_ldst_all_sizes_pair_store(page, vaddr_st, 8, 0xABABABAB, 0xCDCDCDCD);
  INFO("- Verify store pair 64b");
  check_ldst_all_sizes_pair_store(page, vaddr_st + step_size, 16,
                                  0xCCCCAAAA11117777, 0x5555BBBB4444FFFF);

  return 0;
}

TEST_CASE("test-ldst-pair-all-sizes") {
  FPGAContext ctx;
  REQUIRE(initFPGAContext(&ctx) == 0);
  test_ldst_all_sizes_pair(&ctx);

  releaseFPGAContext(&ctx);
}

TEST_CASE("test-pressure-mmu-same-address") {
  FPGAContext ctx;
  REQUIRE(initFPGAContext(&ctx) == 0);
  for (int i = 0; i < 10; i++)
    test_ldst_all_sizes_pair(&ctx);

  releaseFPGAContext(&ctx);
}

TEST_CASE("out-of-page-bound-pair-load") {
  /**
   * Steps:
   * - Pair load  0x2FF8 and 0x3000 (Required pages: 0x2000, 0x3000)
   * - Pair store 0x30F8 and 0x4000 (Required pages: 0x3000, 0x4000)
   * - Check identical state.
   */
  INFO("- Init state");

  FPGAContext ctx;
  REQUIRE(initFPGAContext(&ctx) == 0);

  const int thid = 3;
  const int asid = (thid + 1) << 4;
  uint8_t page[PAGE_SIZE] = {0};

  DevteroflexArchState state;
  state.pc = 0x1000;
  uint64_t inst_pa = ctx.ppage_base_addr;
  uint64_t load_vas[] = {0x2FFC, 0x3000};
  uint64_t store_vas[] = {0x3FFC, 0x4000};

  // Bind addresses to specific registers.
  state.xregs[2] = load_vas[0];
  state.xregs[3] = store_vas[0];
  state.xregs[4] = 0; // repeat the operation to I1.

  // Push instruction page.
  INFO("- Get page from binary")
  makeDeadbeefPage(page, PAGE_SIZE);
  FILE *f =
      fopen("../src/client/tests/asm/executables/ldst_all_sizes.bin", "rb");
  REQUIRE(f != nullptr);
  REQUIRE(fread(page, 1, 4096, f) != 0);
  fclose(f);

  INFO("- Start execution");
  REQUIRE(transplantRegisterAndPush(&ctx, thid, asid, &state) == 0);
  REQUIRE(transplantStart(&ctx, thid) == 0);

  // FPGA requires instruction page.
  MessageFPGA message;
  mmuMsgGetForce(&ctx, &message);
  INFO("- Check page fault request");
  REQUIRE(message.type == sPageFaultNotify);
  REQUIRE(message.asid == asid);
  REQUIRE(message.vpn_lo == VPN_GET_LO(state.pc));
  REQUIRE(message.vpn_hi == VPN_GET_HI(state.pc));
  REQUIRE(message.PageFaultNotif.permission == INST_FETCH);

  INFO("- Push instruction page");
  MessageFPGA pf_reply;
  dramPagePush(&ctx, inst_pa, page);
  makeMissReply(INST_FETCH, thid, asid, state.pc, inst_pa, &pf_reply);
  mmuMsgSend(&ctx, &pf_reply);

  // Then, there should be three page faults.
  uint64_t expected_vas[] = {0x2000, 0x3000, 0x4000};
  uint64_t pas[] = {
      inst_pa + 1 * PAGE_SIZE,
      inst_pa + 2 * PAGE_SIZE,
      inst_pa + 3 * PAGE_SIZE,
  };

  uint32_t data_pages[3][PAGE_SIZE / 4] = {0};
  data_pages[0][PAGE_SIZE/4 - 1] = 0xAABBCCDD;
  data_pages[1][0] = 0x99887766;

  int expected_access_types[] = {DATA_LOAD, DATA_LOAD, DATA_STORE};
  for(int i = 0; i < 3; ++i){
    INFO("- Query " << i << " page fault message");
    mmuMsgGetForce(&ctx, &message);
    REQUIRE(message.type == sPageFaultNotify);
    REQUIRE(message.asid == asid);
    REQUIRE(message.vpn_lo == VPN_GET_LO(expected_vas[i]));
    REQUIRE(message.vpn_hi == VPN_GET_HI(expected_vas[i]));
    CHECK(message.PageFaultNotif.permission == expected_access_types[i]);

    INFO("- Resolve " << i << " page fault");
    dramPagePush(&ctx, pas[i], data_pages[i]);
    makeMissReply(expected_access_types[i], thid, asid, expected_vas[i], pas[i], &pf_reply);
    mmuMsgSend(&ctx, &pf_reply);
  }

  INFO("- Transplat back");
  uint32_t pending_threads = 0;
  while(!pending_threads) {
    REQUIRE(transplantPending(&ctx, &pending_threads) == 0);
    usleep(1e5);
  }
  REQUIRE((pending_threads & (1 << thid)) != 0);
  transplantUnregisterAndPull(&ctx, thid, &state);

  synchronizePage(&ctx, asid, (uint8_t *)data_pages[1], expected_vas[1], pas[1], true);
  synchronizePage(&ctx, asid, (uint8_t *)data_pages[2], expected_vas[2], pas[2], true);

  INFO("- Check context");
  REQUIRE(state.xregs[0] == 0xAABBCCDD);
  REQUIRE(state.xregs[1] == 0x99887766);
  REQUIRE(state.xregs[2] == load_vas[0]);
  REQUIRE(state.xregs[3] == store_vas[0]);
  REQUIRE(state.xregs[4] == 0);

  REQUIRE((data_pages[1][0] & 0xFFFFFFFF) == 0xAABBCCDD);
  REQUIRE((data_pages[2][0] & 0xFFFFFFFF) == 0x99887766);

  releaseFPGAContext(&ctx);
}

TEST_CASE("ldr-wback-addr") {
    FPGAContext ctx;
    DevteroflexArchState state;
    REQUIRE(initFPGAContext(&ctx) == 0);
    initArchState(&state, 0x0);
    int asid = 0x10;

    INFO("Write instruction")
    int paddr = ctx.ppage_base_addr;
    uint8_t page[PAGE_SIZE] = {0}; 
    makeDeadbeefPage(page, PAGE_SIZE);
    ((uint32_t *) page)[0] = 0x38401c08; // ldrb    w8, [x0, #1]!
    ((uint32_t *) page)[1] = 0x0; // Trigger transplant
    state.xregs[8] = 0xDEADBEEF;
    state.xregs[0] = 0xFFFF7e722b2;
    uint64_t addr = state.xregs[0] + 1;

    MessageFPGA pf_reply;

    INFO("Push instruction page");
    dramPagePush(&ctx, paddr, page);
    makeMissReply(INST_FETCH, -1, asid, state.pc, paddr, &pf_reply);
    mmuMsgSend(&ctx, &pf_reply);

    INFO("Push data page");
    page[addr & 0xFFF] = 0x11;
    dramPagePush(&ctx, paddr + PAGE_SIZE, page);
    makeMissReply(DATA_LOAD, -1, asid, addr, paddr + PAGE_SIZE, &pf_reply);
    mmuMsgSend(&ctx, &pf_reply);

    INFO("Push state");
    transplantRegisterAndPush(&ctx, 0, asid, &state);

    INFO("Start Execution");
    transplantStart(&ctx, 0); 

    INFO("Advance");
    advanceTicks(&ctx, 200);

    INFO("Check now execution stopped");
    uint32_t pending_threads = 0;
    transplantPending(&ctx, &pending_threads);
    assert(pending_threads);

    INFO("Check transplant");
    transplantUnregisterAndPull(&ctx, 0, &state);
    INFO("Check address");
    assert(state.xregs[0] == addr);
    INFO("Check data");
    assert(state.xregs[8] == 0x11);
 
    releaseFPGAContext(&ctx);
}
