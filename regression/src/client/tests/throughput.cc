extern "C" {
  #include "../fpga.h"
  #include "../fpga_interface.h"
  #include "asm/asm_helpers.h"
}

#include "../test-helpers.hh"

#define get_asid(thid) (thid << 4)

static int run_thread_test(FPGAContext *ctx, int threads) {
    uint8_t page[PAGE_SIZE] = {0};
    uint8_t deadbeefedPage[PAGE_SIZE] = {0};

    INFO("Get and push instruction page");
    int paddr = ctx->ppage_base_addr;
    FILE *f = fopen("../src/client/tests/asm/executables/infinite-loop.bin", "rb");
    REQUIRE(f != nullptr);
    REQUIRE(fread(page, 1, 4096, f) != 0);
    fclose(f);

    dramPagePush(ctx, paddr, page);

    DevteroflexArchState state;
    INFO("Prepare states");
    for (uint32_t thid = 0; thid < threads; thid++) {
        initArchState(&state, thid << 12);
        initState_infinite_loop(&state, true);

        INFO("Push instruction page");
        MessageFPGA pf_reply;
        makeMissReply(INST_FETCH, -1, thid << 4, state.pc, paddr, &pf_reply);
        mmuMsgSend(ctx, &pf_reply, sizeof(pf_reply));

        INFO("Push state");
        transplantRegisterAndPush(ctx, thid, get_asid(thid), &state);
    }

    INFO("Start threads");
    for (uint32_t thid = 0; thid < threads; thid++) {
        transplantStart(ctx, thid);
    }

    INFO("Wait for warmup");
    advanceTicks(ctx, 1000);

    INFO("Start counter");
    // TDB


    INFO("Advance Cycles")
    advanceTicks(ctx, 1000000);

    INFO("Stop counter");
    // TDB


    INFO("Throughput results");

    releaseFPGAContext(ctx);
}

TEST_CASE("increase-throughput") {
    FPGAContext ctx;
    uint8_t page[PAGE_SIZE] = {0};
    REQUIRE(initFPGAContext(&ctx) == 0);
}
