extern "C" {
#include "../fpga.h"
#include "../fpga_interface.h"
#include "asm/asm_helpers.h"
}

#include "../test-helpers.hh"

int asid = 0x10;
int thid = 0;

TEST_CASE("host-cmd-stop-cpu") {
    FPGAContext ctx;
    ArmflexArchState state;
    uint8_t page[PAGE_SIZE] = {0};
    REQUIRE(initFPGAContext(&ctx) == 0);
    initArchState(&state, 0xABCDABCD0000);
    initState_infinite_loop(&state, true);
    int paddr = ctx.base_address.page_base;

    FILE *f = fopen("../src/client/tests/asm/executables/infinite-loop.bin", "rb");
    REQUIRE(f != nullptr);
    REQUIRE(fread(page, 1, 4096, f) != 0);
    fclose(f);

    INFO("Push instruction page");
    pushPageToFPGA(&ctx, paddr, page);
    MessageFPGA pf_reply;
    makeMissReply(INST_FETCH, -1, asid, state.pc, paddr, &pf_reply);
    sendMessageToFPGA(&ctx, &pf_reply, sizeof(pf_reply));

    INFO("Push state");
    registerAndPushState(&ctx, 0, asid, &state);

    INFO("Advance");
    advanceTicks(&ctx, 100);

    INFO("Start Execution");
    transplant_start(&ctx, 0);

    INFO("Advance");
    advanceTicks(&ctx, 100);

    INFO("Check transplants");
    uint32_t pending_threads;
    transplant_pending(&ctx, &pending_threads);
    assert(!pending_threads);
 
    INFO("Advance");
    advanceTicks(&ctx, 100);

    INFO("Stop CPU");
    transplant_stopCPU(&ctx, 0);

    INFO("Advance");
    advanceTicks(&ctx, 100);
 
    INFO("Check now execution stopped");
    transplant_pending(&ctx, &pending_threads);
    assert(pending_threads);
 
    releaseFPGAContext(&ctx);
}
 