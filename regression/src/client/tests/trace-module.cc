extern "C" {
#include "../fpga.h"
#include "../fpga_interface.h"
#include "asm/asm_helpers.h"
}

#include "../test-helpers.hh"

TEST_CASE("trace-pcs-then-stop-single") {
    FPGAContext ctx;
    DevteroflexArchState state;
    uint8_t page[PAGE_SIZE] = {0};
    REQUIRE(initFPGAContext(&ctx) == 0);
    initArchState(&state, 0);
    initState_infinite_loop(&state, true);
    int paddr = ctx.base_address.page_base;

    INFO("Get program page");
    FILE *f = fopen("../src/client/tests/asm/executables/infinite-loop.bin", "rb");
    REQUIRE(f != nullptr);
    REQUIRE(fread(page, 1, 4096, f) != 0);
    fclose(f);

    INFO("Push instruction page and state");
    pushPageToFPGA(&ctx, paddr, page);
    int thread = 0;
    MessageFPGA pf_reply;
    makeMissReply(INST_FETCH, -1, thread, state.pc, paddr, &pf_reply);
    sendMessageToFPGA(&ctx, &pf_reply, sizeof(pf_reply));
    registerAndPushState(&ctx, thread, thread, &state);

    INFO("Advance");
    advanceTicks(&ctx, 100);

    INFO("Start Execution");
    transplant_start(&ctx, thread);

    INFO("Advance");
    advanceTicks(&ctx, 100);

    INFO("Send start tracing, start and reset counters");
    trace_PC_set_paddr(&ctx, 0x10000);
    trace_PC_start(&ctx);
    trace_PC_counter_start(&ctx);
    trace_PC_counter_reset(&ctx);

    INFO("Advance and stop");
    advanceTicks(&ctx, 30000);
    trace_PC_counter_stop(&ctx);


    INFO("Get counters");
    uint32_t cntExecute;
    uint32_t cntStalls;
    trace_PC_counter_execute(&ctx, &cntExecute);
    trace_PC_counter_stalls(&ctx, &cntStalls);

    INFO("Stop CPU");
    transplant_stopCPU(&ctx, 0);

    INFO("Advance");
    advanceTicks(&ctx, 100);

    INFO("Print results");
    printf("Executing:%016d\n"
           "Stalls:   %016d\n", 
           cntExecute, cntStalls);
 
    releaseFPGAContext(&ctx);
}