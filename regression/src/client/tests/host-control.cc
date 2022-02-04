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
    DevteroflexArchState state;
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
 
TEST_CASE("host-cmd-force-transplant") {
    FPGAContext ctx;
    DevteroflexArchState state;
    uint8_t page[PAGE_SIZE] = {0};
    REQUIRE(initFPGAContext(&ctx) == 0);
    initArchState(&state, 0xABCDABCD0000);
    initState_infinite_loop(&state, true);
    int paddr = ctx.base_address.page_base;

    FILE *f = fopen("../src/client/tests/asm/executables/infinite-loop.bin", "rb");
    REQUIRE(f != nullptr);
    REQUIRE(fread(page, 1, 4096, f) != 0);
    fclose(f);

    INFO("Push state");
    registerAndPushState(&ctx, 0, asid, &state);

    INFO("Advance");
    advanceTicks(&ctx, 100);

    INFO("Check transplants");
    uint32_t pending_threads;
    transplant_pending(&ctx, &pending_threads);
    assert(!pending_threads);
 
    INFO("Advance");
    advanceTicks(&ctx, 100);

    INFO("Stop CPU");
    transplant_forceTransplant(&ctx, 0);

    INFO("Advance");
    advanceTicks(&ctx, 100);
 
    INFO("Check now execution stopped");
    transplant_pending(&ctx, &pending_threads);
    assert(pending_threads);

    INFO("Check flag")
    transplantBack(&ctx, 0, &state);
    assert(FLAGS_GET_IS_EXCEPTION(state.flags));
 
    releaseFPGAContext(&ctx);
}

TEST_CASE("host-cmd-singlestep") {
    FPGAContext ctx;
    DevteroflexArchState state;
    uint8_t page[PAGE_SIZE] = {0};
    REQUIRE(initFPGAContext(&ctx) == 0);
    initArchState(&state, 0xABCDABCD0000);
    initState_simple_inst(&state, 0x10, 0x33);
    int paddr = ctx.base_address.page_base;

    INFO("Get binary")
    FILE *f = fopen("../src/client/tests/asm/executables/simple-inst.bin", "rb");
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
    INFO("Push state");
    registerAndPushState(&ctx, 1, asid, &state);

    INFO("Setup stop CPU");
    transplant_stopCPU(&ctx, 0);
    transplant_stopCPU(&ctx, 1);

    INFO("Advance");
    advanceTicks(&ctx, 100);

    INFO("Check transplants");
    uint32_t pending_threads;
    transplant_pending(&ctx, &pending_threads);
    assert(!pending_threads);

    INFO("Start Execution");
    transplant_start(&ctx, 0); 
    transplant_start(&ctx, 1); 

    INFO("Advance");
    advanceTicks(&ctx, 1000);

    INFO("Check now execution stopped");
    transplant_pending(&ctx, &pending_threads);
    assert(pending_threads & 0b11);

    transplantBack(&ctx, 0, &state);
    assert(state.xregs[2] = state.xregs[0] + state.xregs[1]);
    assert(state.xregs[2] = state.xregs[0] + state.xregs[1]);
    transplantBack(&ctx, 1, &state);
    assert(state.xregs[2] = state.xregs[0] + state.xregs[1]);
    assert(state.xregs[2] = state.xregs[0] + state.xregs[1]);
 
    releaseFPGAContext(&ctx);
}

TEST_CASE("check-flag-undef") {
    FPGAContext ctx;
    DevteroflexArchState state;
    uint8_t page[PAGE_SIZE] = {0};
    REQUIRE(initFPGAContext(&ctx) == 0);
    initArchState(&state, 0xABCDABCD0000);
    initState_simple_inst(&state, 0x10, 0x33);
    int paddr = ctx.base_address.page_base;

    INFO("Get binary")
    FILE *f = fopen("../src/client/tests/asm/executables/simple-inst.bin", "rb");
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

    INFO("Start Execution");
    transplant_start(&ctx, 0); 

    INFO("Advance");
    advanceTicks(&ctx, 1000);

    INFO("Check now execution stopped");
    uint32_t pending_threads = 0;
    transplant_pending(&ctx, &pending_threads);
    assert(pending_threads);

    INFO("Check transplant");
    transplantBack(&ctx, 0, &state);
    printf("flags: %lx\n", state.flags);
    assert(FLAGS_GET_IS_UNDEF(state.flags));
 
    releaseFPGAContext(&ctx);
}

TEST_CASE("check-flag-transplant") {
    FPGAContext ctx;
    DevteroflexArchState state;
    uint8_t page[PAGE_SIZE] = {0};
    REQUIRE(initFPGAContext(&ctx) == 0);
    initArchState(&state, 0x0);
    initState_exception_br(&state);
    int paddr = ctx.base_address.page_base;

    INFO("Get binary")
    FILE *f = fopen("../src/client/tests/asm/executables/exceptions.bin", "rb");
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

    INFO("Start Execution");
    transplant_start(&ctx, 0); 

    INFO("Advance");
    advanceTicks(&ctx, 1000);

    INFO("Check now execution stopped");
    uint32_t pending_threads = 0;
    transplant_pending(&ctx, &pending_threads);
    assert(pending_threads);

    INFO("Check transplant");
    transplantBack(&ctx, 0, &state);
    printf("flags: %lx\n", state.flags);
    assert(FLAGS_GET_IS_EXCEPTION(state.flags));
 
    releaseFPGAContext(&ctx);
}