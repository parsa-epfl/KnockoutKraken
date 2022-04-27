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
    int paddr = ctx.ppage_base_addr;

    FILE *f = fopen("../src/client/tests/asm/executables/infinite-loop.bin", "rb");
    REQUIRE(f != nullptr);
    REQUIRE(fread(page, 1, 4096, f) != 0);
    fclose(f);

    INFO("Push instruction page");
    dramPagePush(&ctx, paddr, page);
    MessageFPGA pf_reply;
    makeMissReply(INST_FETCH, -1, asid, state.pc, paddr, &pf_reply);
    mmuMsgSend(&ctx, &pf_reply);

    INFO("Push state");
    state.asid = asid;
    transplantPushAndStart(&ctx, 0, &state);

    INFO("Advance");
    advanceTicks(&ctx, 100);

    INFO("Check transplants");
    uint32_t pending_threads;
    transplantPending(&ctx, &pending_threads);
    assert(!pending_threads);
 
    INFO("Advance");
    advanceTicks(&ctx, 100);

    INFO("Stop CPU");
    transplantStopCPU(&ctx, 0);

    INFO("Advance");
    advanceTicks(&ctx, 100);
 
    INFO("Check now execution stopped");
    transplantPending(&ctx, &pending_threads);
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
    int paddr = ctx.ppage_base_addr;

    FILE *f = fopen("../src/client/tests/asm/executables/infinite-loop.bin", "rb");
    REQUIRE(f != nullptr);
    REQUIRE(fread(page, 1, 4096, f) != 0);
    fclose(f);

    INFO("Push state");
    state.asid = asid;
    transplantPushAndStart(&ctx, 0, &state);

    INFO("Advance");
    advanceTicks(&ctx, 100);

    INFO("Check transplants");
    uint32_t pending_threads;
    transplantPending(&ctx, &pending_threads);
    assert(!pending_threads);
 
    INFO("Advance");
    advanceTicks(&ctx, 100);

    INFO("Stop CPU");
    transplantForceTransplant(&ctx, 0);

    INFO("Advance");
    advanceTicks(&ctx, 100);
 
    INFO("Check now execution stopped");
    transplantPending(&ctx, &pending_threads);
    assert(pending_threads);

    INFO("Check flag")
    transplantGetState(&ctx, 0, &state);
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
    state.asid = asid;
    int paddr = ctx.ppage_base_addr;

    INFO("Get binary")
    FILE *f = fopen("../src/client/tests/asm/executables/simple-inst.bin", "rb");
    REQUIRE(f != nullptr);
    REQUIRE(fread(page, 1, 4096, f) != 0);
    fclose(f);

    INFO("Push instruction page");
    dramPagePush(&ctx, paddr, page);
    MessageFPGA pf_reply;
    makeMissReply(INST_FETCH, -1, asid, state.pc, paddr, &pf_reply);
    mmuMsgSend(&ctx, &pf_reply);

    INFO("Push state");
    transplantPushAndSinglestep(&ctx, 0, &state);
    INFO("Push state");
    transplantPushAndSinglestep(&ctx, 1, &state);

    INFO("Advance");
    advanceTicks(&ctx, 1000);

    INFO("Check now execution stopped");
    uint32_t pending_threads;
    transplantPending(&ctx, &pending_threads);
    assert(pending_threads & 0b11);

    INFO("Check state 0");
    transplantGetState(&ctx, 0, &state);
    assert(state.xregs[2] = state.xregs[0] + state.xregs[1]);
    assert(state.xregs[2] = state.xregs[0] + state.xregs[1]);
    INFO("Check icount 0");
    assert(state.icount == 1);
    INFO("Check state 1");
    transplantGetState(&ctx, 1, &state);
    assert(state.xregs[2] = state.xregs[0] + state.xregs[1]);
    assert(state.xregs[2] = state.xregs[0] + state.xregs[1]);
    INFO("Check icount 1");
    assert(state.icount == 1);

    transplantPending(&ctx, &pending_threads);
    assert(pending_threads == 0);
 
    releaseFPGAContext(&ctx);
}

TEST_CASE("check-flag-undef") {
    FPGAContext ctx;
    DevteroflexArchState state;
    uint8_t page[PAGE_SIZE] = {0};
    REQUIRE(initFPGAContext(&ctx) == 0);
    initArchState(&state, 0xABCDABCD0000);
    initState_simple_inst(&state, 0x10, 0x33);
    int paddr = ctx.ppage_base_addr;

    INFO("Get binary");
    FILE *f = fopen("../src/client/tests/asm/executables/simple-inst.bin", "rb");
    REQUIRE(f != nullptr);
    REQUIRE(fread(page, 1, 4096, f) != 0);
    fclose(f);

    INFO("Push instruction page");
    dramPagePush(&ctx, paddr, page);
    MessageFPGA pf_reply;
    makeMissReply(INST_FETCH, -1, asid, state.pc, paddr, &pf_reply);
    mmuMsgSend(&ctx, &pf_reply);

    INFO("Push state");
    state.asid = asid;
    transplantPushAndWait(&ctx, 0, &state);

    INFO("Start Execution");
    transplantStart(&ctx, 0); 

    INFO("Advance");
    advanceTicks(&ctx, 500);

    INFO("Check now execution stopped");
    uint32_t pending_threads = 0;
    transplantPending(&ctx, &pending_threads);
    assert(pending_threads);

    INFO("Check transplant");
    transplantGetState(&ctx, 0, &state);
    printf("flags: %lx\n", state.flags);
    assert(FLAGS_GET_IS_UNDEF(state.flags));
    INFO("Check icount");
    assert(state.icount == 1);
 
    releaseFPGAContext(&ctx);
}

TEST_CASE("check-flag-transplant") {
    FPGAContext ctx;
    DevteroflexArchState state;
    uint8_t page[PAGE_SIZE] = {0};
    REQUIRE(initFPGAContext(&ctx) == 0);
    initArchState(&state, 0x0);
    initState_exception_br(&state);
    int paddr = ctx.ppage_base_addr;

    INFO("Get binary")
    FILE *f = fopen("../src/client/tests/asm/executables/exceptions.bin", "rb");
    REQUIRE(f != nullptr);
    REQUIRE(fread(page, 1, 4096, f) != 0);
    fclose(f);

    INFO("Push instruction page");
    dramPagePush(&ctx, paddr, page);
    MessageFPGA pf_reply;
    makeMissReply(INST_FETCH, -1, asid, state.pc, paddr, &pf_reply);
    mmuMsgSend(&ctx, &pf_reply);

    INFO("Push state");
    state.asid = asid;
    transplantPushAndWait(&ctx, 0, &state);

    INFO("Start Execution");
    transplantStart(&ctx, 0); 

    INFO("Advance");
    advanceTicks(&ctx, 1000);

    INFO("Check now execution stopped");
    uint32_t pending_threads = 0;
    transplantPending(&ctx, &pending_threads);
    assert(pending_threads);

    INFO("Check transplant");
    transplantGetState(&ctx, 0, &state);
    printf("flags: %lx\n", state.flags);
    assert(FLAGS_GET_IS_EXCEPTION(state.flags));
    INFO("Check icount");
    assert(state.icount == 0);
 
    releaseFPGAContext(&ctx);
}

TEST_CASE("check-icount-budget") {
    FPGAContext ctx;
    DevteroflexArchState state;
    uint8_t page[PAGE_SIZE] = {0};
    REQUIRE(initFPGAContext(&ctx) == 0);
    initArchState(&state, 0xABCDABCD0000);
    initState_infinite_loop(&state, true);
    int paddr = ctx.ppage_base_addr;

    FILE *f = fopen("../src/client/tests/asm/executables/infinite-loop.bin", "rb");
    REQUIRE(f != nullptr);
    REQUIRE(fread(page, 1, 4096, f) != 0);
    fclose(f);

    INFO("Push instruction page");
    dramPagePush(&ctx, paddr, page);
    MessageFPGA pf_reply;
    makeMissReply(INST_FETCH, -1, asid, state.pc, paddr, &pf_reply);
    mmuMsgSend(&ctx, &pf_reply);

    INFO("Push state");
    state.icountBudget = 100;
    state.asid = asid;
    transplantPushAndWait(&ctx, 0, &state);

    INFO("Advance");
    advanceTicks(&ctx, 100);

    INFO("Start Execution");
    transplantStart(&ctx, 0);

    INFO("Advance");
    advanceTicks(&ctx, 100);

    INFO("Check transplants");
    uint32_t pending_threads;
    transplantPending(&ctx, &pending_threads);
    assert(!pending_threads);
 
    INFO("Advance");
    advanceTicks(&ctx, 2000);
 
    INFO("Check now execution stopped");
    transplantPending(&ctx, &pending_threads);
    assert(pending_threads);

    INFO("Check transplant");
    transplantGetState(&ctx, 0, &state);
    printf("flags: %lx\n", state.flags);

    INFO("Check icount depletion");
    assert(FLAGS_GET_IS_ICOUNT_DEPLETED(state.flags));
    
    INFO("Check icount");
    assert(state.icount == 100);
 
    releaseFPGAContext(&ctx);
}