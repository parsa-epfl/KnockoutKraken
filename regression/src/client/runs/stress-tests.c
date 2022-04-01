#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "../fpga.h"
#include "../fpga_interface.h"
#include "../tests/asm/asm_helpers.h"
#include "../fpga_helpers.h"

int main(int argc, char **argv) {
    assert(argc != 1);
    int threads = atoi(argv[1]);
    printf("Starting with %i threads\n", threads);
    FPGAContext ctx;
    DevteroflexArchState state;
    uint8_t page[PAGE_SIZE] = {0};
    int ret = initFPGAContext(&ctx);
    printf("initFPGAContext retuned %i, should be 0\n", ret);
    int paddr = ctx.ppage_base_addr;

    printf("Get program page\n");
    FILE *f = fopen("../src/client/tests/asm/executables/infinite-loop.bin", "rb");
    assert(f != NULL);
    assert(fread(page, 1, 4096, f) != 0);
    fclose(f);

    printf("Push instruction page and state\n");
    dramPagePush(&ctx, paddr, page);
    for(int thread = 0; thread < threads; thread++) {
        initArchState(&state, thread << 12);
        initState_infinite_loop(&state, true);
        transplantRegisterAndPush(&ctx, thread, thread, &state);
        MessageFPGA pf_reply;
        makeMissReply(INST_FETCH, -1, thread, state.pc, paddr, &pf_reply);
        mmuMsgSend(&ctx, &pf_reply);
    }

    printf("Advance\n");
    advanceTicks(&ctx, 100);

    printf("Start Execution\n");
    for(int thread = 0; thread < threads; thread++) {
        transplantStart(&ctx, thread);
    }

    printf("Advance\n");
    advanceTicks(&ctx, 100);

    printf("Send start tracing, start and reset counters\n");
    trace_PC_set_paddr(&ctx, 0x10000);
    trace_PC_start(&ctx);
    trace_PC_counter_start(&ctx);
    trace_PC_counter_reset(&ctx);

    printf("Advance and stop\n");
    advanceTicks(&ctx, 30000);
    trace_PC_counter_stop(&ctx);


    printf("Get counters\n");
    uint32_t cntExecute;
    uint32_t cntStalls;
    uint32_t cntBursts;
    trace_PC_counter_execute(&ctx, &cntExecute);
    trace_PC_counter_stalls(&ctx, &cntStalls);
    trace_PC_counter_bursts(&ctx, &cntBursts);

    printf("Stop CPU\n");
    transplantStopCPU(&ctx, 0);

    printf("Advance\n");
    advanceTicks(&ctx, 100);

    printf("Print results\n");
    printf("Executing:%010d\n"
           "Stalls:   %010d\n"
           "Bursts:   %010d\n", 
           cntExecute, cntStalls, cntBursts);
 
    releaseFPGAContext(&ctx);
}
