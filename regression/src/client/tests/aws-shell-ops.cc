extern "C" {
#include "../fpga.h"
#include "../fpga_interface.h"
}

#include "../test-helpers.hh"

TEST_CASE("check-dram-address") {
  FPGAContext ctx;
  uint8_t page_deadbeef[PAGE_SIZE] = {0};
  uint8_t page[PAGE_SIZE] = {0};
  INFO("INIT CONTEXT");
  REQUIRE(initFPGAContext(&ctx) == 0);
  uint64_t paddr = 0;
  makeDeadbeefPage(page_deadbeef, PAGE_SIZE);
  INFO("SENT PAGE");
  dramPagePush(&ctx, paddr, page_deadbeef);
  dramPagePush(&ctx, paddr + ctx.dram_size - PAGE_SIZE, page_deadbeef);
  INFO("BRING BACK PAGE");
  dramPagePull(&ctx, paddr, page);
  INFO("CHECK PAGE");
  checkPagePerWord(page_deadbeef, page);
  dramPagePull(&ctx, paddr + ctx.dram_size - PAGE_SIZE, page);
  INFO("CHECK PAGE");
  checkPagePerWord(page_deadbeef, page);
}
