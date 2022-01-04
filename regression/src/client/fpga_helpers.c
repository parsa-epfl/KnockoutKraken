#include "fpga_helpers.h"

void makeDeadbeefPage(uint8_t *pages, size_t bytes) {
  uint32_t *page_word = (uint32_t *)pages;
  for (int word = 0; word < bytes / 4; word++) {
    page_word[word] = 0xDEADBEEF;
  }
}

void makeZeroPage(uint8_t *page) {
  uint32_t *page_word = (uint32_t *)page;
  for (int word = 0; word < PAGE_SIZE / 4; word++) {
    page_word[word] = 0xFFFFFFFF;
  }
}

void advanceTicks(const FPGAContext *c, int ticks) {
#ifndef AWS_FPGA
  writeSimCtrl(c, cycleStep, ticks);
#else
  return;
#endif
}
