#pragma once
extern "C" {
#include "fpga_helpers.h"
}

#include "catch.hpp"

/**
 * @brief Evict page from FPGA and move it back.
 * 
 * @param ctx FPGA handler
 * @param asid address space id
 * @param page a buffer where the evicted page will be moved to
 * @param vaddr the VA of the page to evict
 * @param paddr the PA of the page to evict (for checking only)
 * @param expect_modified whether the page is modified (for checking only)
 */
void synchronizePage(FPGAContext *ctx, int asid, uint8_t *page, uint64_t vaddr, bool is_instruction_page,
                     uint64_t paddr, bool expect_modified);
void requireStateIsIdentical(const DevteroflexArchState &state1,
                             const DevteroflexArchState &state2);
void checkPagePerWord(uint8_t *page_expect, uint8_t *page_actual);
void initFPGAContextAndPage(int num_threads, FPGAContext *c);

static uint8_t zero_page[PAGE_SIZE] = {0};
static uint8_t page[PAGE_SIZE] = {0};

/**
 * @brief print the counters from PMU.
 */
void printPMUCounters(const FPGAContext *ctx);