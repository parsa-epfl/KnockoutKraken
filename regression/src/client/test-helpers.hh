#pragma once
extern "C" {
#include "fpga.h"
#include "fpga_interface.h"
}

#define GET_asid(th) ((th + 1) << 4)
#include "catch.hpp"

void requireStateIsIdentical(const ArmflexArchState &state1,
                             const ArmflexArchState &state2);

void initArchState(ArmflexArchState *state, uint64_t pc);

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
void synchronizePage(FPGAContext *ctx, int asid, uint8_t *page, uint64_t vaddr,
                     uint64_t paddr, bool expect_modified);

// Page initialization
void makeDeadbeefPage(uint8_t *pages, size_t bytes);
void makeZeroPage(uint8_t *page);