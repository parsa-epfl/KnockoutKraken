#pragma once

#include <stdint.h>
#include <stdio.h>

// Base addresses
// BAR1 - AXIL
#define BASE_ADDR_AXIL             (0x000000000000)

// PCIS - AXI
#define BASE_ADDR_DRAM             (0x000000000000)
#define BASE_ADDR_RTL              (0x001000000000)

typedef struct FPGAContext {
#ifndef AWS_FPGA
  ssize_t axil_fd;
  ssize_t axi_fd;
#else
  int axil;
  int read_fd;
  int write_fd;
#endif
  uint64_t dram_size;
  uint64_t ppage_base_addr;
} FPGAContext;


/**
 * @file the interface for manipulating FPGA.
 */

int initFPGAContext(struct FPGAContext *c);
int readAXIL(const struct FPGAContext *c, uint32_t addr, uint32_t *data);
int writeAXIL(const struct FPGAContext *c, uint32_t addr, uint32_t data);
int readAXI(const struct FPGAContext *c, uint64_t addr, void *data, uint64_t size_in_byte);
int writeAXI(const struct FPGAContext *c, uint64_t addr, void *data, uint64_t size_in_byte);
int releaseFPGAContext(struct FPGAContext *c);

#ifndef AWS_FPGA
enum MagicTypes {
  cycleStep
};
int writeAXILMagic(const struct FPGAContext *c, uint32_t addr, uint32_t data);
#endif