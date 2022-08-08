#pragma once

#include <stdint.h>
#include <stdio.h>

// Base addresses
// BAR1 - AXIL
#define BASE_ADDR_AXIL                   (0x000000000000)
#define BASE_ADDR_PLATFORM_INFO          (BASE_ADDR_AXIL + 0x0)
#define VERILOG_GENERATED_TIME_LO        (0x4 * 0)
#define VERILOG_GENERATED_TIME_HI        (0x4 * 1)
#define PLATFORM_PADDR_WIDTH             (0x4 * 2)
#define PLATFORM_THREAD_NUMBER           (0x4 * 3)

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

  uint64_t dram_addr_base;
  uint64_t axi_peri_addr_base; 
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