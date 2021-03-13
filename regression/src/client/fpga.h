#pragma once

#include <stdint.h>
#include <stdio.h>

typedef struct AXIBaseAddress {
  uint32_t axil_base;

  uint32_t tt;
  uint32_t transplant_data;
  uint32_t transplant_ctl;
  uint32_t message_queue; 

  uint64_t dram_base;
  uint64_t pt_base;
  uint64_t page_base;

  uint64_t axi_base;
  uint64_t message;
} AXIBaseAddress;

#ifndef AWS_FPGA

typedef struct FPGAContext {
  ssize_t axil_fd;
  ssize_t axi_fd;
  uint64_t dram_size;

  AXIBaseAddress base_address;
} FPGAContext;

#else

typedef struct FPGAContext {
  int axil;
  int read_fd;
  int write_fd;
  uint64_t dram_size;
  AXIBaseAddress base_address;
} FPGAContext;

#endif

/**
 * @file the interface for manipulating FPGA.
 */

int initFPGAContext(struct FPGAContext *c);
int readAXIL(const struct FPGAContext *c, uint32_t addr, uint32_t *data);
int writeAXIL(const struct FPGAContext *c, uint32_t addr, uint32_t data);
int readAXI(const struct FPGAContext *c, uint64_t addr, void *data, uint64_t size_in_byte);
int writeAXI(const struct FPGAContext *c, uint64_t addr, void *data, uint64_t size_in_byte);
int releaseFPGAContext(struct FPGAContext *c);
