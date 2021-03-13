#include "fpga_interface.h"
#include "fpga.h"

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <unistd.h>

/**
 * @file Definition of functions to exchange data with FPGA.
 */

/**
 * Bind a thread id with process id.
 * @param thread_id the given thread id.
 * @param process_id the process id. 
 * @returns 0 if successful.
 *
 * @note associate with S_AXIL_TT
 * @note pairing a thread id with 0 pid means translanting back since no PID in a system will be zero.
 */
int registerThreadWithProcess(const FPGAContext *c, uint32_t thread_id,
                              uint32_t process_id) {
  const uint32_t axi_tt_base = c->base_address.axil_base + c->base_address.tt;
  return writeAXIL(c, axi_tt_base + thread_id * 4, process_id);
}

/**
 * Push the context of specific thread to the FPGA.
 * 
 * @param thread_id the thread if that this thread will be bind to.
 * @param process_id the process id for the address space.
 * @param state the state registers of the context.
 * 
 * @returns 0 if successful.
 * 
 * @note this function will not start the transplant but only bind.
 */
int registerAndPushState(const struct FPGAContext *c, uint32_t thread_id, uint32_t process_id, const struct ArmflexArchState *state) {
  // 1. register thread id.
  int res = registerThreadWithProcess(c, thread_id, process_id);
  if(res != 0) return res;
  // 2. push the state.
  res = transplant_pushState(c, thread_id, (uint64_t *)state, ARMFLEX_TOT_REGS);
  return res;
}

/**
 * Transplant a thread back from the FPGA and load its context.
 * 
 * @param thread_id the thread that should be transplanted.
 * @param state the state registers of the thread.
 * 
 * @returns 0 if successful.
 * 
 * @note Please make sure that target thread must be ready to be transplanted back. Unknown case will be happened if thread is still working.
 * FIXME: Add mechanism to stop the execution of that thread on FPGA
 */
int transplantBack(const struct FPGAContext *c, uint32_t thread_id, struct ArmflexArchState *state) {
  // 1. unregister thread id.
  int res = registerThreadWithProcess(c, thread_id, 0);
  if(res != 0) return res;
  // 2. Read staste.
  res = transplant_getState(c, thread_id, (uint64_t *)state, ARMFLEX_TOT_REGS);
  return res;
}

/**
 * Query the state of all threads to see if each one requires a transplant back.
 * 
 * @param pending_threads the state of each theads
 * 
 * @returns 0 if successful.
 */
int queryThreadState(const struct FPGAContext *c, uint32_t *pending_threads) {

  return transplant_pending(c, pending_threads);
}

int transplant_getState(const FPGAContext *c, uint32_t thread_id,
                        uint64_t *state, size_t regCount) {
  const uint32_t axi_transplant_base = c->base_address.axil_base + c->base_address.transplant_data;
  int ret = 0;
  uint32_t reg_val1;
  uint32_t reg_val2;
  size_t base_offset = axi_transplant_base + (thread_id << 7) * 4;
  for (int reg = 0; reg < regCount; reg++) {
    ret = readAXIL(c, base_offset + (2 * reg) * 4, &reg_val1);
    if (ret)
      return ret;
    ret = readAXIL(c, base_offset + (2 * reg + 1) * 4, &reg_val2);
    if (ret)
      return ret;
    state[reg] = reg_val1 | ((uint64_t)reg_val2) << 32;
  }
  return 0;
}

// TODO: Strongly suggest that functions are named in camel style.
int transplant_pushState(const FPGAContext *c, uint32_t thread_id,
                         uint64_t *state, size_t regCount) {
  const uint32_t axi_transplant_base = c->base_address.axil_base + c->base_address.transplant_data;
  int ret = 0;
  uint32_t reg_val1;
  uint32_t reg_val2;
  size_t base_offset = axi_transplant_base + (thread_id << 7) * 4;
  for (int reg = 0; reg < regCount; reg++) {
    reg_val1 = (uint64_t)state[reg];
    reg_val2 = ((uint64_t)state[reg] >> 32);
    ret = writeAXIL(c, base_offset + (2 * reg) * 4, reg_val1);
    if (ret)
      return ret;
    ret = writeAXIL(c, base_offset + (2 * reg + 1) * 4, reg_val2);
    if (ret)
      return ret;
  }
  return 0;
}

int transplant_waitTillPending(const FPGAContext *c, uint32_t *pending_threads) {
  uint32_t pending = 0;
#ifndef AWS_FPGA
  printf("Waiting for transplant pending...\n");
#endif
  while(!pending) {
    if(queryThreadState(c, &pending)) return -1;
    usleep(1e5);
  }
  transplant_freePending(c, pending);
  *pending_threads = pending;
#ifndef AWS_FPGA
  printf("Pending Threads[%x]\n", pending);
#endif
  return 0;
}

int transplant_pending(const FPGAContext *c, uint32_t *pending_threads) {
  const uint32_t axi_transplant_ctrl_base = c->base_address.axil_base + c->base_address.transplant_ctl;
  *pending_threads = 0;
  return readAXIL(c, axi_transplant_ctrl_base, pending_threads);
}

int transplant_freePending(const FPGAContext *c, uint32_t pending_threads) {
  const uint32_t axi_transplant_ctrl_base = c->base_address.axil_base + c->base_address.transplant_ctl;
  return writeAXIL(c, axi_transplant_ctrl_base, pending_threads);
}

int transplant_start(const FPGAContext *c, uint32_t thread_id) {
  const uint32_t axi_transplant_ctrl_base = c->base_address.axil_base + c->base_address.transplant_ctl;
  return writeAXIL(c, axi_transplant_ctrl_base + 4, 1 << thread_id);
}

/**
 * Block till there's a message in the MMU pending.
 * @param message the buffer for the message.
 * @returns 0 if successful.
 *
 * @note associate with S_AXI_QEMU_MQ and S_AXIL_QEMU_MQ
 * @note this will block the routine until it get message.
 */
int queryMessageFromFPGA(const FPGAContext *c, uint8_t message[64]) {
  const uint32_t query_base = c->base_address.axil_base + c->base_address.message_queue;
  const uint64_t data_base = c->base_address.axi_base + c->base_address.message;
  uint32_t res = -1;
  bool hasMessage = false;
  do {
    hasMessage = hasMessagePending(c);
  } while (!hasMessage);
  return readAXI(c, data_base, (uint64_t *) message, 64);
}

/**
 * Check whether there is a MMU message pending.
 * @param message the buffer for the message.
 * @returns 0 if successful.
 *
 * @note associate with S_AXI_QEMU_MQ and S_AXIL_QEMU_MQ
 * @note this will block the routine until it get message.
 */
bool hasMessagePending(const FPGAContext *c) {
  const uint32_t query_base = c->base_address.axil_base + c->base_address.message_queue;
  uint32_t res = -1;
  readAXIL(c, query_base + 0x4, &res);
  return !(res == 0);
}

/**
 * Block till there's a message in the MMU pending.
 * @param message the buffer for the message.
 * @returns 0 if successful.
 *
 * @note associate with S_AXI_QEMU_MQ and S_AXIL_QEMU_MQ
 * @note this will block the routine until it get message.
 */
int getMessagePending(const FPGAContext *c, uint8_t message[64]) {
  const uint32_t query_base = c->base_address.axil_base + c->base_address.message_queue;
  const uint64_t data_base = c->base_address.axi_base + c->base_address.message;
  return readAXI(c, data_base, (uint64_t *) message, 64);
}

/**
 * Check whether the receiver queue is empty.
 * @param data the result of the queue. 0 means it's empty, and 1 means it has at least one message.
 * 
 * @return 0 if successful.
 * 
 */
int checkRxMessageQueue(const FPGAContext *c, uint32_t *data) {
  const uint32_t query_base = c->base_address.axil_base + c->base_address.message_queue;
  return readAXIL(c, query_base + 0x4, data);
}

/**
 * Push a message to FPGA.
 * @param raw_message the pointer pointing to the message.
 * @param message_size the size of the message.
 * @return 0 if successful. 1 for the full Rx queue.
 *
 * @note associate with S_AXI_QEMU_MQ
 */
int sendMessageToFPGA(const FPGAContext *c, void *raw_message,
                      size_t message_size) {
  const uint32_t query_base = c->base_address.axil_base + c->base_address.message_queue;
  const uint64_t data_base = c->base_address.axi_base + c->base_address.message;
  uint32_t res = -1;
  do {
    readAXIL(c, query_base, &res);
  } while (res == 0); // wait for the message to be 1.
  uint64_t buffer[8];
  bzero(buffer, 64);
  memcpy(buffer, raw_message, message_size);
  return writeAXI(c, data_base, buffer, 64);
}

/**
 * Push a page to FPGA DRAM according to its physical page number
 * 
 * @param paddr the physical page address, ignore LSB
 * @param page the page itself
 * @return 0 if successful.
 * 
 */
int pushPageToFPGA(const struct FPGAContext *c, uint64_t paddr, void *page){
  if(paddr & (PAGE_SIZE-1)){
    printf("Warning: misaligned address found: %lu. \n Its lower 12bit should be zero. \n", paddr);
    paddr ^= (paddr & 4095UL);
  }
  return writeAXI(c, c->base_address.dram_base + paddr, page, PAGE_SIZE);
}


/**
 * fetch a page from FPGA DRAM.
 * @param paddr the physical page address, ignore LSB
 * @param buffer a 4kb buffer for the result page.
 * 
 * @return 0 if successful.
 */
int fetchPageFromFPGA(const struct FPGAContext *c, uint64_t paddr, void *buffer){
  if(paddr & (PAGE_SIZE-1)){
    printf("Warning: misaligned address found: %lu. \n Its lower 12bit should be zero. \n", paddr);
    paddr ^= (paddr & 4095UL);
  }
  return readAXI(c, c->base_address.dram_base + paddr, buffer, PAGE_SIZE);
}
