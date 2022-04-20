#include "fpga_interface.h"
#include "fpga.h"

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <unistd.h>

#include <assert.h>

/**
 * @file Definition of functions to exchange data with FPGA.
 */

/**
 * Bind a thread id with process id.
 * @param thid the given thread id.
 * @param asid the process id. 
 * @returns 0 if successful.
 *
 * @note associate with S_AXIL_TT
 * @note pairing a thread id with 0 asid means translanting back since no asid in a system will be zero.
 */
int mmuRegisterTHID2ASID(const FPGAContext *c, uint32_t thid,
                              uint32_t asid) {
  assert(thid < 128 && "The maximum number of supported thread is 128.");
  return writeAXIL(c, BASE_ADDR_BIND_ASID_THID + thid * 4, asid);
}

/**
 * Push the context of specific thread to the FPGA.
 * 
 * @param thid the thread if that this thread will be bind to.
 * @param asid the process id for the address space.
 * @param state the state registers of the context.
 * 
 * @returns 0 if successful.
 * 
 * @note this function will not start the transplant but only bind.
 */
int transplantRegisterAndPush(const FPGAContext *c, uint32_t thid, uint32_t asid, DevteroflexArchState *state) {
  // 1. register thread id.
  int res = mmuRegisterTHID2ASID(c, thid, asid);
  if(res != 0) return res;
  // 2. push the state.
  res = transplantPushState(c, thid, state);
  return res;
}

/**
 * Transplant a thread back from the FPGA and load its context.
 * 
 * @param thid the thread that should be transplanted.
 * @param state the state registers of the thread.
 * 
 * @returns 0 if successful.
 * 
 * @note Please make sure that target thread must be ready to be transplanted back. Unknown case will be happened if thread is still working.
 * FIXME: Add mechanism to stop the execution of that thread on FPGA
 */
int transplantUnregisterAndPull(const FPGAContext *c, uint32_t thid, DevteroflexArchState *state) {
  // 1. unregister thread id.
  int res = mmuRegisterTHID2ASID(c, thid, 0);
  if(res != 0) return res;
  // 2. Read staste.
  res = transplantGetState(c, thid, state);
  return res;
}

int transplantSinglestep(const FPGAContext *c, uint32_t thid, uint32_t asid, DevteroflexArchState *state) {
  int res = 0;
  res |= transplantRegisterAndPush(c, thid, asid, state);
  res |= transplantStopCPU(c, thid);
  res |= transplantStart(c, thid);
  return res;
}

int transplantGetState(const FPGAContext *c, uint32_t thid, DevteroflexArchState *state) {
  assert(thid < 128 && "The maximum number of supported thread is 128.");
  return readAXI(c, BASE_ADDR_TRANSPLANT_DATA + thid * TRANS_STATE_THID_MAX_BYTES, state, TRANS_STATE_SIZE_BYTES);
}

int transplantPushState(const FPGAContext *c, uint32_t thid, DevteroflexArchState *state) {
  assert(thid < 128 && "The maximum number of supported thread is 128.");
  return writeAXI(c, BASE_ADDR_TRANSPLANT_DATA + thid * TRANS_STATE_THID_MAX_BYTES, state, TRANS_STATE_SIZE_BYTES);
}

int transplantWaitTillPending(const FPGAContext *c, uint32_t *pending_threads) {
  uint32_t pending = 0;
#ifndef AWS_FPGA
  printf("Waiting for transplant pending...\n");
#endif
  while(!pending) {
    if(transplantPending(c, &pending)) return -1;
    usleep(1e5);
  }
  transplantFreePending(c, pending);
  *pending_threads = pending;
#ifndef AWS_FPGA
  printf("Pending Threads[%x]\n", pending);
#endif
  return 0;
}

int transplantPending(const FPGAContext *c, uint32_t *pending_threads) {
  *pending_threads = 0;
  return readAXIL(c, BASE_ADDR_TRANSPLANT_CTRL + TRANS_REG_OFFST_PENDING, pending_threads);
}

int transplantFreePending(const FPGAContext *c, uint32_t pending_threads) {
  return writeAXIL(c, BASE_ADDR_TRANSPLANT_CTRL + TRANS_REG_OFFST_FREE_PENDING, pending_threads);
}

int transplantStart(const FPGAContext *c, uint32_t thid) {
  return writeAXIL(c, BASE_ADDR_TRANSPLANT_CTRL + TRANS_REG_OFFST_START, 1 << thid);
}

int transplantStopCPU(const FPGAContext *c, uint32_t thid) {
  return writeAXIL(c, BASE_ADDR_TRANSPLANT_CTRL + TRANS_REG_OFFST_STOP_CPU, 1 << thid);
}

int transplantForceTransplant(const FPGAContext *c, uint32_t thid) {
  return writeAXIL(c, BASE_ADDR_TRANSPLANT_CTRL + TRANS_REG_OFFST_FORCE_TRANSPLANT, 1 << thid);
}


/**
 * @brief Check whether there is a MMU message waiting for processing.
 * @returns true if there is a message.
 */
bool mmuMsgHasPending(const FPGAContext *c) {
  uint32_t ret = -1;
  if(readAXIL(c, BASE_ADDR_MMU_MSG_QUEUE + MMU_MSG_QUEUE_REG_OFST_PENDING, &ret)){
    printf("Error: Access pending registers trigger an error. \n");
    exit(-1);
  }
  return !(ret == 0);
}

/**
 * @brief Block till there's a message in the MMU pending.
 * @param msg the buffer for the message.
 * @returns 0 if successful.
 *
 * @note associate with S_AXI_QEMU_MQ and S_AXIL_QEMU_MQ
 * @note this will block the routine until it get message.
 */
int mmuMsgGet(const FPGAContext *c, MessageFPGA *msg) {
  uint32_t res = -1;
  bool hasMessage = false;
  do {
    hasMessage = mmuMsgHasPending(c);
  } while (!hasMessage);
  res = readAXI(c, BASE_ADDR_AXI_MMU_MSG, msg, sizeof(MessageFPGA));
  if(res == 0){
    return writeAXIL(c, BASE_ADDR_MMU_MSG_QUEUE + MMU_MSG_QUEUE_REG_OFST_POP, 1);
  }
  return res;
}



/**
 * @brief Peek the MMU message queue, without popping the message.
 * @param message the buffer for the message.
 * @returns 0 if successful.
 *
 * @note may return invalid value if there is no message in the queue.
 */
int mmuMsgPeek(const FPGAContext *c, MessageFPGA *msg) {
  return readAXI(c, BASE_ADDR_AXI_MMU_MSG, msg, sizeof(MessageFPGA));
}

/**
 * Push a message to FPGA.
 * @param raw_message the pointer pointing to the message.
 * @param message_size the size of the message.
 * @return 0 if successful. 1 for the full Rx queue.
 *
 * @note associate with S_AXI_QEMU_MQ
 */
int mmuMsgSend(const FPGAContext *c, MessageFPGA *msg) {
  uint32_t res = -1;
  do {
    readAXIL(c, BASE_ADDR_MMU_MSG_QUEUE + MMU_MSG_QUEUE_REG_OFST_FREE, &res);
  } while (res == 0); // wait for the message to be 1.
  res = writeAXI(c, BASE_ADDR_AXI_MMU_MSG + 64, msg, sizeof(MessageFPGA));
  if (res != 0) return res;
  // send the message out
  return writeAXIL(c, BASE_ADDR_MMU_MSG_QUEUE + MMU_MSG_QUEUE_REG_OFST_PUSH, 1);
}

/**
 * Push a page to FPGA DRAM according to its physical page number
 * 
 * @param paddr the physical page address, ignore LSB
 * @param page the page itself
 * @return 0 if successful.
 * 
 */
int dramPagePush(const FPGAContext *c, uint64_t paddr, void *page){
  if(paddr & (PAGE_SIZE-1)){
    printf("Warning: misaligned address found: 0x%016lx. \n Its lower 12bit should be zero. \n", paddr);
    paddr ^= (paddr & 0xFFFUL);
  }
  assert(paddr < c->dram_size && "DRAM range overflow.");
  return writeAXI(c, BASE_ADDR_DRAM + paddr, page, PAGE_SIZE);
}


/**
 * fetch a page from FPGA DRAM.
 * @param paddr the physical page address, ignore LSB
 * @param buffer a 4kb buffer for the result page.
 * 
 * @return 0 if successful.
 */
int dramPagePull(const FPGAContext *c, uint64_t paddr, void *page){
  if(paddr & (PAGE_SIZE-1)){
    printf("Warning: misaligned address found: 0x%016lx. \n Its lower 12bit should be zero. \n", paddr);
    paddr ^= (paddr & 0xFFFUL);
  }
  assert(paddr < c->dram_size && "DRAM range overflow.");
  return readAXI(c, BASE_ADDR_DRAM + paddr, page, PAGE_SIZE);
}

// Instrumentation helpers
int trace_PC_set_paddr(const FPGAContext *c, uint32_t paddr) {
  return writeAXIL(c, BASE_ADDR_TRANSPLANT_CTRL + TRACE_PC_OFFST_PADDR, paddr);
}

int trace_PC_start(const FPGAContext *c) {
  return writeAXIL(c, BASE_ADDR_TRANSPLANT_CTRL + TRACE_PC_OFFST_START, 1 << 0);
}

int trace_PC_counter_reset(const FPGAContext *c) {
  return writeAXIL(c, BASE_ADDR_TRANSPLANT_CTRL + TRACE_PC_OFFST_START, 1 << 1);
}

int trace_PC_counter_execute(const FPGAContext *c, uint32_t* count) {
  return readAXIL(c, BASE_ADDR_TRANSPLANT_CTRL + TRACE_PC_OFFST_CNT_EXE, count);
}

int trace_PC_counter_stalls(const FPGAContext *c, uint32_t* count) {
  return readAXIL(c, BASE_ADDR_TRANSPLANT_CTRL + TRACE_PC_OFFST_CNT_STALLS, count);
}

int trace_PC_counter_start(const FPGAContext *c) {
  return writeAXIL(c, BASE_ADDR_TRANSPLANT_CTRL + TRACE_PC_OFFST_CNT_CTLREG, 1);
}

int trace_PC_counter_stop(const FPGAContext *c) {
  return writeAXIL(c, BASE_ADDR_TRANSPLANT_CTRL + TRACE_PC_OFFST_CNT_CTLREG, 0);
}

int trace_PC_counter_bursts(const FPGAContext *c, uint32_t* count) {
  return readAXIL(c, BASE_ADDR_TRANSPLANT_CTRL + TRACE_PC_OFFST_CNT_BURSTS, count);
}

#ifndef AWS_FPGA
int writeSimCtrl(const FPGAContext *c, int type, int value) {
  return writeAXILMagic(c, type, value);
}
#endif

