#pragma once

#include "fpga.h"

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#define PAGE_SIZE (4096LLU)
#define GET_PAGE_MASK(addr) (addr & ~(PAGE_SIZE-1))
#define GET_PPN_FROM_PADDR(addr) (addr >> 12) // log2(4096)

/**
 * @file This file defines the software messages for FPGA communication.
 */

typedef enum MessageType {
  // FPGA -> QEMU
  sPageFaultNotify = 4,
  sEvictNotify = 5,
  sEvictDone = 6,
  // QEMU -> FPGA
  sPageEvict = 7,
  sMissReply = 2,
  sEvictReply = 3
} MessageType;

// QEMU requests a page eviction.
typedef struct QEMUPageEvictRequest {
  MessageType type; // constant 7
  uint32_t vpn_lo;
  uint32_t vpn_hi;
  uint32_t pid;
} QEMUPageEvictRequest;

// QEMU resolves a page fault.
typedef struct QEMUMissReply {
  MessageType type; // constant 2
  uint32_t vpn_lo;
  uint32_t vpn_hi;
  uint32_t pid;
  uint32_t permission;
  uint32_t tid; // -1 means no wake up.
  uint32_t ppn;
} QEMUMissReply;

#define VPN_ALIGN(va) (va >> 12)
#define VPN_GET_HI(va) ((VPN_ALIGN(va) >> 32) & 0xFFFFFFFF)
#define VPN_GET_LO(va) (VPN_ALIGN(va) & 0xFFFFFFFF)
#define GET_NZCV(nzcv) (nzcv & 0xF)

// QEMU confirms a page eviction. 
typedef struct QEMUEvictReply {
  MessageType type; // constant 3
  uint32_t vpn_lo;
  uint32_t vpn_hi;
  uint32_t pid;
  uint32_t old_ppn;
  // uint32_t synonym_v;
} QEMUEvictReply;

// FPGA finds a page fault.
typedef struct PageFaultNotification {
  MessageType type; // constant 4
  uint32_t vpn_lo;
  uint32_t vpn_hi;
  uint32_t pid;
  uint32_t permission;
  uint32_t tid;
} PageFaultNotification;

// type = 5: FPGA starts the page eviction. (No page movement if the modified is not dirty).
// type = 6: FPGA finishes the page eviction (The page is available in the page buffer).
typedef struct PageEvictNotification {
  MessageType type; // maybe 5(start) or 6(done)
  uint32_t vpn_lo;
  uint32_t vpn_hi;
  uint32_t pid;

  uint32_t ppn;
  uint32_t permission;
  uint32_t modified;
} PageEvictNotification;


// helper class
typedef struct ArmflexArchState {
	uint64_t xregs[32];
	uint64_t pc;
	uint64_t sp;
	uint64_t nzcv;
} ArmflexArchState;


int registerThreadWithProcess(const struct FPGAContext *c, uint32_t thread_id, uint32_t process_id);
int checkRxMessageQueue(const struct FPGAContext *c, uint32_t *result);
int queryMessageFromFPGA(const struct FPGAContext *c, uint8_t message[64]);
bool hasMessagePending(const FPGAContext *c);
int getMessagePending(const FPGAContext *c, uint8_t message[64]);
int sendMessageToFPGA(const struct FPGAContext *c,  void *raw_message, size_t message_size);
int pushPageToFPGA(const struct FPGAContext *c, uint64_t paddr, void *page);
int fetchPageFromFPGA(const struct FPGAContext *c, uint64_t paddr, void *buffer);
 
int registerAndPushState(const struct FPGAContext *c, uint32_t thread_id, uint32_t process_id, const struct ArmflexArchState *state);
int queryThreadState(const struct FPGAContext *c, uint32_t *pending_threads);
int transplantBack(const struct FPGAContext *c, uint32_t thread_id, struct ArmflexArchState *state);

// Helper functions
int transplant_getState(const FPGAContext *c, uint32_t thread_id, uint64_t *state, size_t regCount);
int transplant_pushState(const FPGAContext *c, uint32_t thread_id, uint64_t *state, size_t regCount);
int transplant_pending(const FPGAContext *c, uint32_t *pending_threads);
int transplant_freePending(const FPGAContext *c, uint32_t pending_threads);
int transplant_waitTillPending(const FPGAContext *c, uint32_t *pending_threads);
int transplant_start(const FPGAContext *c, uint32_t thread_id);


// See cpu.h to match MMUAccessType
typedef enum MemoryAccessType {
    DATA_LOAD  = 0,
    DATA_STORE = 1,
    INST_FETCH = 2
} MemoryAccessType;

/* This describes layour of arch state elements
 *
 * XREGS: uint64_t
 * PC   : uint64_t
 * SP   : uint64_t
 * NF/ZF/CF/VF : uint64_t
 */
#define ARCH_PSTATE_PC_OFFST    (32)
#define ARCH_PSTATE_SP_OFFST    (33)
#define ARCH_PSTATE_NZCV_OFFST  (34)
#define ARCH_PSTATE_NF_MASK     (3)    // 64bit 3
#define ARCH_PSTATE_ZF_MASK     (2)    // 64bit 2
#define ARCH_PSTATE_CF_MASK     (1)    // 64bit 1
#define ARCH_PSTATE_VF_MASK     (0)    // 64bit 0
#define ARMFLEX_TOT_REGS        (35)
