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

typedef struct MessageFPGA
{
    union
    {
        struct
        { // Common types
            uint32_t type;
            uint32_t asid;
            union {
                uint64_t vpn;
                struct {
                    uint32_t vpn_lo;
                    uint32_t vpn_hi;
                };
            };
        };
        struct
        { // type == 2
            MessageType type;
            uint32_t asid;
            uint32_t vpn_lo;
            uint32_t vpn_hi;
            uint32_t permission;
            uint32_t thid; // -1 means no wake up.
            uint32_t ppn;
        } MissReply;
        struct
        { // type == 3
            MessageType type;
            uint32_t asid;
            uint32_t vpn_lo;
            uint32_t vpn_hi;
            uint32_t old_ppn;
        } EvictReply;
        struct
        { // type == 4
            MessageType type;
            uint32_t asid;
            uint32_t vpn_lo;
            uint32_t vpn_hi;

            uint32_t permission;
            uint32_t thid;
        } PageFaultNotif;
        struct
        { // type == 5
            MessageType type;
            uint32_t asid;
            uint32_t vpn_lo;
            uint32_t vpn_hi;

            uint32_t ppn;
            uint32_t permission;
            uint32_t modified;
        } EvictNotif;
        struct
        { // type == 6
            MessageType type;
            uint32_t asid;
            uint32_t vpn_lo;
            uint32_t vpn_hi;

            uint32_t ppn;
            uint32_t permission;
            uint32_t modified;
        } EvictDone;
        struct
        { // type == 7
            MessageType type;
            uint32_t asid;
            uint32_t vpn_lo;
            uint32_t vpn_hi;
        } PageEvictRequest;
        uint32_t words[16];
        uint8_t bytes[64];
    };
} MessageFPGA;

// QEMU resolves a page fault.
#define VPN_ALIGN(va) (va >> 12)
#define VPN_GET_HI(va) ((VPN_ALIGN(va) >> 32) & 0xFFFFFFFF)
#define VPN_GET_LO(va) (VPN_ALIGN(va) & 0xFFFFFFFF)
#define GET_NZCV(nzcv) (nzcv & 0xF)

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
int transplant_stopCPU(const FPGAContext *c, uint32_t thread_id);


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

static inline void makeEvictRequest(int asid, uint64_t va, MessageFPGA *evict_request)
{
    evict_request->type = sPageEvict;
    evict_request->asid = asid;
    evict_request->vpn_hi = VPN_GET_HI(va);
    evict_request->vpn_lo = VPN_GET_LO(va);
}

static inline void makeEvictReply(MessageFPGA *notif, MessageFPGA *evict_reply)
{
    evict_reply->type = sEvictReply;
    evict_reply->asid = notif->asid;
    evict_reply->vpn_hi = notif->vpn_hi;
    evict_reply->vpn_lo = notif->vpn_lo;

    evict_reply->EvictReply.old_ppn = notif->EvictNotif.ppn;
}

static inline void makeMissReply(int type, int thid, int asid, uint64_t va, uint64_t paddr,
                                 MessageFPGA *miss_reply)
{
    miss_reply->type = sMissReply;
    miss_reply->asid = asid;
    miss_reply->vpn_hi = VPN_GET_HI(va);
    miss_reply->vpn_lo = VPN_GET_LO(va);

    miss_reply->MissReply.thid = thid;
    miss_reply->MissReply.permission = type;
    miss_reply->MissReply.ppn = GET_PPN_FROM_PADDR(paddr);
}

// Instrumentation control
#define TRACE_PC_OFFST_PADDR       (0*0x4)
#define TRACE_PC_OFFST_START       (1*0x4)
#define TRACE_PC_OFFST_CNT_EXE     (2*0x4)
#define TRACE_PC_OFFST_CNT_STALLS  (3*0x4)
#define TRACE_PC_OFFST_CNT_CTLREG  (4*0x4)
#define TRACE_PC_OFFST_CNT_BURSTS  (5*0x4)

int trace_PC_set_paddr(const FPGAContext *c, uint32_t paddr);
int trace_PC_start(const FPGAContext *c);
int trace_PC_counter_reset(const FPGAContext *c);
int trace_PC_counter_execute(const FPGAContext *c, uint32_t* count);
int trace_PC_counter_stalls(const FPGAContext *c, uint32_t* count);
int trace_PC_counter_bursts(const FPGAContext *c, uint32_t* count);
int trace_PC_counter_start(const FPGAContext *c);
int trace_PC_counter_stop(const FPGAContext *c);


#ifndef AWS_FPGA
int writeSimCtrl(const FPGAContext *c, int type, int value);
#endif