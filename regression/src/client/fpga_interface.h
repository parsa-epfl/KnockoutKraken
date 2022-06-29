#ifndef FPGA_INTERFACE_H
#define FPGA_INTERFACE_H

#include "fpga.h"

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#define PAGE_SIZE (4096LLU)
#define GET_PAGE_MASK(addr) ((uint64_t) addr & ~(PAGE_SIZE-1))
#define GET_PPN_FROM_PADDR(addr) ((uint64_t) addr >> 12LU) // log2(4096)

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

/* This describes layour of arch state elements
 *
 * XREGS: uint64_t
 * PC   : uint64_t
 * Flags : uint64_t
 * icount : uint64_t
 */
typedef struct DevteroflexArchState {
	uint64_t xregs[32];
	uint64_t pc;
	uint64_t flags;
    union {
        uint64_t asid_reg;
        struct {
            uint32_t asid;
            uint32_t _asid_unused_;
        };
    };
    uint64_t icount;
    union {
        uint64_t icountRegs;
        struct {
            uint32_t icountExecuted;
            uint32_t icountBudget;
        };
    };
    uint64_t _unused_[3]; // Pad to 320 bytes -- Max 512 bytes
} DevteroflexArchState;

#define ARCH_PSTATE_XREG_OFFST   (0)
#define ARCH_PSTATE_PC_OFFST     (32)
#define ARCH_PSTATE_FLAGS_OFFST  (33)
#define ARCH_PSTATE_ASID_OFFST   (34)
#define ARCH_PSTATE_ICOUNT_OFFST (35)
#define ARCH_PSTATE_ICOUNTREGS_OFFST (36)
#define ARCH_PSTATE_TOT_REGS     (37)

#define FLAGS_NZCV_MASK                     (0xF)
#define FLAGS_GET_NZCV(flags)               (flags & FLAGS_NZCV_MASK)
#define FLAGS_SET_NZCV(flags, set)          (flags = (flags & ~FLAGS_NZCV_MASK) | set)
#define FLAGS_GET_IS_EXCEPTION(flags)       (flags & (1ULL << 4))
#define FLAGS_GET_IS_UNDEF(flags)           (flags & (1ULL << 5))
#define FLAGS_GET_IS_ICOUNT_DEPLETED(flags) (flags & (1ULL << 6))
#define FLAGS_GET_EXEC_MODE(flags)          ((flags >> 7) & 0b11)

#define PSTATE_FLAGS_EXECUTE_SINGLESTEP     (2ULL)
#define PSTATE_FLAGS_EXECUTE_NORMAL         (1ULL)
#define PSTATE_FLAGS_EXECUTE_WAIT           (0ULL)
#define FLAGS_MASK_EXEC_MODE(mode)          ((mode & 0b11) << 7)
#define FLAGS_MASK_EXEC_MODE_MASK           (0b11ULL << 7)
#define FLAGS_SET_EXEC_MODE(flags, mode)    (flags = ((flags & ~FLAGS_MASK_EXEC_MODE_MASK) | FLAGS_MASK_EXEC_MODE(mode)))



#define ARCH_PSTATE_NF_MASK      (3)    // 64bit 3
#define ARCH_PSTATE_ZF_MASK      (2)    // 64bit 2
#define ARCH_PSTATE_CF_MASK      (1)    // 64bit 1
#define ARCH_PSTATE_VF_MASK      (0)    // 64bit 0

// MMU
bool mmuMsgHasPending(const FPGAContext *c);
int  mmuMsgGet(const FPGAContext *c, MessageFPGA *msg);
int  mmuMsgPeek(const FPGAContext *c, MessageFPGA *msg);
int  mmuMsgSend(const FPGAContext *c,  MessageFPGA *msg);

// DRAM
int dramPagePush(const FPGAContext *c, uint64_t paddr, void *page);
int dramPagePull(const FPGAContext *c, uint64_t paddr, void *page);

// PMU
int pmuStartCounting(const FPGAContext *c);
int pmuStopCounting(const FPGAContext *c);
uint64_t pmuTotalCycles(const FPGAContext *c);
uint64_t pmuTotalCommitInstructions(const FPGAContext *c);
int pmuReadCycleCounters(const FPGAContext *c, int index, uint16_t counters[16]);


#define BASE_ADDR_MMU_MSG_QUEUE          (BASE_ADDR_AXIL + 0x200 * 4)
#define MMU_MSG_QUEUE_REG_OFST_PENDING   (0x0)
#define MMU_MSG_QUEUE_REG_OFST_FREE      (0x4)
#define MMU_MSG_QUEUE_REG_OFST_PUSH      (0x8)
#define MMU_MSG_QUEUE_REG_OFST_POP       (0xC)

#define BASE_ADDR_AXI_MMU_MSG            (BASE_ADDR_RTL + 0x10000)

// State transplants
#define BASE_ADDR_TRANSPLANT_DATA        (BASE_ADDR_RTL + 0x0)
#define BASE_ADDR_BIND_ASID_THID         (BASE_ADDR_AXIL + 0x0)
#define BASE_ADDR_TRANSPLANT_CTRL        (BASE_ADDR_AXIL + 0x100 * 4)
#define TRANS_REG_OFFST_PENDING          (0x4 * 0)
#define TRANS_REG_OFFST_FREE_PENDING     (0x4 * 0)
#define TRANS_REG_OFFST_START            (0x4 * 1)
#define TRANS_REG_OFFST_STOP_CPU         (0x4 * 2)
#define TRANS_REG_OFFST_FORCE_TRANSPLANT (0x4 * 3)
#define TRANS_REG_OFFST_WAITING          (0x4 * 4)
#define TRANS_REG_OFFST_RUNNING          (0x4 * 5)
#define TRANS_REG_OFFST_WAIT_STOP        (0x4 * 6)
#define TRANS_STATE_SIZE_BYTES      (320)   // 5 512-bit blocks; State fits in this amount of bytes
#define TRANS_STATE_THID_MAX_BYTES  (512)   // 8 512-bit blocks; max bytes allocated per thread
#define TRANS_STATE_THID_MAX_REGS   (512/8) // 64 64-bit words: total regs allocated per thread
int transplantPushAndWait(const FPGAContext *c, uint32_t thid, DevteroflexArchState *state);
int transplantPushAndStart(const FPGAContext *c, uint32_t thid, DevteroflexArchState *state);
int transplantPushAndSinglestep(const FPGAContext *c, uint32_t thid, DevteroflexArchState *state);
int transplantGetState(const FPGAContext *c, uint32_t thid, DevteroflexArchState *state);
int transplantPending(const FPGAContext *c, uint32_t *pending_threads);
int transplantFreePending(const FPGAContext *c, uint32_t pending_threads);
int transplantWaitTillPending(const FPGAContext *c, uint32_t *pending_threads);
int transplantStart(const FPGAContext *c, uint32_t thid);
int transplantStopCPU(const FPGAContext *c, uint32_t thid);
int transplantForceTransplant(const FPGAContext *c, uint32_t thid);
int transplantCheckRunning(const FPGAContext *c, uint32_t *running_threads);
int transplantCheckWaitStop(const FPGAContext *c, uint32_t *waitStop_threads);


#ifndef MemoryAccessType
// See cpu.h to match MMUAccessType
typedef enum MemoryAccessType {
    DATA_LOAD  = 0,
    DATA_STORE = 1,
    INST_FETCH = 2
} MemoryAccessType;
#define MemoryAccessType
#endif

static inline void makeEvictRequest(int asid, uint64_t va, MessageFPGA *evict_request)
{
    evict_request->type = sPageEvict;
    evict_request->asid = asid;
    evict_request->vpn_hi = VPN_GET_HI(va);
    evict_request->vpn_lo = VPN_GET_LO(va);
}

/* sEvictReply not used anymore
static inline void makeEvictReply(MessageFPGA *notif, MessageFPGA *evict_reply)
{
    evict_reply->type = sEvictReply;
    evict_reply->asid = notif->asid;
    evict_reply->vpn_hi = notif->vpn_hi;
    evict_reply->vpn_lo = notif->vpn_lo;

    evict_reply->EvictReply.old_ppn = notif->EvictNotif.ppn;
}
// */

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
#define BASE_ADDR_INSTRUMENTATION_TRACE  (BASE_ADDR_AXIL + 0x1F000)
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

#endif