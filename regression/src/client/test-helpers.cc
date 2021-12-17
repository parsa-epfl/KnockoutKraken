#include "test-helpers.hh"
#include "fpga.h"
#include "fpga_interface.h"

void makeEvictRequest(int pid, uint64_t va,
                      QEMUPageEvictRequest *evict_request) {
  evict_request->type = sPageEvict;
  evict_request->pid = pid;
  evict_request->vpn_hi = VPN_GET_HI(va);
  evict_request->vpn_lo = VPN_GET_LO(va);
}

void makeEvictReply(PageEvictNotification *notif, QEMUEvictReply *evict_reply) {
  evict_reply->type = sEvictReply;
  evict_reply->old_ppn = notif->ppn;
  evict_reply->pid = notif->pid;
  evict_reply->vpn_hi = notif->vpn_hi;
  evict_reply->vpn_lo = notif->vpn_lo;
}

void makeMissReply(int type, int tid, int pid, uint64_t va, uint64_t paddr,
                   QEMUMissReply *miss_reply) {
  miss_reply->type = sMissReply;
  miss_reply->tid = tid;
  miss_reply->pid = pid;
  miss_reply->vpn_hi = VPN_GET_HI(va);
  miss_reply->vpn_lo = VPN_GET_LO(va);

  miss_reply->permission = type;

  miss_reply->ppn = GET_PPN_FROM_PADDR(paddr);
}

void requireStateIsIdentical(const ArmflexArchState &state1,
                             const ArmflexArchState &state2) {
  REQUIRE(state1.pc == state2.pc);
  REQUIRE(GET_NZCV(state1.nzcv) == GET_NZCV(state2.nzcv));
  REQUIRE(state1.sp == state2.sp);
  for (int i = 0; i < 32; ++i) {
    REQUIRE(state1.xregs[i] == state2.xregs[i]);
  }
}

void initArchState(ArmflexArchState *state, uint64_t pc) {
  for (int i = 0; i < 32; ++i) {
    state->xregs[i] = i;
  }
  state->nzcv = 0;
  state->sp = 0;
  state->pc = pc;
}

void synchronizePage(FPGAContext *ctx, int asid, uint8_t *page, uint64_t vaddr,
                     uint64_t paddr, bool expect_modified) {
  INFO("Synchronize page");
  QEMUPageEvictRequest evict_request;
  makeEvictRequest(asid, vaddr, &evict_request);
  sendMessageToFPGA(ctx, &evict_request, sizeof(evict_request));

  // Let's query message. It should send an eviction message
  INFO("1. Query Eviction Notification");
  uint32_t buffer[16] = {0};
  queryMessageFromFPGA(ctx, (uint8_t *)buffer);

  PageEvictNotification *evict_notify = (PageEvictNotification *)buffer;
  REQUIRE(evict_notify->type == sEvictNotify);
  REQUIRE(evict_notify->pid == asid);
  REQUIRE(evict_notify->vpn_hi == VPN_GET_HI(vaddr));
  REQUIRE(evict_notify->vpn_lo == VPN_GET_LO(vaddr));

  // Let's query message. It should send an eviction completed message
  INFO("2. Query Eviction Notification Complete");
  queryMessageFromFPGA(ctx, (uint8_t *)buffer);

  evict_notify = (PageEvictNotification *)buffer;
  REQUIRE(evict_notify->type == sEvictDone);
  REQUIRE(evict_notify->pid == asid);
  REQUIRE(evict_notify->vpn_hi == VPN_GET_HI(vaddr));
  REQUIRE(evict_notify->vpn_lo == VPN_GET_LO(vaddr));
  REQUIRE(evict_notify->ppn == GET_PPN_FROM_PADDR(paddr));
  REQUIRE(evict_notify->permission == DATA_STORE);
  REQUIRE(evict_notify->modified == expect_modified);

  fetchPageFromFPGA(ctx, paddr, page);
}

void makeDeadbeefPage(uint8_t *pages, size_t bytes) {
  uint32_t *page_word = (uint32_t *)pages;
  for (int word = 0; word < bytes / 4; word++) {
    page_word[word] = 0xDEADBEEF;
  }
}

void makeZeroPage(uint8_t *page) {
  uint32_t *page_word = (uint32_t *)page;
  for (int word = 0; word < PAGE_SIZE / 4; word++) {
    page_word[word] = 0xFFFFFFFF;
  }
}