#include "demander_api.hpp"

// TODO: In order to easily test the module, split the functions to different files.

int main(){
  while(true){
    volatile int *message_available = reinterpret_cast<int *>(0x20000);
    if(*message_available == 0){
      continue;
    }
    const volatile MessangeType *message_type = reinterpret_cast<MessangeType *>(0x20004);
    switch(*message_type){
      case eTLBMissRequest: {
        const volatile TLBMissRequestMessage *tlb_request = reinterpret_cast<TLBMissRequestMessage *>(0x20008);
        // 1. Transfer thread id to process id.
        PTTag pt_tag; // with thread id replaced.
        pt_tag.process_id = lookupThreadTable(tlb_request->tag.thread_id);
        pt_tag.vpn = tlb_request->tag.vpn;
        // 2. Look up Page table according to the vpage and pid. 
        loadPTSet(0, &pt_tag);
        PTEntry entry;
        // Here we have an invocation of DMA? (They should be considered in the )
        if(lookupPT(0, &pt_tag, &entry)){
          // hit? reponse to the TLB
          responseToTLB(tlb_request->permission == 2 ? 0 : 1, &pt_tag, &entry);
        } else {
          // miss.
          sendMissRequestToQEMU(&pt_tag, tlb_request->permission);
        }
        // Update LRU bits and sync back?
        // syncPTSet(0);
        break;
      }
      case eTLBEvict: {
        const volatile TLBEvictionMessage *tlb_evict = reinterpret_cast<TLBEvictionMessage *>(0x20008);
        PTTag pt_tag; // with thread id replaced.
        pt_tag.process_id = lookupThreadTable(tlb_evict->tag.thread_id);
        pt_tag.vpn = tlb_evict->tag.vpn;
        loadPTSet(0, &pt_tag);
        // Get victim
        PageTableItem item_to_evict;
        if(getLRU(0, &item_to_evict)){
          // Get entry from the TLB
          flushTLBEntry(tlb_evict->entry.permission == 2 ? 0 : 1, &item_to_evict.tag, &item_to_evict.entry);
          movePageToQEMU(&item_to_evict.entry);
        }
        replaceLRU(0, &pt_tag, &tlb_evict->entry);
        syncPTSet(0);
        break;
      }
      case eQEMUMissReply: {
        const volatile QEMUMissReplyMessage *qemu_miss = reinterpret_cast<QEMUMissReplyMessage *>(0x20008);
        // 1. determine eviction
        loadPTSet(0, &qemu_miss->tag);
        PageTableItem item_to_evict;
        if(getLRU(0, &item_to_evict)){
          // How to determine which to flush?
          flushTLBEntry(qemu_miss->permission == 2 ? 0 : 1, &item_to_evict.tag, &item_to_evict.entry);
          movePageToQEMU(&item_to_evict.entry);
        }
        // 2. determine eviction?
        PTEntry entry;
        if(qemu_miss->synonym_valid){
          loadPTSet(1, &qemu_miss->synonym);
          lookupPT(1, &qemu_miss->synonym, &entry); // assert hit!
        } else {
          // pop the entry from the Pool
          entry.ppn = getFreePPN();
          entry.modified = 0;
          entry.permission = qemu_miss->permission;
          insertPageFromQEMU(entry.ppn);
        }
        replaceLRU(0, &qemu_miss->tag, &entry);
        syncPTSet(0);
        responseToTLB(qemu_miss->permission == 2 ? 0 : 1, &qemu_miss->tag, &entry);
        break;
      }
      case eQEMUEvictReply: {
        const volatile QEMUEvictReplyMessage *qemu_evict = reinterpret_cast<QEMUEvictReplyMessage *>(0x20008);
        if(!qemu_evict->synonym_valid){
          recyclePPN(qemu_evict->ppn);
        }
        break;
      }
    }
    // pop the message
    *message_available = 1;
  }
}
