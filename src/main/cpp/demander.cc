#include "demander_api.hpp"

// TODO: In order to easily test the module, split the functions to different files.

int main(){
  while(true){
    volatile Message *base = fetchMessage();
    if(base == nullptr){
      continue;
    }
    switch(base->type){
      case eTLBMissRequest: {
        // 1. Transfer thread id to process id.
        PTTag pt_tag; // with thread id replaced.
        pt_tag.process_id = lookupThreadTable(base->tlb_request.tag.thread_id);
        pt_tag.vpn = base->tlb_request.tag.vpn;
        // 2. Look up Page table according to the vpage and pid. 
        loadPTSet(0, &pt_tag);
        PTEntry entry;
        // Here we have an invocation of DMA? (They should be considered in the )
        if(lookupPT(0, &pt_tag, &entry)){
          // hit? reponse to the TLB
          responseToTLB(base->tlb_request.permission == 2 ? 0 : 1, &pt_tag, &entry);
        } else {
          // miss.
          sendMissRequestToQEMU(&pt_tag, base->tlb_request.permission);
        }
        // Update LRU bits and sync back?
        syncPTSet(0);
        break;
      }
      case eTLBEvict: {
        PTTag pt_tag; // with thread id replaced.
        pt_tag.process_id = lookupThreadTable(base->tlb_evict.tag.thread_id);
        pt_tag.vpn = base->tlb_evict.tag.vpn;
        loadPTSet(0, &pt_tag);
        // Get victim
        PageTableItem item_to_evict;
        if(getLRU(0, &item_to_evict)){
          // Get entry from the TLB
          flushTLBEntry(base->tlb_evict.entry.permission == 2 ? 0 : 1, &item_to_evict.tag, &item_to_evict.entry);
          movePageToQEMU(&item_to_evict.entry);
        }
        replaceLRU(0, &pt_tag, &base->tlb_evict.entry);
        syncPTSet(0);
        break;
      }
      case eQEMUMissReply: {
        // 1. determine eviction
        loadPTSet(0, &base->qemu_miss.tag);
        PageTableItem item_to_evict;
        if(getLRU(0, &item_to_evict)){
          // How to determine which to flush?
          flushTLBEntry(base->qemu_miss.permission == 2 ? 0 : 1, &item_to_evict.tag, &item_to_evict.entry);
          movePageToQEMU(&item_to_evict.entry);
        }
        // 2. determine eviction?
        PTEntry entry;
        if(base->qemu_miss.synonym_valid){
          loadPTSet(1, &base->qemu_miss.synonym);
          lookupPT(1, &base->qemu_miss.synonym, &entry); // assert hit!
        } else {
          // pop the entry from the Pool
          entry.ppn = getFreePPN();
          entry.modified = 0;
          entry.permission = base->qemu_miss.permission;
          insertPageFromQEMU(entry.ppn);
        }
        replaceLRU(0, &base->qemu_miss.tag, &entry);
        syncPTSet(0);
        responseToTLB(base->qemu_miss.permission == 2 ? 0 : 1, &base->qemu_miss.tag, &entry);
        break;
      }
      case eQEMUEvictReply: {
        if(!base->qemu_evict.synonym_valid){
          recyclePPN(base->qemu_evict.ppn);
        }
        break;
      }
    }
  }
}
