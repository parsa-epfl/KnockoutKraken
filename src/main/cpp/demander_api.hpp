#pragma once

#include <stdint.h>

constexpr int VPN_WIDTH = 52;
constexpr int PPN_WIDTH = 24;

constexpr int THREAD_ID_WIDTH = 2;
constexpr int PROCESS_ID_WIDTH = 16;

/**
 * The tag of a PTE in TLB. Note that we're using thread id rather than process id.
 */ 
struct TLBTag {
  uint64_t vpn;
  uint32_t thread_id;
};

/**
 * The tag of a PTE in PT. process id rather than thread id
 */ 
struct PTTag {
  uint64_t vpn;
  uint32_t process_id;
};

/**
 * Page table entry
 */ 
struct PTEntry {
  uint32_t ppn;
  uint32_t permission;
  uint32_t modified;
};

/**
 * One Item in the page table, including virtual page number, process id, and virtual 
 */ 

struct PageTableItem {
  PTTag tag;
  PTEntry entry;
};

/**
 * Message to request the Page walk from the TLB.
 */ 
struct TLBMissRequestMessage {
  TLBTag tag;
  uint32_t permission;
};

/**
 * Message of entry eviction from the TLB.
 */ 
struct TLBEvictionMessage {
  TLBTag tag;
  PTEntry entry;
};

/**
 * The miss reply message from the QEMU.
 */
struct QEMUMissReplyMessage {
  PTTag tag;
  uint32_t permission;

  uint32_t synonym_valid;
  PTTag synonym;
};

/**
 * The eviction reply message from the QEMU
 */
struct QEMUEvictReplyMessage {
  PTTag tag;
  uint32_t ppn;
  uint32_t synonym_valid;
};

enum MessangeType{
  eTLBMissRequest = 0,
  eTLBEvict = 1,
  eQEMUMissReply = 2,
  eQEMUEvictReply = 3
};

/**
 * A message that could be handled by the processor
 */
struct Message {
  MessangeType type;
  union {
    TLBMissRequestMessage tlb_request;
    TLBEvictionMessage tlb_evict;
    QEMUMissReplyMessage qemu_miss;
    QEMUEvictReplyMessage qemu_evict;
    uint32_t raw[8];
  };
};

/**
 * fetch the Message from a given address.
 * @return the address if the message is valid. nullptr if not.
 */
inline volatile Message *fetchMessage(){
  volatile int *valid_ptr = reinterpret_cast<int *>(0x20000);
  volatile Message *outer_mess = reinterpret_cast<Message *>(0x20004);
  if(*valid_ptr){
    // copy
    return outer_mess;
  }
  return nullptr;
}

/**
 * Access the Tread table (which stores thread_id-process_id pair) and find the process id.
 * 
 * @param src the given thread id.
 * @return the process id.
 */ 
inline uint32_t lookupThreadTable(uint32_t src){
  volatile uint32_t *base_ptr = reinterpret_cast<uint32_t *>(0x30000);
  return base_ptr[src];
}

struct PageTableSetCSR {
  uint64_t vpn;
  uint32_t process_id;
  uint32_t index;
  uint32_t load_dma;
  uint32_t store_dma;
  uint32_t ppn;
  uint32_t permission;
  uint32_t modified;
  uint32_t lookup;
  uint32_t lru_result;
  uint32_t replace_lru;
  uint32_t ready;
};

/**
 * Call DMA to move the PT Set to one scratchpad. Return when finish.
 * The function will automatically calculate the set address internally.
 * 
 * @param index the index of the scratchpad. Select between 0 and 1.
 * @param tag the tag of the expected PTE.
 */ 
inline void loadPTSet(uint32_t index, const volatile PTTag *tag){
  volatile PageTableSetCSR *base_ptr = reinterpret_cast<PageTableSetCSR *>(0x40000);
  base_ptr->vpn = tag->vpn;
  base_ptr->process_id = tag->process_id;
  base_ptr->index = index;
  base_ptr->load_dma = 1;
  while(!base_ptr->load_dma);
}


/**
 * Look up the scracthpad to see if whether there is a hit?
 * This will call a special hardware to go over the scratchpad and wait for it complete.
 * 
 * @param index the index of the scratchpad. Select between 0 and 1.
 * @param tag the tag of the expected PTE.
 * @param result the look up result
 * @return whether the result is valid.
 * 
 * @note this will update the LRU automatically if there is a hit.
 */ 
inline bool lookupPT(uint32_t index, const volatile PTTag *tag, PTEntry *result){
  volatile PageTableSetCSR *base_ptr = reinterpret_cast<PageTableSetCSR *>(0x40000);
  base_ptr->vpn = tag->vpn;
  base_ptr->process_id = tag->process_id;
  base_ptr->lookup = 1;
  while(!base_ptr->ready);
  if(base_ptr->lookup){
    result->modified = base_ptr->modified;
    result->permission = base_ptr->permission;
    result->ppn = base_ptr->ppn;
    return true;
  }
  return false;
}

/**
 * Get LRU entry of the PT Set in the scratchpad.
 * 
 * @param index the index of the scratchpad.
 * @param item the result if valid.
 * @return true if the result is valid. Returning false means there is an empty place.
 */ 
inline bool getLRU(uint32_t index, PageTableItem *item){
  volatile PageTableSetCSR *base_ptr = reinterpret_cast<PageTableSetCSR *>(0x40000);
  base_ptr->index = index;
  base_ptr->lru_result = 1;
  if(base_ptr->lru_result){
    item->entry.modified = base_ptr->modified;
    item->entry.permission = base_ptr->permission;
    item->entry.ppn = base_ptr->ppn;
    item->tag.process_id = base_ptr->process_id;
    item->tag.vpn = base_ptr->vpn;
    return true;
  }
  return false;
}

/**
 * Replace the LRU entry in the scratchpad with the new given entry.
 * Make sure the PT Set is in the scracthpad before call this function.
 * 
 * @param index the index of the scratchpad.
 * @param vpn the virtual page number of the entry
 * @param p_id the process id of the entry
 * @param entry the new given entry.
 */ 
inline void replaceLRU(uint32_t index, const volatile PTTag *tag, const volatile PTEntry *entry){
  volatile PageTableSetCSR *base_ptr = reinterpret_cast<PageTableSetCSR *>(0x40000);
  base_ptr->index = index;
  base_ptr->vpn = tag->vpn;
  base_ptr->process_id = tag->process_id;
  base_ptr->ppn = entry->ppn;
  base_ptr->permission = entry->permission;
  base_ptr->modified = entry->modified;
  base_ptr->replace_lru = 1;
}

/**
 * Replace the LRU entry in the scratchpad with the new given entry.
 * Make sure the PT Set is in the scracthpad before call this function.
 * 
 * @param index the index of the scratchpad.
 * @param item the Page Table item (tag + entry)
 * 
 * @overload function of replaceLRU
 */ 
inline void replaceLRU(uint32_t index, const volatile PageTableItem *item){
  replaceLRU(index, &item->tag, &item->entry);
}

/**
 * Sync the scratchpad with PT in the DRAM.
 * 
 * @param the index of the scratchpad.
 */ 
inline void syncPTSet(uint32_t index){
  volatile PageTableSetCSR *base_ptr = reinterpret_cast<PageTableSetCSR *>(0x40000);
  base_ptr->store_dma = 1;
  while(!base_ptr->store_dma);
}

struct TLBControllerCSR {
  uint64_t vpn;
  uint32_t process_id;
  uint32_t ppn;
  uint32_t permission;
  uint32_t modified;
  uint32_t which_tlb;
  uint32_t flush;
  uint32_t flush_hit;
  uint32_t response;
};

/**
 * Send response to the TLB. This will refill the TLB with the given entry.
 * 
 * @param which_tlb 0: I_TLB, 1: D_TLB
 * @param tag the tag of the entry to reply.
 * @param entry the entry to response
 * 
 * @note The hardware will automatically lookup the thread_id by the process id.
 */ 
inline void responseToTLB(uint32_t which_tlb, const volatile PTTag *tag, const PTEntry *entry){
  volatile TLBControllerCSR *base_ptr = reinterpret_cast<TLBControllerCSR *>(0x50000);
  base_ptr->which_tlb = which_tlb;
  base_ptr->vpn = tag->vpn;
  base_ptr->process_id = tag->process_id;
  base_ptr->ppn = entry->ppn;
  base_ptr->modified = entry->modified;
  base_ptr->permission = entry->permission;
  while(!base_ptr->response); 
  base_ptr->response = 1;
}

/**
 * Flush the TLB by given virtual page number and thread id.
 * 
 * @param which_tlb 0: I_TLB, 1: D_TLB
 * @param tag the tag for the entry to flush. It's possible that this thread is not registered in the ThreadTable
 * @param entry the flushed entry if the flush request hits. It will not be changed if miss.
 * @return true if the entry is valid.
 * @note Always remember to put the entry back to the DRAM!
 */ 
inline bool flushTLBEntry(uint32_t which_tlb, const PTTag *tag, PTEntry *entry){
  volatile TLBControllerCSR *base_ptr = reinterpret_cast<TLBControllerCSR *>(0x50000);
  base_ptr->which_tlb = which_tlb;
  base_ptr->vpn = tag->vpn;
  base_ptr->process_id = tag->process_id;

  base_ptr->flush = 1;
  while(!base_ptr->flush);

  if(base_ptr->flush_hit){
    // TODO: try to
    entry->modified = base_ptr->modified;
    entry->permission = base_ptr->permission;
    entry->ppn = base_ptr->ppn;
    return true;
  }
  return false;
}

struct FreeListCSR {
  uint32_t pop;
  uint32_t push;
  uint32_t result;
};

/**
 * Pop a free physical page number from the free list.
 * @return the physical page number.
 */ 
inline uint32_t getFreePPN(){
  volatile FreeListCSR *base_ptr = reinterpret_cast<FreeListCSR *>(0x60000);
  while(!base_ptr->pop);
  base_ptr->pop = 1;
  return base_ptr->result;
}

/**
 * Recycle a physical page number and push it in the pool.
 * @param ppn the page number to be free.
 */ 
inline void recyclePPN(uint32_t ppn){
  volatile FreeListCSR *base_ptr = reinterpret_cast<FreeListCSR *>(0x60000);
  while(!base_ptr->push);
  base_ptr->result = ppn;
  base_ptr->push = 1;
}

/**
 * Forward *Miss* Request to the QEMU side.
 * 
 * @param request the page request received from the TLB.
 * 
 */ 
inline void sendMissRequestToQEMU(const PTTag *tag, const uint32_t permission){
  struct page_fault_request_t {
    uint64_t vpn;
    uint32_t process_id;
    uint32_t permission;
  };
  volatile uint32_t *control_ptr = reinterpret_cast<uint32_t *>(0x70000);
  volatile page_fault_request_t *base_ptr = reinterpret_cast<page_fault_request_t *>(0x70004);
  base_ptr->permission = permission;
  base_ptr->vpn = tag->vpn;
  base_ptr->process_id = tag->process_id;
  *control_ptr = 1;
  while(!control_ptr);
}

/**
 * Send request to the DMA to evict a page. This will flush I$ and D$ before actually moving the page.
 * 
 * @param entry the page table entry pointing to the page.
 */ 
inline void movePageToQEMU(const PTEntry *entry){
  struct page_deleater_csr_t {
    uint32_t ppn;
    uint32_t permission;
    uint32_t modified;
    uint32_t control;
  };
  volatile page_deleater_csr_t *control_ptr = reinterpret_cast<page_deleater_csr_t *>(0x80000);
  control_ptr->ppn = entry->ppn;
  control_ptr->permission = entry->permission;
  control_ptr->modified = entry->modified;
  while(!control_ptr->control);
  control_ptr->control = 1;
}

/**
 * Insert the page from the buffer of QEMU to the DRAM.
 * 
 * @param ppn the physical page number of the page.
 */ 
inline void insertPageFromQEMU(uint32_t ppn){
  struct page_inserter_csr_t {
    uint32_t ppn;
    uint32_t control;
  };
  volatile page_inserter_csr_t *control_ptr = reinterpret_cast<page_inserter_csr_t *>(0x90000);
  while(!control_ptr->control);
  control_ptr->ppn = ppn;
  control_ptr->control = 1;
}

