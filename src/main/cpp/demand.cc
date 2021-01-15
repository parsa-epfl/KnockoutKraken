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
  eTLBMissRequest,
  eTLBEvict,
  eQEMUMissReply,
  eQEMUEvictReply
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
  };
};

/**
 * fetch the Message from a given address.
 * 
 * @param message the pointer of the message body
 * @return if the message is valid.
 */
bool fetchMessage(Message *message);

/**
 * Access the Tread table (which stores thread_id-process_id pair) and find the process id.
 * 
 * @param src the given thread id.
 * @return the process id.
 */ 
uint32_t lookupThreadTable(uint32_t src);

/**
 * Call DMA to move the PT Set to one scratchpad. Return when finish.
 * The function will automatically calculate the set address internally.
 * 
 * @param index the index of the scratchpad. Select between 0 and 1.
 * @param tag the tag of the expected PTE.
 */ 
void loadPTSet(uint32_t index, const PTTag *tag);


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
bool lookupPT(uint32_t index, const PTTag *tag, PTEntry *result);

/**
 * Get LRU entry of the PT Set in the scratchpad.
 * 
 * @param index the index of the scratchpad.
 * @param item the result if valid.
 * @return true if the result is valid. Returning false means there is an empty place.
 */ 
bool getLRU(uint32_t index, PageTableItem *item);

/**
 * Replace the LRU entry in the scratchpad with the new given entry.
 * Make sure the PT Set is in the scracthpad before call this function.
 * 
 * @param index the index of the scratchpad.
 * @param vpn the virtual page number of the entry
 * @param p_id the process id of the entry
 * @param entry the new given entry.
 */ 
void replaceLRU(uint32_t index, const PTTag *tag, const PTEntry *entry);

/**
 * Replace the LRU entry in the scratchpad with the new given entry.
 * Make sure the PT Set is in the scracthpad before call this function.
 * 
 * @param index the index of the scratchpad.
 * @param item the Page Table item (tag + entry)
 * 
 * @overload function of replaceLRU
 */ 
void replaceLRU(uint32_t index, const PageTableItem *item){
  replaceLRU(index, &item->tag, &item->entry);
}

/**
 * Sync the scratchpad with PT in the DRAM.
 * 
 * @param the index of the scratchpad.
 */ 
void syncPTSet(uint32_t index);

/**
 * Send response to the TLB. This will refill the TLB with the given entry.
 * 
 * @param tag the tag of the entry to reply.
 * @param entry the entry to response
 */ 
void responseToTLB(const TLBTag *tag, const PTEntry *entry);

/**
 * Flush the TLB by given virtual page number and thread id.
 * 
 * @param tag the tag for the entry to flush. It's possible that this thread is not registered in the ThreadTable
 * @param entry the flushed entry if the flush request hits. It will not be changed if miss.
 * @return true if the entry is valid.
 * @note Always remember to put the entry back to the DRAM!
 */ 
bool flushTLBEntry(const PTTag *tag, PTEntry *entry);

/**
 * Pop a free physical page number from the free list.
 * @return the physical page number.
 */ 
uint32_t getFreePPN();

/**
 * Recycle a physical page number and push it in the pool.
 * @param ppn the page number to be free.
 */ 
void recyclePPN(uint32_t ppn);

/**
 * Forward *Miss* Request to the QEMU side.
 * 
 * @param request the page request received from the TLB.
 * 
 * TODO: There is a problem. Mesage forwarding to QEMU should use p_id instead of t_id
 */ 
void sendMissRequestToQEMU(const PTTag *tag, const uint32_t permission);

/**
 * Send request to the DMA to evict a page. This will flush I$ and D$ before actually moving the page.
 * 
 * @param entry the page table entry pointing to the page.
 */ 
void movePageToQEMU(const PTEntry *entry);

/**
 * Insert the page from the buffer of QEMU to the DRAM.
 * 
 * @param ppn the physical page number of the page.
 */ 
void insertPageFromQEMU(uint32_t ppn);

int main(){
  Message base;
  while(true){
    if(!fetchMessage(&base)){
      continue;
    }
    switch(base.type){
      case eTLBMissRequest: {
        // 1. Transfer thread id to process id.
        PTTag pt_tag; // with thread id replaced.
        pt_tag.process_id = lookupThreadTable(base.tlb_request.tag.thread_id);
        pt_tag.vpn = base.tlb_request.tag.vpn;
        // 2. Look up Page table according to the vpage and pid. 
        loadPTSet(0, &pt_tag);
        PTEntry entry;
        // Here we have an invocation of DMA? (They should be considered in the )
        if(lookupPT(0, &pt_tag, &entry)){
          // hit? reponse to the TLB
          responseToTLB(&base.tlb_request.tag, &entry);
        } else {
          // miss.
          sendMissRequestToQEMU(&pt_tag, base.tlb_request.permission);
        }
        break;
      }
      case eTLBEvict: {
        PTTag pt_tag; // with thread id replaced.
        pt_tag.process_id = lookupThreadTable(base.tlb_evict.tag.thread_id);
        pt_tag.vpn = base.tlb_evict.tag.vpn;
        loadPTSet(0, &pt_tag);
        // Get victim
        PageTableItem item_to_evict;
        if(getLRU(0, &item_to_evict)){
          // Get entry from the TLB
          // TODO: Add a function to look TID by PID
          flushTLBEntry(&item_to_evict.tag, &item_to_evict.entry);
          movePageToQEMU(&item_to_evict.entry);
        }
        replaceLRU(0, &pt_tag, &base.tlb_evict.entry);
        syncPTSet(0);
        break;
      }
      case eQEMUMissReply: {
        // 1. determine eviction
        loadPTSet(0, &base.qemu_miss.tag);
        PageTableItem item_to_evict;
        if(getLRU(0, &item_to_evict)){
          flushTLBEntry(&item_to_evict.tag, &item_to_evict.entry);
          movePageToQEMU(&item_to_evict.entry);
        }
        // 2. determine eviction?
        PTEntry entry;
        if(base.qemu_miss.synonym_valid){
          loadPTSet(1, &base.qemu_miss.synonym);
          lookupPT(1, &base.qemu_miss.synonym, &entry); // assert hit!
        } else {
          // pop the entry from the Pool
          entry.ppn = getFreePPN();
          entry.modified = 0;
          entry.permission = base.qemu_miss.permission;
        }
        replaceLRU(0, &base.qemu_miss.tag, &entry);
        // TODO: Move the page to the DRAM?
        insertPageFromQEMU(entry.ppn);
        syncPTSet(0);
        break;
      }
      case eQEMUEvictReply: {
        if(!base.qemu_evict.synonym_valid){
          recyclePPN(base.qemu_evict.ppn);
        }
        break;
      }
    }
  }
}
