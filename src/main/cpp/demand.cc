#include <stdint.h>

constexpr int VPN_WIDTH = 52;
constexpr int PPN_WIDTH = 24;

constexpr int THREAD_ID_WIDTH = 2;
constexpr int PROCESS_ID_WIDTH = 8;

/**
 * Page table entry
 */ 
struct PTEntry {
  uint32_t ppn        : PPN_WIDTH;  // physical page number
  uint32_t permission : 1;
  uint32_t modified   : 1;
};

/**
 * One Item in the page table, including virtual page number, process id, and virtual 
 */ 

struct PageTableItem {
  uint64_t v_page    : VPN_WIDTH;
  uint32_t p_id      : PROCESS_ID_WIDTH;
  PTEntry entry;
};

/**
 * Message to request the Page walk from the TLB.
 */ 
struct PageRequestMessage {
  uint64_t v_page     : VPN_WIDTH;
  uint32_t t_id       : THREAD_ID_WIDTH;
  uint32_t permission : 2;
};

/**
 * Message of entry eviction from the TLB.
 */ 
struct PageWritebackMessage {
  uint64_t v_page     : VPN_WIDTH;
  uint32_t t_id       : THREAD_ID_WIDTH;
  PTEntry  entry;
};

/**
 * The miss reply message from the QEMU.
 */
struct MissReplyMessage {
  uint64_t v_page     : VPN_WIDTH;
  uint32_t p_id       : PROCESS_ID_WIDTH;

  uint32_t permission : 2;

  uint32_t synonym    : 1;
  uint64_t s_v_page     : VPN_WIDTH;
  uint64_t s_p_id      : PROCESS_ID_WIDTH;
};

/**
 * The eviction reply message from the QEMU
 */
struct EvictReplyMessage {
  uint64_t v_page     : VPN_WIDTH;
  uint64_t p_id       : PROCESS_ID_WIDTH;
  uint32_t old_ppn    : PPN_WIDTH;
  uint32_t synonym    : 1;
};

enum MessangeType{
  ePageRequest,
  ePTEWriteback,
  eMissReply,
  eEvictReply
};

/**
 * A message that could be handled by the processor
 */
struct Message {
  MessangeType type;
  union {
    PageRequestMessage request;
    PageWritebackMessage writeback;
    MissReplyMessage miss;
    EvictReplyMessage evict;
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
 * @param vpn the virtual page number.
 * @param p_id the process id.
 */ 
void loadPTSet(uint32_t index, uint64_t vpn, uint32_t p_id);


/**
 * Look up the scracthpad to see if whether there is a hit?
 * This will call a special hardware to go over the scratchpad and wait for it complete.
 * 
 * @param index the index of the scratchpad. Select between 0 and 1.
 * @param vpn the virtual page number
 * @param p_id the process id.
 * @param result the look up result
 * @return whether the result is valid.
 * 
 * @note this will update the LRU automatically if there is a hit.
 */ 
bool lookupPT(uint32_t index, uint64_t vpn, uint32_t p_id, PTEntry *result);

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
void replaceLRU(uint32_t index, uint64_t vpn, uint32_t p_id, const PTEntry *entry);

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
  replaceLRU(index, item->v_page, item->p_id, &item->entry);
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
 * @param vpn the virtual page number
 * @param t_id the thread id.
 * @param entry the entry to response
 */ 
void responseToTLB(uint64_t vpn, uint32_t t_id, const PTEntry *entry);

/**
 * Flush the TLB by given virtual page number and thread id.
 * 
 * @param vpn the virtual page number
 * @param t_id the given thread_id.
 * @param entry the flushed entry if the flush request hits. It will not be changed if miss.
 * @return true if the entry is valid.
 * @note Always remember to put the entry back to the DRAM!
 */ 
bool flushTLBEntry(uint64_t vpn, uint32_t t_id, PTEntry *entry);

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
 */ 
void sendMissRequestToQEMU(const PageRequestMessage *request);

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
      case ePageRequest: {
        // 1. Transfer thread id to process id.
        uint32_t pid = lookupThreadTable(base.request.t_id);
        // 2. Look up Page table according to the vpage and pid. 
        loadPTSet(0, base.request.v_page, pid);
        PTEntry entry;
        // Here we have an invocation of DMA? (They should be considered in the )
        if(lookupPT(0, base.request.v_page, pid, &entry)){
          // hit? reponse to the TLB
          responseToTLB(base.request.t_id, base.request.v_page, &entry);
        } else {
          // miss.
          sendMissRequestToQEMU(&base.request);
        }
        break;
      }
      case ePTEWriteback: {
        uint32_t pid = lookupThreadTable(base.writeback.t_id);
        loadPTSet(0, base.writeback.v_page, pid);
        // Get victim
        PageTableItem item_to_evict;
        if(getLRU(0, &item_to_evict)){
          // Get entry from the TLB
          flushTLBEntry(item_to_evict.v_page, item_to_evict.p_id, &item_to_evict.entry);
          movePageToQEMU(&item_to_evict.entry);
        }
        replaceLRU(0, base.writeback.v_page, pid, &base.writeback.entry);
        syncPTSet(0);
        break;
      }
      case eMissReply: {
        // 1. determine eviction
        loadPTSet(0, base.miss.v_page, base.miss.p_id);
        PageTableItem item_to_evict;
        if(getLRU(0, &item_to_evict)){
          flushTLBEntry(item_to_evict.v_page, item_to_evict.p_id, &item_to_evict.entry);
          movePageToQEMU(&item_to_evict.entry);
        }
        // 2. determine eviction?
        PTEntry entry;
        if(base.miss.synonym){
          loadPTSet(1, base.miss.s_v_page, base.miss.s_p_id);
          lookupPT(1, base.miss.s_v_page, base.miss.s_p_id, &entry); // assert hit!
        } else {
          // pop the entry from the Pool
          entry.ppn = getFreePPN();
          entry.modified = 0;
          entry.permission = base.miss.permission;
        }
        replaceLRU(0, base.miss.v_page, base.miss.p_id, &entry);
        // TODO: Move the page to the DRAM?
        insertPageFromQEMU(entry.ppn);
        syncPTSet(0);
        break;
      }
      case eEvictReply: {
        if(!base.evict.synonym){
          recyclePPN(base.evict.old_ppn);
        }
        break;
      }
    }
  }
}
