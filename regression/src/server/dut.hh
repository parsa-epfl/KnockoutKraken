#pragma once

#include "Vdevteroflex_top.h"
#include "../client/fpga_interface.h"
#include <exception>
#include <mutex>
#include <thread>
#include <condition_variable>
#include <vector>
#include <functional>

#include "verilated.h"
#include "verilated_fst_c.h"

#define TICK(dut)                                                   \
  if (dut.waitForTick(lock))                                        \
    return;

#define CHECK(dut, state, message)                                  \
  if (!(state)) {                                                   \
    printf("Assertion Failed (Line: %d): %s\n", __LINE__, message); \
    dut.reportError("Failed Assertion");                            \
    goto wait_for_join;                                             \
  }

#define BLOCK_SIZE     (512)
#define WORD_SIZE      (32)
#define WORDS_PER_BLOCK (BLOCK_SIZE/WORD_SIZE)
#define BYTES_PER_BLOCK (BLOCK_SIZE/8)
#define BYTE_TO_WORD(addr) (addr >> 2) // log2(32/8) = 2

class TopDUT {
public:
  TopDUT(bool withTrace = true);
  ~TopDUT();


  /**
   * A pointer replacement.
   */
  Vdevteroflex_top *operator-> (){
    return dut;
  }

  /**
   * Get commit state.
   */
  void getArchState(uint32_t thid, ArmflexArchState* state);
  int isInstructionBeingCommitted();
  int getTransplant();

 
  /**
   * Reset the module. 
   * 
   * @note Only called from the main thread.
   */ 
  void reset();

  /**
   * Wait for a positive clock edge
   * 
   * @param lock the lock that current thread has.
   * @return true if the main thread suggests a termination.
   * 
   * @note only called from subroutines.
   */
  bool waitForTick(std::unique_lock<std::mutex> &lock);


  /**
   * Report an error to the main thread so that it will stop in next iteration.
   * 
   * @note only called from subroutines.
   */ 
  void reportError(const char* str_error);

  /**
   * Trigger a positive clock edge and activate all subroutines.
   * 
   * @return true if error occurred.
   * 
   * @note only called from the main thread
   */
  bool tick();

  /**
   * join all subroutines.
   * 
   * @note only called from the main thread
   */
  void join();

  /**
   * Register a subroute.
   * 
   * @param func void(this, std::dut_mutex) the subroute that will run on the subroute
   * 
   * @note Thr subroutine will be attached initially.
   * 
   */
  void registerSubroutine(std::function<void(TopDUT &)> func){
    std::lock_guard<std::mutex> lock{this->dut_mutex}; // make sure the thread will not start before correctly registed.
    subroutines.push_back(std::thread(func, std::ref(*this)));
  }

  /**
   * Get the mutex of the DUT.
   * 
   * @return the mutex.
   */
  std::mutex &getLock() {
    return dut_mutex;
  }
  
  /**
   * Decouple a routine from the dut.
   * 
   * @param lock the lock that current thread has.
   * 
   * @note must called from a registed and attached subroutine. It will release the lock that current routine has.
   */ 
  void decoupleCurrentRoutine(std::unique_lock<std::mutex> &lock);

  /**
   * Attach a routine from the dut.
   * 
   * @param lock the lock that current thread has.
   * 
   * @note must called from a register and detached subroutine.It will make the thread to reacquire the lock.
   */ 
  void attachCurrentRoutine(std::unique_lock<std::mutex> &lock);

  /**
   * The variable used for recording the 
   */ 
  static double time;

  bool isAXIDRAM(uint64_t addr) {
    return (addr < dram_size);
  }

  void closeSimulation();

public:
  uint32_t *dram;
  size_t dram_size;
  size_t dram_addr_mask;
  size_t axi_base_addr;

private:
  std::condition_variable subroutine_cv;

  std::condition_variable clock_cv;
  std::mutex dut_mutex;

  size_t ready_thread;
  size_t decoupled_count;
  std::vector<std::thread> subroutines;
  bool terminating;
  bool withTrace;

  bool error_occurred;

  Vdevteroflex_top *dut;
  VerilatedFstC *tfp;
};

