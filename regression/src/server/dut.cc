#include "dut.hh"
#include "verilated.h"
#include "../client/fpga_interface.h"
#include <mutex>
#include <thread>

double TopDUT::time = 0;

TopDUT::TopDUT(bool withTrace) {
  size_t dram_size = 1024 * 1024 * 16;
  dut = new Vdevteroflex_top();
  dram = new uint32_t[dram_size / 4];
  tfp = new VerilatedFstC;
  if(withTrace) {
    dut->trace(tfp, 99);
    tfp->open("test.fst");
  }
  ready_thread = 0;
  terminating = false;
  error_occurred = false;

  this->dram_size = dram_size;
  this->axi_base_addr = dram_size;
  size_t power2_dram_size = 0;
  for (power2_dram_size = 1; dram_size > 0;
       dram_size >>= 1, power2_dram_size <<= 1);
  this->dram_addr_mask = power2_dram_size - 1;

  decoupled_count = 0;
}

TopDUT::~TopDUT() {
  if (withTrace) {
    tfp->close();
    delete tfp;
  }
  dut->final();
  delete dut;
  delete[] dram;
}

void TopDUT::reset() {
  // get the lock of the dut
  std::unique_lock<std::mutex> lock(dut_mutex);
  dut->reset = 1;
  dut->eval();
  tfp->dump(time);
  time += 1;
  dut->clock = 1;
  dut->eval();
  tfp->dump(time);
  tfp->flush();
  time += 1;
  dut->clock = 0;
  dut->reset = 0;
  dut->eval();
}

bool TopDUT::waitForTick(std::unique_lock<std::mutex> &lock) {
  // update the number of ready thread
  ready_thread += 1;
  clock_cv.notify_all(); // try to notify the clock thread.
  if (terminating) {
    lock.unlock(); // release the lock
    return true;   // terminate the threads.
  }
  // wait for the clock to notify
  subroutine_cv.wait(lock);
  return false;
}

void TopDUT::reportError(const char* str_error) {
  perror(str_error);
  error_occurred = true;
}

bool TopDUT::tick() {
  // get the lock of the dut
  std::unique_lock<std::mutex> lock(dut_mutex);
  // tick clock
  dut->eval();
  tfp->dump(time);
  time += 1;
  dut->clock = 1;
  dut->eval();
  tfp->dump(time);
  time += 1;
  dut->clock = 0;
  dut->eval();
  // clear ready threads
  ready_thread = 0;

  // wait up all subroutines.
  subroutine_cv.notify_all();
  // collect all threads
  clock_cv.wait(lock, [&]() {
    return ready_thread == subroutines.size() - decoupled_count;
  });
  return error_occurred;
}

void TopDUT::join() {
  // setup terminating
  terminating = true;
  // tick
  tick();
  // wait for their complete.
  for (auto &t : subroutines) {
    t.join();
  }
  terminating = false;

  tfp->flush();
}

void TopDUT::decoupleCurrentRoutine(std::unique_lock<std::mutex> &lock) {
  for (const auto &t : subroutines) {
    if (t.get_id() == std::this_thread::get_id()) {
      decoupled_count += 1;
      lock.unlock(); // the lock is released
      return;
    }
  }
  puts("Warning: Try to decouple a thread that is not affiliated with the dut.");
  assert(false);
}

void TopDUT::attachCurrentRoutine(std::unique_lock<std::mutex> &lock) {
  for (const auto &t : subroutines) {
    if (t.get_id() == std::this_thread::get_id()) {
      lock.lock(); // get lock.
      decoupled_count -= 1;
      return;
    }
  }
  puts("Warning: Try to attach a thread that is not affiliated with the dut.");
  assert(false);
}


void TopDUT::closeSimulation(void) {
  puts("Closing simulation.\n");
  tfp->close();

  // dump DRAM to a file.
  FILE *f = fopen("dram.bin", "rb");
  fwrite(this->dram, 1, this->dram_size, f);
  fflush(f);
  fclose(f);
}

#ifdef DEBUG
int TopDUT::isInstructionBeingCommitted() {
  if(dut->dbg_bits_commit_valid && !dut->dbg_bits_commitIsTransplant) {
    return dut->dbg_bits_commit_tag;
  } else {
    return -1;
  }
}

int TopDUT::getTransplant(void) {
  if(dut->dbg_bits_transplant_valid) {
    return dut->dbg_bits_transplant_tag;
  } else {
    return -1;
  }
}

void TopDUT::getArchState(uint32_t thid, ArmflexArchState* state) {
  state->pc = uint64_t(dut->dbg_bits_stateVec_regs_PC[thid*2]) | uint64_t(dut->dbg_bits_stateVec_regs_PC[thid*2 + 1]) << 32;
  state->flags = uint64_t(dut->dbg_bits_stateVec_regs_flags[thid / 8] >> ((thid % 8) * 4)) & 0xF;
  for(int reg = 0; reg < 32; reg++) {
      state->xregs[reg] = uint64_t(dut->dbg_bits_stateVec_rfile[(thid*32 + reg) * 2]) | uint64_t(dut->dbg_bits_stateVec_rfile[(thid*32 + reg) * 2 + 1]) << 32;
  }
}
#endif
