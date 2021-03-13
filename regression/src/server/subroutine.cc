#include "subroutine.hh"
#include "dut.hh"
#include "ipc.hh"

#include <mutex>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

#define TICK()                                                      \
  if (dut.waitForTick(lock))                                        \
    return;

#define CHECK(state, message)                                       \
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

void handleDRAMSubroute(TopDUT &dut) {
  std::unique_lock<std::mutex> lock(dut.getLock());
  while (true) {
    if (dut->M_AXI_ar_arvalid) {
      uint64_t addr_byte = dut->M_AXI_ar_araddr;
      // check address. It should be small enough.
      CHECK(addr_byte < dut.dram_size, "AXI read address out of bound");
      CHECK((addr_byte & 0x3F) == 0, "AXI read address is not aligned");
      CHECK((dut->M_AXI_ar_arsize == 6), "AXI read burst count must be 64B");
      uint32_t id = dut->M_AXI_ar_arid;
      uint32_t burst_count = dut->M_AXI_ar_arlen + 1;
      dut->M_AXI_ar_arready = 1;

      TICK();

      dut->M_AXI_ar_arready = 0;
      dut->eval();

      printf("SHELL:FPGA:AXI DRAM:RD[0x%lx]:BURST[%i]\n", addr_byte, burst_count);
      printf("           WORD ADDR[0x%lx]\n", BYTE_TO_WORD(addr_byte));
      for (int curr_block = 0; curr_block < burst_count; ++curr_block) {
        size_t addr_word = BYTE_TO_WORD(addr_byte) + curr_block * WORDS_PER_BLOCK;

        dut->M_AXI_r_rid = id;
        dut->M_AXI_r_rresp = 0;
        for (size_t sub_word = 0; sub_word < WORDS_PER_BLOCK; ++sub_word) {
          dut->M_AXI_r_rdata[sub_word] = dut.dram[addr_word + sub_word];
        }
        dut->M_AXI_r_rlast = curr_block == burst_count - 1 ? 1 : 0;
        dut->M_AXI_r_rvalid = true;
        dut->eval();

        // wait for reading data accepted.
        while (!dut->M_AXI_r_rready) {
          TICK();
        }

        TICK();
        dut->M_AXI_r_rvalid = false;
        dut->eval();
      }
    }

    if (dut->M_AXI_aw_awvalid) {
      uint64_t addr_byte = dut->M_AXI_aw_awaddr;
      CHECK(addr_byte < dut.dram_size, "AXI write address out of bound");
      CHECK((addr_byte & 0x3F) == 0, "AXI write address is not aligned");
      CHECK((dut->M_AXI_ar_arsize == 6), "AXI write burst count must be 64B");
      uint32_t id = dut->M_AXI_aw_awid;
      uint32_t burst_count = dut->M_AXI_aw_awlen + 1;
      dut->M_AXI_aw_awready = 1;
      dut->eval();
      TICK();

      dut->M_AXI_aw_awready = 0;
      dut->eval();
      printf("SHELL:FPGA:AXI DRAM:WR[0x%lx]:BURST[%u]\n", addr_byte, burst_count);
      printf("           WORD ADDR[0x%lx]\n", BYTE_TO_WORD(addr_byte));
      for (int curr_block = 0; curr_block < burst_count; ++curr_block) {

        // setup the write to be ready.
        dut->M_AXI_w_wready = true;
        dut->eval();

        // wait for data to be valid.
        while (!dut->M_AXI_w_wvalid) {
          TICK();
        }

        size_t addr_word = BYTE_TO_WORD(addr_byte) + curr_block * WORDS_PER_BLOCK;
        for (size_t sub_word = 0; sub_word < WORDS_PER_BLOCK; ++sub_word) {
          dut.dram[addr_word + sub_word] = dut->M_AXI_w_wdata[sub_word];
        }
        

        TICK();

        dut->M_AXI_w_wready = false;
        dut->eval();
      }

      dut->M_AXI_b_bvalid = true;
      dut->M_AXI_b_bid = id;
      dut->M_AXI_b_bresp = 0;
      dut->eval();
      while (!dut->M_AXI_b_bready) {
        TICK();
      }
      TICK();

      dut->M_AXI_b_bvalid = false;
      dut->eval();
    }

    TICK();
  }
wait_for_join:
  while(1) {
    TICK();
  }
}



void AXILRoutine(TopDUT &dut, IPCServer &ipc) {
  std::unique_lock<std::mutex> lock(dut.getLock());
  while (true) {
    MemoryRequestAXIL result;
    // dut.decoupleCurrentRoutine(lock);
    while(!ipc.getRequestAXIL(&result)) {
      if(ipc.hasPendingError()) {
        dut.reportError("   ERROR:SOCKET:AXIL: Failed to get request.\n");
        TICK();
        goto wait_for_join;
      }
      TICK();
    }
    // dut.attachCurrentRoutine(lock);
    CHECK(result.byte_size == 4, "AXIL write size must be 4B."); // only one word.
    // we have the read result now.
    if (result.is_write) {
      printf("SHELL:HOST:AXIL:WR[0x%lx]:BURST[%lu]:DATA[%x]\n", result.addr, result.byte_size, result.w_data);
      // it's a writing request, activate AXI writing request channel.
      dut->S_AXIL_aw_awaddr = result.addr;
      dut->S_AXIL_aw_awprot = 0;
      dut->S_AXIL_aw_awvalid = true;
      dut->eval();
      while (!dut->S_AXIL_aw_awready) {
        TICK();
      }
      // wait one cycle
      TICK();

      dut->S_AXIL_aw_awvalid = false;
      dut->eval();
      // pushing data.
      dut->S_AXIL_w_wdata = result.w_data;
      dut->S_AXIL_w_wstrb = 0xF;
      dut->S_AXIL_w_wvalid = true;
      dut->eval();
      // wait for ready signal
      while (!dut->S_AXIL_w_wready) {
        TICK();
      }
      // trigger wb
      TICK();

      dut->S_AXIL_w_wvalid = false;
      dut->S_AXIL_b_bready = true;
      dut->eval();

      // wait for the response of B
      while (!dut->S_AXIL_b_bvalid) {
        TICK();
      }

      TICK();

      dut->S_AXIL_b_bready = false;
      dut->eval();

      // ack
      // dut.decoupleCurrentRoutine(lock);
      uint32_t success = 0;
      if (!ipc.reply(&success, 1)) {
        dut.reportError("   ERROR:SOCKET:AXIL:WR:REPLY\n");
        TICK();
        goto wait_for_join;
      }
      // dut.attachCurrentRoutine(lock);

    } else {
     // read operation
      dut->S_AXIL_ar_araddr = result.addr;
      dut->S_AXIL_ar_arprot = 0;
      dut->S_AXIL_ar_arvalid = true;
      dut->eval();
      while (!dut->S_AXIL_ar_arready) {
        TICK();
      }
      TICK();

      dut->S_AXIL_ar_arvalid = false;
      dut->S_AXIL_r_rready = true;
      dut->eval();

      while (!dut->S_AXIL_r_rvalid) {
        TICK();
      }
      auto read_res = dut->S_AXIL_r_rdata;
      TICK();

      dut->S_AXIL_r_rready = false;
      dut->eval();

      // read result
      // dut.decoupleCurrentRoutine(lock);
      printf("SHELL:HOST:AXIL:RD[0x%lx]:BURST[%lu]:DATA[0x%x]\n", result.addr, result.byte_size, read_res);
      if (!ipc.reply(&read_res, 1)) {
        dut.reportError("   ERROR:SOCKET:AXIL:RD:REPLY\n");
        TICK();
        goto wait_for_join;
      }
      // dut.attachCurrentRoutine(lock);
    }
  }
wait_for_join:
  while(1) {
    TICK();
  }
}

static int AXI_DRAM_access(TopDUT &dut, IPCServer &ipc, MemoryRequest req) {
  // Bypass AXI port, interact directly with DRAM
  uint64_t addr_byte = req.addr & dut.dram_addr_mask; // DRAM is addressed in 32bit
  uint64_t addr_word = BYTE_TO_WORD(addr_byte);
  if (req.is_write) {
    printf("SHELL:HOST:AXI DRAM:WR[0x%lx]:BURST[%lu]\n", addr_byte, req.byte_size);
    printf("           WORD ADDR[0x%lx]\n", addr_word);
    memcpy(&dut.dram[addr_word], &req.w_data[0], req.byte_size);
    uint32_t data = 0;
    if (!ipc.reply(&data, 1)) {
      dut.reportError("   ERROR:SOCKET:AXI:DRAM WR:REPLY\n");
      return -1;
    }
  } else {
    printf("SHELL:HOST:AXI DRAM:RD[0x%lx]:BURST[%lu]\n", addr_byte, req.byte_size);
    printf("           WORD ADDR[0x%lx]\n", addr_word);
    memcpy(&req.w_data[0], &dut.dram[addr_word], req.byte_size);
    if (!ipc.reply(req.w_data, req.byte_size / 4)) {
      dut.reportError("   ERROR:SOCKET:AXI:DRAM RD:REPLY\n");
      return -1;
    }
  }
  return 0;
}

void AXIRoutine(TopDUT &dut, IPCServer &ipc) {
  std::unique_lock<std::mutex> lock(dut.getLock());
  while (true) {
    MemoryRequest result;
    // dut.decoupleCurrentRoutine(lock);
    while(!ipc.getRequestAXI(&result)) {
      if(ipc.hasPendingError()) {
        dut.reportError("   ERROR:SOCKET:AXI: Failed to get request.\n");
        TICK();
        goto wait_for_join;
      }
      TICK();
    }
    CHECK((result.byte_size % 64) == 0, "AXI write operation must be a multiple of 64B"); // aligned with 64B.
    // we have the read result now.
    if (dut.isAXIDRAM(result.addr)) {
      // Bypass AXI port, interact directly with DRAM
      if(AXI_DRAM_access(dut, ipc, result) == -1) {
        TICK();
        goto wait_for_join;
      }
    } else if (result.is_write) {
      // it's a writing request, activate AXI writing request channel.
      printf("SHELL:HOST:AXI FPGA:WR[0x%lx]:BURST[%lu]\n", result.addr, result.byte_size);
      dut->S_AXI_aw_awaddr = result.addr - dut.axi_base_addr; // get rid of base address.
      dut->S_AXI_aw_awsize = 6; // 64Byte
      dut->S_AXI_aw_awburst = 1;
      dut->S_AXI_aw_awlen = (result.byte_size / BYTES_PER_BLOCK) - 1;
      dut->S_AXI_aw_awvalid = true;
      dut->eval();

      while (!dut->S_AXI_aw_awready) {
        TICK();
      }
      // wait one cycle
      TICK();

      dut->S_AXI_aw_awvalid = false;
      dut->eval();
      // pushing data.
      for (size_t curr_block = 0; curr_block < result.byte_size / BYTES_PER_BLOCK; ++curr_block) {
        memcpy(dut->S_AXI_w_wdata, &result.w_data[curr_block * WORDS_PER_BLOCK], BYTES_PER_BLOCK);
        dut->S_AXI_w_wlast = (curr_block == result.byte_size / BYTES_PER_BLOCK - 1);
        dut->S_AXI_w_wstrb = 0xFFFFFFFFFFFFFFFF;
        dut->S_AXI_w_wvalid = true;
        dut->eval();
        // wait for ready signal
        while (!dut->S_AXI_w_wready) {
          TICK();
        }
        TICK();
      }

      dut->S_AXI_w_wvalid = false;
      dut->S_AXI_b_bready = true;
      dut->eval();

      // wait for the response of B
      while (!dut->S_AXI_b_bvalid) {
        TICK();
      }

      TICK();

      dut->S_AXI_b_bready = false;
      dut->eval();

      // ack
      // dut.decoupleCurrentRoutine(lock);
      uint32_t success = 0;
      if (!ipc.reply(&success, 1)) {
        dut.reportError("   ERROR:SOCKET:AXI:WR:REPLY\n");
        TICK();
        goto wait_for_join;
      }
      // dut.attachCurrentRoutine(lock);

    } else {
      printf("SHELL:HOST:AXI FPGA:RD[0x%lx]:BURST[%lu]\n", result.addr, result.byte_size);
      // read operation
      dut->S_AXI_ar_araddr = result.addr - dut.axi_base_addr;
      dut->S_AXI_ar_arlen = (result.byte_size / BYTES_PER_BLOCK) - 1;
      dut->S_AXI_ar_arburst = 1;
      dut->S_AXI_ar_arsize = 6;
      dut->S_AXI_ar_arvalid = true;
      dut->eval();
      while (dut->S_AXI_ar_arready) {
        TICK();
      }
      TICK();

      dut->S_AXI_ar_arvalid = false;
      dut->S_AXI_r_rready = true;
      dut->eval();

      for (size_t curr_block = 0; curr_block < result.byte_size / BYTES_PER_BLOCK; ++curr_block) {
        while (!dut->S_AXI_r_rvalid) {
          TICK();
        }
        memcpy(&result.w_data[curr_block * WORDS_PER_BLOCK], dut->S_AXI_r_rdata, BYTES_PER_BLOCK);
        if (curr_block == result.byte_size / BYTES_PER_BLOCK - 1) {
          CHECK(dut->S_AXI_r_rlast, "Last block (tlast) mismatch."); // this should be the last element.
        }
        TICK();
      }

      dut->S_AXI_r_rready = false;
      dut->eval();

      // read result
      // dut.decoupleCurrentRoutine(lock);
      if (!ipc.reply(result.w_data, result.byte_size / 4)) {
        dut.reportError("   ERROR:SOCKET:AXI:RD:REPLY\n");
        TICK();
        goto wait_for_join;
      }
      // dut.attachCurrentRoutine(lock);
    }
  }
wait_for_join:
  while(1) {
    TICK()
  }
}
