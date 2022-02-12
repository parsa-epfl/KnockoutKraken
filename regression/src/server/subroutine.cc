#include "subroutine.hh"
#include "dut.hh"
#include "ipc.hh"

#include <mutex>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

#ifdef DEBUG
void DebugRoutine(TopDUT &dut) {
  std::unique_lock<std::mutex> lock(dut.getLock());
  DevteroflexArchState state;
  FILE* logfile = fopen("devteroflex_emulation_log.proto", "wb");
  while (true) {
    uint32_t thid = dut.isInstructionBeingCommitted();
    if(thid != -1) {
      // Buffer message or do something with the commited intruction
      dut.getArchState(thid, &state);
      fwrite(&state, sizeof(state), 1, logfile);
    }
    thid = dut.getTransplant();
    if (thid != -1) {
      dut.getArchState(thid, &state);
      fwrite(&state, sizeof(state), 1, logfile);
      fflush(logfile);
    }
    TICK(dut);
  }
}
#endif

void handleDRAMSubroute(TopDUT &dut) {
  std::unique_lock<std::mutex> lock(dut.getLock());
  while (true) {
    if (dut->M_AXI_arvalid) {
      uint64_t addr_byte = dut->M_AXI_araddr;
      // check address. It should be small enough.
      CHECK(dut, addr_byte < dut.dram_size, "AXI read address out of bound");
      CHECK(dut, (addr_byte & 0x3F) == 0, "AXI read address is not aligned");
      CHECK(dut, (dut->M_AXI_arsize == 6), "AXI read burst count must be 64B");
      uint32_t id = dut->M_AXI_arid;
      uint32_t burst_count = dut->M_AXI_arlen + 1;
      dut->M_AXI_arready = 1;

      TICK(dut);

      dut->M_AXI_arready = 0;
      dut->eval();

      // printf("SHELL:FPGA:AXI DRAM:RD[0x%lx]:BURST[%i]\n", addr_byte, burst_count);
      // printf("           WORD ADDR[0x%lx]\n", BYTE_TO_WORD(addr_byte));
      for (int curr_block = 0; curr_block < burst_count; ++curr_block) {
        size_t addr_word = BYTE_TO_WORD(addr_byte) + curr_block * WORDS_PER_BLOCK;

        dut->M_AXI_rid = id;
        dut->M_AXI_rresp = 0;
        for (size_t sub_word = 0; sub_word < WORDS_PER_BLOCK; ++sub_word) {
          dut->M_AXI_rdata[sub_word] = dut.dram[addr_word + sub_word];
        }
        dut->M_AXI_rlast = curr_block == burst_count - 1 ? 1 : 0;
        dut->M_AXI_rvalid = true;
        dut->eval();

        // wait for reading data accepted.
        while (!dut->M_AXI_rready) {
          TICK(dut);
        }

        TICK(dut);
        dut->M_AXI_rvalid = false;
        dut->eval();
      }
    }

    if (dut->M_AXI_awvalid) {
      uint64_t addr_byte = dut->M_AXI_awaddr;
      CHECK(dut, addr_byte < dut.dram_size, "AXI write address out of bound");
      CHECK(dut, (addr_byte & 0x3F) == 0, "AXI write address is not aligned");
      CHECK(dut, (dut->M_AXI_arsize == 6), "AXI write burst count must be 64B");
      uint32_t id = dut->M_AXI_awid;
      uint32_t burst_count = dut->M_AXI_awlen + 1;
      dut->M_AXI_awready = 1;
      dut->eval();
      TICK(dut);

      dut->M_AXI_awready = 0;
      dut->eval();
      // printf("SHELL:FPGA:AXI DRAM:WR[0x%lx]:BURST[%u]\n", addr_byte, burst_count);
      // printf("           WORD ADDR[0x%lx]\n", BYTE_TO_WORD(addr_byte));
      for (int curr_block = 0; curr_block < burst_count; ++curr_block) {

        // setup the write to be ready.
        dut->M_AXI_wready = true;
        dut->eval();

        // wait for data to be valid.
        while (!dut->M_AXI_wvalid) {
          TICK(dut);
        }

        size_t addr_word = BYTE_TO_WORD(addr_byte) + curr_block * WORDS_PER_BLOCK;
        for (size_t sub_word = 0; sub_word < WORDS_PER_BLOCK; ++sub_word) {
          dut.dram[addr_word + sub_word] = dut->M_AXI_wdata[sub_word];
        }
        

        TICK(dut);

        dut->M_AXI_wready = false;
        dut->eval();
      }

      dut->M_AXI_bvalid = true;
      dut->M_AXI_bid = id;
      dut->M_AXI_bresp = 0;
      dut->eval();
      while (!dut->M_AXI_bready) {
        TICK(dut);
      }
      TICK(dut);

      dut->M_AXI_bvalid = false;
      dut->eval();
    }

    TICK(dut);
  }
wait_for_join:
  while(1) {
    TICK(dut);
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
        TICK(dut);
        goto wait_for_join;
      }
      TICK(dut);
    }

    if (result.is_write == 0xDEED) {
      // Sim magic command to advance cycles
      for(int cycle = 0; cycle < result.w_data; cycle ++) {
        TICK(dut);
      }
      uint32_t success = 0;
      if (!ipc.reply(&success, 1)) {
        dut.reportError("   ERROR:SOCKET:AXIL:WR:REPLY\n");
        TICK(dut);
        goto wait_for_join;
      }
      continue; // Get next message
    }
    assert(result.is_write != 0xDEED);

    CHECK(dut, result.byte_size == 4, "AXIL write size must be 4B."); // only one word.
    if (result.is_write) {
      // TODO: Reduce the output inside the subroutine.
      // printf("SHELL:HOST:AXIL:WR[0x%lx]:BURST[%lu]:DATA[%x]\n", result.addr, result.byte_size, result.w_data);
      // it's a writing request, activate AXI writing request channel.
      dut->S_AXIL_awaddr = result.addr;
      dut->S_AXIL_awprot = 0;
      dut->S_AXIL_awvalid = true;
      dut->eval();
      while (!dut->S_AXIL_awready) {
        TICK(dut);
      }
      // wait one cycle
      TICK(dut);

      dut->S_AXIL_awvalid = false;
      dut->eval();
      // pushing data.
      dut->S_AXIL_wdata = result.w_data;
      dut->S_AXIL_wstrb = 0xF;
      dut->S_AXIL_wvalid = true;
      dut->eval();
      // wait for ready signal
      while (!dut->S_AXIL_wready) {
        TICK(dut);
      }
      // trigger wb
      TICK(dut);

      dut->S_AXIL_wvalid = false;
      dut->S_AXIL_bready = true;
      dut->eval();

      // wait for the response of B
      while (!dut->S_AXIL_bvalid) {
        TICK(dut);
      }

      TICK(dut);

      dut->S_AXIL_bready = false;
      dut->eval();

      // ack
      // dut.decoupleCurrentRoutine(lock);
      uint32_t success = 0;
      if (!ipc.reply(&success, 1)) {
        dut.reportError("   ERROR:SOCKET:AXIL:WR:REPLY\n");
        TICK(dut);
        goto wait_for_join;
      }
      // dut.attachCurrentRoutine(lock);

    } else {
     // read operation
      dut->S_AXIL_araddr = result.addr;
      dut->S_AXIL_arprot = 0;
      dut->S_AXIL_arvalid = true;
      dut->eval();
      while (!dut->S_AXIL_arready) {
        TICK(dut);
      }
      TICK(dut);

      dut->S_AXIL_arvalid = false;
      dut->S_AXIL_rready = true;
      dut->eval();

      while (!dut->S_AXIL_rvalid) {
        TICK(dut);
      }
      auto read_res = dut->S_AXIL_rdata;
      TICK(dut);

      dut->S_AXIL_rready = false;
      dut->eval();

      // read result
      // dut.decoupleCurrentRoutine(lock);
      // printf("SHELL:HOST:AXIL:RD[0x%lx]:BURST[%lu]:DATA[0x%x]\n", result.addr, result.byte_size, read_res);
      if (!ipc.reply(&read_res, 1)) {
        dut.reportError("   ERROR:SOCKET:AXIL:RD:REPLY\n");
        TICK(dut);
        goto wait_for_join;
      }
      // dut.attachCurrentRoutine(lock);
    }
  }
wait_for_join:
  while(1) {
    TICK(dut);
  }
}

static int AXI_DRAM_access(TopDUT &dut, IPCServer &ipc, MemoryRequest req) {
  // Bypass AXI port, interact directly with DRAM
  uint64_t addr_byte = req.addr & dut.dram_addr_mask; // DRAM is addressed in 32bit
  uint64_t addr_word = BYTE_TO_WORD(addr_byte);
  if (req.is_write) {
    // printf("SHELL:HOST:AXI DRAM:WR[0x%lx]:BURST[%lu]\n", addr_byte, req.byte_size);
    // printf("           WORD ADDR[0x%lx]\n", addr_word);
    memcpy(&dut.dram[addr_word], &req.w_data[0], req.byte_size);
    uint32_t data = 0;
    if (!ipc.reply(&data, 1)) {
      dut.reportError("   ERROR:SOCKET:AXI:DRAM WR:REPLY\n");
      return -1;
    }
  } else {
    // printf("SHELL:HOST:AXI DRAM:RD[0x%lx]:BURST[%lu]\n", addr_byte, req.byte_size);
    // printf("           WORD ADDR[0x%lx]\n", addr_word);
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
        TICK(dut);
        goto wait_for_join;
      }
      TICK(dut);
    }
    CHECK(dut, (result.byte_size % 64) == 0, "AXI write operation must be a multiple of 64B"); // aligned with 64B.
    // we have the read result now.
    if (dut.isAXIDRAM(result.addr)) {
      // Bypass AXI port, interact directly with DRAM
      if(AXI_DRAM_access(dut, ipc, result) == -1) {
        TICK(dut);
        goto wait_for_join;
      }
    } else if (result.is_write) {
      // it's a writing request, activate AXI writing request channel.
      // printf("SHELL:HOST:AXI FPGA:WR[0x%lx]:BURST[%lu]\n", result.addr, result.byte_size);
      dut->S_AXI_awaddr = result.addr - dut.axi_base_addr; // get rid of base address.
      dut->S_AXI_awsize = 6; // 64Byte
      dut->S_AXI_awburst = 1;
      dut->S_AXI_awlen = (result.byte_size / BYTES_PER_BLOCK) - 1;
      dut->S_AXI_awvalid = true;
      dut->eval();

      while (!dut->S_AXI_awready) {
        TICK(dut);
      }
      // wait one cycle
      TICK(dut);

      dut->S_AXI_awvalid = false;
      dut->eval();
      // pushing data.
      for (size_t curr_block = 0; curr_block < result.byte_size / BYTES_PER_BLOCK; ++curr_block) {
        memcpy(dut->S_AXI_wdata, &result.w_data[curr_block * WORDS_PER_BLOCK], BYTES_PER_BLOCK);
        dut->S_AXI_wlast = (curr_block == result.byte_size / BYTES_PER_BLOCK - 1);
        dut->S_AXI_wstrb = 0xFFFFFFFFFFFFFFFF;
        dut->S_AXI_wvalid = true;
        dut->eval();
        // wait for ready signal
        while (!dut->S_AXI_wready) {
          TICK(dut);
        }
        TICK(dut);
      }

      dut->S_AXI_wvalid = false;
      dut->S_AXI_bready = true;
      dut->eval();

      // wait for the response of B
      while (!dut->S_AXI_bvalid) {
        TICK(dut);
      }

      TICK(dut);

      dut->S_AXI_bready = false;
      dut->eval();

      // ack
      // dut.decoupleCurrentRoutine(lock);
      uint32_t success = 0;
      if (!ipc.reply(&success, 1)) {
        dut.reportError("   ERROR:SOCKET:AXI:WR:REPLY\n");
        TICK(dut);
        goto wait_for_join;
      }
      // dut.attachCurrentRoutine(lock);

    } else {
      // printf("SHELL:HOST:AXI FPGA:RD[0x%lx]:BURST[%lu]\n", result.addr, result.byte_size);
      // read operation
      dut->S_AXI_araddr = result.addr - dut.axi_base_addr;
      dut->S_AXI_arlen = (result.byte_size / BYTES_PER_BLOCK) - 1;
      dut->S_AXI_arburst = 1;
      dut->S_AXI_arsize = 6;
      dut->S_AXI_arvalid = true;
      dut->eval();
      while (dut->S_AXI_arready) {
        TICK(dut);
      }
      TICK(dut);

      dut->S_AXI_arvalid = false;
      dut->S_AXI_rready = true;
      dut->eval();

      for (size_t curr_block = 0; curr_block < result.byte_size / BYTES_PER_BLOCK; ++curr_block) {
        while (!dut->S_AXI_rvalid) {
          TICK(dut);
        }
        memcpy(&result.w_data[curr_block * WORDS_PER_BLOCK], dut->S_AXI_rdata, BYTES_PER_BLOCK);
        if (curr_block == result.byte_size / BYTES_PER_BLOCK - 1) {
          CHECK(dut, dut->S_AXI_rlast, "Last block (tlast) mismatch."); // this should be the last element.
        }
        TICK(dut);
      }

      dut->S_AXI_rready = false;
      dut->eval();

      // read result
      // dut.decoupleCurrentRoutine(lock);
      if (!ipc.reply(result.w_data, result.byte_size / 4)) {
        dut.reportError("   ERROR:SOCKET:AXI:RD:REPLY\n");
        TICK(dut);
        goto wait_for_join;
      }
      // dut.attachCurrentRoutine(lock);
    }
  }
wait_for_join:
  while(1) {
    TICK(dut)
  }
}
