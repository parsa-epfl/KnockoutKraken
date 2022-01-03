#ifndef AWS_FPGA

#include "fpga.h"
#include "ipc.h"

#include <bits/stdint-uintn.h>
#include <stddef.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <string.h>
#include <stdbool.h>

#define DRAM_AXI_BASE_ADDR (1UL << 40)

/**
 * @file the interface for simulator.
 */

int initFPGAContext(FPGAContext *c){
  c->dram_size = 1024 * 1024 * 16;
  c->axi_fd = -1;
  c->axil_fd = -1;

  c->base_address.axil_base = 0;
  c->base_address.transplant_data = 0;
  c->base_address.tt = 0x8000;
  c->base_address.transplant_ctl = 0x9000;
  c->base_address.message_queue = 0x10000;
  c->base_address.instrumentation_trace = 0x1F000;

  c->base_address.dram_base = 0;
  c->base_address.pt_base = 0;
  c->base_address.page_base = c->dram_size >> 8;
  
  c->base_address.axi_base = c->dram_size;
  c->base_address.message = 0x8000;

  printf("Init AXI socket\n");
  if(initIPC(&c->axi_fd, "/dev/shm/axi") !=0) {
    goto failed;
  }

  printf("Init AXIL socket\n");
  if(initIPC(&c->axil_fd, "/dev/shm/axi_lite") !=0) {
    goto failed;
  }

  printf("Done with sockets\n");
  return 0;

failed:
  releaseFPGAContext(c);
  return -1;
}

int releaseFPGAContext(FPGAContext *c){
  int res = 0;
  if(c->axi_fd > 0) res |= close(c->axi_fd);
  if(c->axil_fd > 0) res |= close(c->axil_fd);
  return res;
}


int readAXIL(const FPGAContext *c, uint32_t addr, uint32_t *data){
  MemoryRequestAXIL request;
  request.addr = addr;
  request.byte_size = 4;
  request.is_write = 0;
  if(c->axil_fd < 0){
    perror("AXIL socket not valid.\n");
    return -1;
  }
  //ssize_t res = send(c->axil_fd, &request, 2*sizeof(size_t) + sizeof(uint32_t), MSG_NOSIGNAL); // send read request
  int res = sendIPC((uint8_t *) &request, c->axil_fd, sizeof(MemoryRequestAXIL));
  if(res != 0) return res;
  res = recvIPC((uint8_t *) data, c->axil_fd,  4);
  if(res != 0) return res;
  return 0;
}

int writeAXIL(const FPGAContext *c, uint32_t addr, uint32_t data){
  MemoryRequestAXIL request;
  request.addr = addr;
  request.byte_size = 4;
  request.is_write = 1;
  request.w_data = data;
  if(c->axil_fd < 0){
    perror("AXIL socket not valid.\n");
    return -1;
  }
  int res = sendIPC((uint8_t *) &request, c->axil_fd, sizeof(MemoryRequestAXIL));
  if(res != 0) return res;
  uint32_t ack_data;
  res = recvIPC((uint8_t *) &ack_data, c->axil_fd,  4);
  if(res != 0) return res;
  return 0;
}

int readAXI(const FPGAContext *c, uint64_t addr, void *data, uint64_t size_in_byte){
  MemoryRequest request;
  request.addr = addr;
  request.byte_size = size_in_byte;
  request.is_write = 0;

  if(size_in_byte > 4096) {
    perror("Trying to read more than the maximum message size\n");
    return -1;
  }
  if(c->axi_fd < 0){
    perror("AXI FD not valid.\n");
    return -1;
  }

  //ssize_t res = send(c->axi_fd, &request, 2*sizeof(size_t) + sizeof(uint32_t), MSG_NOSIGNAL);
  int res = sendIPC((uint8_t *) &request, c->axi_fd, sizeof(MemoryRequest));
  if(res != 0) return res;
  res = recvIPC((uint8_t *) data, c->axi_fd, size_in_byte);
  if(res != 0) return res;

  return 0;
}

int writeAXI(const FPGAContext *c, uint64_t addr, void *data, uint64_t size_in_byte){
  MemoryRequest request;
  request.addr = addr;
  request.byte_size = size_in_byte;
  request.is_write = 1;
  if(size_in_byte > 4096) {
    perror("Trying to write more than the maximum message size\n");
    return -1;
  }
  memcpy(request.w_data, data, size_in_byte);
  if(c->axi_fd > 0){
    int res = sendIPC((uint8_t *) &request, c->axi_fd, sizeof(MemoryRequest));
    if(res != 0) return res;
    uint32_t ack_data;
    res = recvIPC((uint8_t *) &ack_data, c->axi_fd,  4);
    if(res != 0) return res;
  }
  return 0;
}

int writeAXILMagic(const FPGAContext *c, uint32_t addr, uint32_t data){
  MemoryRequestAXIL request;
  request.addr = addr;
  request.byte_size = 4;
  request.is_write = 0xDEED;
  request.w_data = data;
  if(c->axil_fd < 0){
    perror("AXIL socket not valid.\n");
    return -1;
  }
  int res = sendIPC((uint8_t *) &request, c->axil_fd, sizeof(MemoryRequestAXIL));
  if(res != 0) return res;
  uint32_t ack_data;
  res = recvIPC((uint8_t *) &ack_data, c->axil_fd,  4);
  if(res != 0) return res;
  return 0;
}
#endif