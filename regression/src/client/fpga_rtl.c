#ifndef AWS_FPGA

#include "fpga.h"
#include "ipc.h"

#include <stddef.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <string.h>
#include <time.h>

/**
 * @file the interface for simulator.
 */
int initFPGAContext(FPGAContext *c) {
  c->axi_fd = -1;
  c->axil_fd = -1;

  printf("Init AXI socket\n");
  if(initIPC_client(&c->axi_fd, "/dev/shm/axi") !=0) {
    goto failed;
  }
  printf("Init AXIL socket\n");
  if(initIPC_client(&c->axil_fd, "/dev/shm/axi_lite") !=0) {
    goto failed;
  }
  printf("Done with sockets\n");

  // read platform information
  // Verilog exporting time.
  uint64_t verilog_generated_time = 0;
  if(readAXIL(c, BASE_ADDR_PLATFORM_INFO + VERILOG_GENERATED_TIME_HI, (uint32_t *)&verilog_generated_time) != 0) {
    goto failed;
  }
  verilog_generated_time <<= 32;
  if(readAXIL(c, BASE_ADDR_PLATFORM_INFO + VERILOG_GENERATED_TIME_LO, (uint32_t *)&verilog_generated_time) != 0) {
    goto failed;
  }

  // convert the unix timestamp to actual time.
  struct tm ts;
  localtime_r((time_t *) &verilog_generated_time, &ts);
  char time_buf[80];
  strftime(time_buf, 80, "%Y-%m-%d %H:%M:%S", &ts);
  printf("Verilog Generation Time: %s \n", time_buf);

  // Read PA size
  uint32_t pa_width = 0;
  if(readAXIL(c, BASE_ADDR_PLATFORM_INFO + PLATFORM_PADDR_WIDTH, &pa_width) != 0) {
    goto failed;
  }
  printf("Platform PA size: %d \n", pa_width);

  // Use PA width to initialize the DRAM size.
  c->dram_size = 1ULL << pa_width;
  c->ppage_base_addr = (c->dram_size >> 8);
  // the AXI RTL device is just above the physical pages.
  c->axi_peri_addr_base = c->dram_size;
  // DRAM base is always zero.
  c->dram_addr_base = 0;

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