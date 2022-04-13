#include <signal.h>
#include <stdio.h>
#include <stdlib.h>

#include <utils/sh_dpi_tasks.h>
#include <fpga_pci_sv.h>
#include "test_dram_dma_common.h"

#include "ipc.h"
#include "cl_dram_dma_server.h"
#include <sys/socket.h>

static void request_handler_axi(ssize_t port);
static void request_handler_axil(ssize_t port);
static inline int do_axi_read(uint8_t *buffer, size_t size, uint64_t address);
static inline int do_axi_write(uint8_t *buffer, size_t size, uint64_t address);

bool break_loop = false;
ssize_t axi_fd, axil_fd;

void interrupt_handler(int signum) {
    printf("Caught interrupt signal\n");
    shutdown(axi_fd, SHUT_RDWR);
    shutdown(axil_fd, SHUT_RDWR);
    axi_fd = -1;
    axil_fd = -1;
    break_loop = true;
}

int create_request_server(void) {
    int ret = 0;
    signal(SIGINT, interrupt_handler);

    printf("Waiting for AXI connection\n");
    ret = initIPC_server(&axi_fd, "/dev/shm/axi");
    if(ret) {
        perror("AXI socket error, EXIT");
        exit(1);
    }
    printf("Waiting for AXIL connection\n");
    ret = initIPC_server(&axil_fd, "/dev/shm/axi_lite");
    if(ret) {
        perror("AXI socket error, EXIT");
        exit(1);
    }

    printf("Start routine:\n");

    printf("Starting DDR init...\n");
    init_ddr();
    // deselect_atg_hw();
    printf("Done DDR init...\n");

    while(!break_loop) {
        request_handler_axi(axi_fd);
        // if(break_loop) break;
        request_handler_axil(axil_fd);
        // if(break_loop) break;
    }
    return 0;
}

static void request_handler_axil(ssize_t port) {
    MemoryRequestAXIL msg;
    uint32_t word;
    int ack = 1;
    int ret = recvIPC_nonblocky((void *) &msg, port, sizeof(MemoryRequestAXIL));
    if(ret == EMPTY_MESSAGE || port == -1)  {
        return;
    }

    if(msg.is_write) {
        if(msg.is_write == 0xDEED) {
            sv_pause(msg.w_data);
        } else {
            printf("AXIL:WR[0x%016lx]:DATA[0x%016lx]\n", msg.addr, msg.w_data);
            cl_poke_bar1(msg.addr, msg.w_data);
        }
        sendIPC((uint8_t *) &ack, port, 4);
    } else {
        cl_peek_bar1(msg.addr, &word);
        printf("AXIL:RD[0x%016lx]:DATA[0x%016lx]\n", msg.addr, word);
        sendIPC((uint8_t *) &word, port, msg.byte_size);
    }
}

uint8_t data[4096] = {0};
static void request_handler_axi(ssize_t port) {
    MemoryRequest msg; 
    int ack = 1;
    int ret = recvIPC_nonblocky((void *) &msg, port, sizeof(MemoryRequest));
    if(ret == EMPTY_MESSAGE || port == -1)  {
        return;
    }

    if(msg.is_write) {
        printf("AXI:WR[0x%016lx]:BYTES[%u]\n", msg.addr, msg.byte_size);
        do_axi_write((uint8_t *) msg.w_data, msg.byte_size, msg.addr);
        sendIPC((uint8_t *) &ack, port, 4);
    } else {
        do_axi_read(data, msg.byte_size, msg.addr);
        printf("AXI:RD[0x%016lx]:BYTES[%u]\n", msg.addr, msg.byte_size);
        sendIPC(data, port, msg.byte_size);
    }
}

static inline int do_axi_read(uint8_t *buffer, size_t size, uint64_t address)
{
    sv_fpga_start_cl_to_buffer(0, GET_CHANNEL(address), size, (uint64_t) buffer, address);
    return 0;
}

static inline int do_axi_write(uint8_t *buffer, size_t size, uint64_t address)
{
    sv_fpga_start_buffer_to_cl(0, GET_CHANNEL(address), size, (uint64_t) buffer, address);
    return 0;
}
