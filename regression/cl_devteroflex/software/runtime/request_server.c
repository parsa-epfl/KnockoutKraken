#include <stdlib.h>

#include <utils/sh_dpi_tasks.h>
#include <fpga_pci_sv.h>
#include "test_dram_dma_common.h"

#include "ipc.h"
#include "cl_dram_dma_server.h"

static void request_handler_axi(ssize_t port);
static void request_handler_axil(ssize_t port);
static inline int do_axi_read(uint8_t *buffer, size_t size, uint64_t address);
static inline int do_axi_write(uint8_t *buffer, size_t size, uint64_t address);

int create_request_server(void) {
    int ret = 0;
    bool running = true;
    ssize_t axi_fd, axil_fd;
    ret = initIPC_server(&axi_fd, "/home/eda/.sockets/axi");
    if(ret) {
        perror("AXI socket error, EXIT");
        exit(1);
    }
    ret = initIPC_server(&axil_fd, "/home/eda/.sockets/axi_lite");
    if(ret) {
        perror("AXI socket error, EXIT");
        exit(1);
    }

    printf("Start routine:\n");

    printf("Starting DDR init...\n");
    init_ddr();
    deselect_atg_hw();
    printf("Done DDR init...\n");

    while(running) {
        request_handler_axi(axi_fd);
        request_handler_axil(axil_fd);
        //printf("sv_pause 10\n");
        //sv_pause(10);
    }
    return 0;
}

static void request_handler_axil(ssize_t port) {
    MemoryRequestAXIL msg;
    uint32_t data;
    int ack = 1;
    int ret = recvIPC_nonblocky((void *) &msg, port, sizeof(MemoryRequestAXIL));
    if(ret == EMPTY_MESSAGE)  {
        return;
    }
    printf("Received AXIL transaction:%i:%016lx\n", msg.is_write, msg.addr);
    if(msg.is_write) {
        cl_poke_bar1(msg.addr, msg.w_data);
        sendIPC((uint8_t *) &ack, port, 4);
    } else {
        cl_peek_bar1(msg.addr, &data);
        sendIPC((uint8_t *) &data, port, msg.byte_size);
    }
}

static void request_handler_axi(ssize_t port) {
    MemoryRequest msg; 
    uint8_t data[1024];
    int ack = 1;
    int ret = recvIPC_nonblocky((void *) &msg, port, sizeof(MemoryRequest));
    if(ret == EMPTY_MESSAGE)  {
        return;
    }
    printf("Received AXI transaction:%i:%016lx\n", msg.is_write, msg.addr);

    if(msg.is_write) {
        do_axi_write((uint8_t *) msg.w_data, msg.byte_size, msg.addr);
        sendIPC((uint8_t *) &ack, port, 4);
    } else {
        do_axi_read(data, msg.byte_size, msg.addr);
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
