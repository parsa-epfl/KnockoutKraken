// Amazon FPGA Hardware Development Kit
//
// Copyright 2016 Amazon.com, Inc. or its affiliates. All Rights Reserved.
//
// Licensed under the Amazon Software License (the "License"). You may not use
// this file except in compliance with the License. A copy of the License is
// located at
//
//    http://aws.amazon.com/asl/
//
// or in the "license" file accompanying this file. This file is distributed on
// an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, express or
// implied. See the License for the specific language governing permissions and
// limitations under the License.

#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <malloc.h>
#include <poll.h>

#include <utils/sh_dpi_tasks.h>

#ifdef SV_TEST
# include <fpga_pci_sv.h>
#else
# include <fpga_pci.h>
# include <fpga_mgmt.h>
# include "fpga_dma.h"
# include <utils/lcd.h>
#endif

#include "test_dram_dma_common.h"

#define MEM_16G              (1ULL << 34)

void usage(const char* program_name);
int dma_example_hwsw_cosim(int slot_id, size_t buffer_size);

static inline int do_dma_read(int fd, uint8_t *buffer, size_t size,
    uint64_t address, int channel, int slot_id);
static inline int do_dma_write(int fd, uint8_t *buffer, size_t size,
    uint64_t address, int channel, int slot_id);

# define log_error(...) printf(__VA_ARGS__); printf("\n")
# define log_info(...) printf(__VA_ARGS__); printf("\n")

/* Main will be different for different simulators and also for C. The
 * definition is in sdk/userspace/utils/include/sh_dpi_tasks.h file */
#if defined(INT_MAIN)
/* For cadence and questa simulators main has to return some value */
int test_main(uint32_t *exit_code)
#else
void test_main(uint32_t *exit_code)
#endif
{
    size_t buffer_size;
    buffer_size = 128;

    /* The statements within SCOPE ifdef below are needed for HW/SW
     * co-simulation with VCS */
#if defined(SCOPE)
    svScope scope;
    scope = svGetScopeFromName("tb");
    svSetScope(scope);
#endif

    int rc;
    int slot_id = 0;

    printf("Running example\n");
    rc = dma_example_hwsw_cosim(slot_id, buffer_size);

out:

    if (rc != 0) {
        printf("TEST FAILED \n");
    }
    else {
        printf("TEST PASSED \n");
    }
    /* For cadence and questa simulators main has to return some value */
    #ifdef INT_MAIN
    *exit_code = 0;
    return 0;
    #else
    *exit_code = 0;
    #endif
}

void usage(const char* program_name) {
    printf("usage: %s [--slot <slot>]\n", program_name);
}

/**
 * Write 4 identical buffers to the 4 different DRAM channels of the AFI
 */
int dma_example_hwsw_cosim(int slot_id, size_t buffer_size)
{
    int write_fd, read_fd, dimm, rc;

    write_fd = -1;
    read_fd = -1;

    uint8_t *write_buffer = malloc(buffer_size);
    uint8_t *read_buffer = malloc(buffer_size);
    if (write_buffer == NULL || read_buffer == NULL) {
        rc = -ENOMEM;
        goto out;
    }

    printf("Memory has been allocated, initializing DMA and filling the buffer...\n");
    setup_send_rdbuf_to_c(read_buffer, buffer_size);
    printf("Starting DDR init...\n");
    //init_ddr();
    deselect_atg_hw();
    printf("Done DDR init...\n");
    printf("filling buffer with  random data...\n") ;

    rc = fill_buffer_urandom(write_buffer, buffer_size);
    fail_on(rc, out, "unable to initialize buffer");

    printf("Now performing the DMA transactions...\n");
    bool passed = true;
    dimm = 2;
    rc = do_dma_write(write_fd, write_buffer, buffer_size,
        dimm * MEM_16G, dimm, slot_id);
    fail_on(rc, out, "DMA write failed on DIMM: %d", dimm);
    printf("Pausing SV\n");
    sv_pause(100);
    printf("Do the read");
    rc = do_dma_read(read_fd, read_buffer, buffer_size,
        dimm * MEM_16G, dimm, slot_id);
    printf("Returned read");
    fail_on(rc, out, "DMA read failed on DIMM: %d", dimm);
    uint64_t differ = buffer_compare(read_buffer, write_buffer, buffer_size);
    if (differ != 0) {
        log_error("DIMM %d failed with %lu bytes which differ", dimm, differ);
        passed = false;
    } else {
        log_info("DIMM %d passed!", dimm);
    }

    rc = (passed) ? 0 : 1;

out:
    if (write_buffer != NULL) {
        free(write_buffer);
    }
    if (read_buffer != NULL) {
        free(read_buffer);
    }
    /* if there is an error code, exit with status 1 */
    return (rc != 0 ? 1 : 0);
}

static inline int do_dma_read(int fd, uint8_t *buffer, size_t size,
    uint64_t address, int channel, int slot_id)
{
    sv_fpga_start_cl_to_buffer(slot_id, channel, size, (uint64_t) buffer, address);
    return 0;
}

static inline int do_dma_write(int fd, uint8_t *buffer, size_t size,
    uint64_t address, int channel, int slot_id)
{
    sv_fpga_start_buffer_to_cl(slot_id, channel, size, (uint64_t) buffer, address);
    return 0;
}
