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
#include <fpga_pci_sv.h>

#include "test_dram_dma_common.h"
#include "cl_dram_dma_server.h"

#define MEM_16G (1ULL << 34)

void usage(const char* program_name);
int dma_example_basic(int slot_id, size_t buffer_size);

#define log_error(...) printf(__VA_ARGS__); printf("\n")
#define log_info(...) printf(__VA_ARGS__); printf("\n")

/* Main will be different for different simulators and also for C. The
 * definition is in sdk/userspace/utils/include/sh_dpi_tasks.h file */
#if defined(INT_MAIN)
/* For cadence and questa simulators main has to return some value */
int test_main(uint32_t *exit_code)
#else
void test_main(uint32_t *exit_code)
#endif
{

    /* The statements within SCOPE ifdef below are needed for HW/SW
     * co-simulation with VCS */
#if defined(SCOPE)
    svScope scope;
    scope = svGetScopeFromName("tb");
    svSetScope(scope);
#endif

    int rc;
    int slot_id = 0;

    rc = create_request_server();
    fail_on(rc, out, "Test failed.");

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
