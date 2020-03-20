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
#include <stdint.h>
#include <stdbool.h>
#include <unistd.h>

#include <fpga_pci.h>
#include <fpga_mgmt.h>
#include <utils/lcd.h>
#include "helper.c"

/* Constants determined by the CL */

#define TPU_REG_ADDR UINT64_C(0x00)
#define STATE_BASE_ADDR UINT64_C(0xC0000000)
#define PAGE_BASE_ADDR UINT64_C(0xC2000000)

// TODO remove
#define GPIO_REG_ADDR UINT64_C(0x0)
#define BASE_ADDR UINT64_C(0x0C0000000)



/*
 * pci_vendor_id and pci_device_id values below are Amazon's and avaliable to use for a given FPGA slot.
 * Users may replace these with their own if allocated to them by PCI SIG
 */
static uint16_t pci_vendor_id = 0x1D0F; /* Amazon PCI Vendor ID */
static uint16_t pci_device_id = 0xf010; /* PCI Device ID preassigned by Amazon for F1 applications */


/* use the stdout logger for printing debug information  */
const struct logger *logger = &logger_stdout;

/* Declaring the local functions */

int peek_poke_example(int slot, int pf_id, int bar_id);
int vled_example(int slot);

/* Declating auxilary house keeping functions */
int initialize_log(char* log_name);
int check_afi_ready(int slot);


int main(int argc, char **argv) {
    int rc;
    int slot_id;

    /* initialize the fpga_mgmt library */
    rc = fpga_mgmt_init();

    /* initialize the fpga_pci library so we could have access to FPGA PCIe from this applications */
    rc = fpga_pci_init();
    fail_on(rc, out, "Unable to initialize the fpga_pci library");

    /* This demo works with single FPGA slot, we pick slot #0 as it works for both f1.2xl and f1.16xl */

    slot_id = 0;

    rc = check_afi_ready(slot_id);
    fail_on(rc, out, "AFI not ready");

    
    printf("\n");
    printf("===== Start =====\n");
    rc = peek_poke_example(slot_id, FPGA_APP_PF, APP_PF_BAR1);
    fail_on(rc, out, "peek-poke example failed");

    return rc;


out:
    return 1;
}



// global structures
pci_bar_handle_t pci_bar_handle;
pci_bar_handle_t pci_bar_handle_pcis;

/* helper function to write bram*/
int wr32b(uint64_t addr, uint64_t offset, uint32_t data){
    return fpga_pci_poke(pci_bar_handle_pcis, (addr + offset * 0x00000004), data);
}

int wr64b(uint64_t addr, uint64_t offset, uint64_t data){
    int rc;
    rc = wr32b(addr, offset, data>>32);
    fail_on(rc, error, "Wr64b: Unable to write to the fpga !");
    rc = wr32b(addr, offset + 1, data&0x0000ffff);
    fail_on(rc, error, "Wr64b: Unable to write to the fpga !");
    return rc;
error:
    return 1;
}

/* helper function read from bram */
uint32_t rd32b(uint64_t addr, uint64_t offset){
    uint32_t value;
    int rc = fpga_pci_peek(pci_bar_handle_pcis, (addr + offset*4), &value);
    fail_on(rc, out, "Unable to read read from the fpga !");
    return value;
    out:
    return 0;
}
uint64_t rd64b(uint64_t addr, uint64_t offset){
    return (((uint64_t)rd32b(addr, offset*2))<<32) | rd32b(addr, offset*2 + 1);
}



/* fire host->tpu*/
int fire(){
    int rc;
    uint32_t fire_signal = 0x80000000;
    printf("[AFLX-INFO] Host->TPU fire\n");
    rc = fpga_pci_poke(pci_bar_handle, TPU_REG_ADDR, fire_signal);
    fail_on(rc, error, "Unable to write to the fpga !");
    usleep(1);
    fire_signal=0x0;
    rc = fpga_pci_poke(pci_bar_handle, TPU_REG_ADDR, fire_signal);
    fail_on(rc, error, "Unable to write to the fpga !");
    return rc;
error:
    return 1;
}


/* wait until receive done singal from tpu*/
int waitForDone(){
    int rc;
    uint32_t done;
    do
    {
        rc = fpga_pci_peek(pci_bar_handle, (TPU_REG_ADDR + 4), &done);
        fail_on(rc, error, "Unable to read read from the fpga !");
        usleep(1);
    } while (done!=0x1);
    printf("[AFLX-INFO] TPU->Host done\n");
    return rc;
error:
    return 1;
}



/*
 * An example to attach to an arbitrary slot, pf, and bar with register access.
 */
int peek_poke_example(int slot_id, int pf_id, int bar_id) {
    int rc;
    int rc_pcis;

    /* pci_bar_handle_t is a handler for an address space exposed by one PCI BAR on one of the PCI PFs of the FPGA */
    pci_bar_handle = PCI_BAR_HANDLE_INIT;
    pci_bar_handle_pcis = PCI_BAR_HANDLE_INIT;

    /* attach to the fpga, with a pci_bar_handle out param
     * To attach to multiple slots or BARs, call this function multiple times,
     * saving the pci_bar_handle to specify which address space to interact with in
     * other API calls.
     * This function accepts the slot_id, physical function, and bar number
     */
    rc = fpga_pci_attach(slot_id, pf_id, bar_id, 0, &pci_bar_handle);
    fail_on(rc, out, "Unable to attach to the AFI on slot id %d", slot_id);

    rc_pcis = fpga_pci_attach(slot_id, pf_id, 4, 0, &pci_bar_handle_pcis);
    fail_on(rc_pcis, out, "Unable to attach to the AFI on slot id %d", slot_id);

    //open command files for communication
    openCMDFiles();

    while(1){
    // wait for start command
    waitForStart();
    // begin simulation
    

    writeState(STATE_BASE_ADDR);
    writePage(PAGE_BASE_ADDR);
    rc = fire();
    fail_on(rc, out, "Fire failed %d", slot_id);
    // waiting for done signal
    waitForDone();
    // transplant back
    writeStateBack(STATE_BASE_ADDR);
    writeUndefined();
    }
    


 out:
    /* clean up */
    if (pci_bar_handle >= 0) {
        rc = fpga_pci_detach(pci_bar_handle);
        if (rc) {
            printf("Failure while detaching from the fpga.\n");
        }
    }



    if (pci_bar_handle_pcis >= 0) {
        rc_pcis = fpga_pci_detach(pci_bar_handle_pcis);
        if (rc_pcis) {
            printf("Failure while detaching from the fpga.\n");
        }
    }


    /* if there is an error code, exit with status 1 */
    return (rc || rc_pcis != 0 ? 1 : 0);
}

/*
 * check if the corresponding AFI for test_cl is loaded
 */

int check_afi_ready(int slot_id) {
    struct fpga_mgmt_image_info info = {0};
    int rc;

    /* get local image description, contains status, vendor id, and device id. */
    rc = fpga_mgmt_describe_local_image(slot_id, &info,0);
    fail_on(rc, out, "Unable to get AFI information from slot %d. Are you running as root?",slot_id);

    /* check to see if the slot is ready */
    if (info.status != FPGA_STATUS_LOADED) {
        rc = 1;
        fail_on(rc, out, "AFI in Slot %d is not in READY state !", slot_id);
    }

    printf("AFI PCI  Vendor ID: 0x%x, Device ID 0x%x\n",
        info.spec.map[FPGA_APP_PF].vendor_id,
        info.spec.map[FPGA_APP_PF].device_id);

    /* confirm that the AFI that we expect is in fact loaded */
    if (info.spec.map[FPGA_APP_PF].vendor_id != pci_vendor_id ||
        info.spec.map[FPGA_APP_PF].device_id != pci_device_id) {
        printf("AFI does not show expected PCI vendor id and device ID. If the AFI "
               "was just loaded, it might need a rescan. Rescanning now.\n");

        rc = fpga_pci_rescan_slot_app_pfs(slot_id);
        fail_on(rc, out, "Unable to update PF for slot %d",slot_id);
        /* get local image description, contains status, vendor id, and device id. */
        rc = fpga_mgmt_describe_local_image(slot_id, &info,0);
        fail_on(rc, out, "Unable to get AFI information from slot %d",slot_id);

        printf("AFI PCI  Vendor ID: 0x%x, Device ID 0x%x\n",
            info.spec.map[FPGA_APP_PF].vendor_id,
            info.spec.map[FPGA_APP_PF].device_id);

        /* confirm that the AFI that we expect is in fact loaded after rescan */
        if (info.spec.map[FPGA_APP_PF].vendor_id != pci_vendor_id ||
             info.spec.map[FPGA_APP_PF].device_id != pci_device_id) {
            rc = 1;
            fail_on(rc, out, "The PCI vendor id and device of the loaded AFI are not "
                             "the expected values.");
        }
    }

    return rc;

out:
    return 1;
}

