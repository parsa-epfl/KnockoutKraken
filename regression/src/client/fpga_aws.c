#ifdef AWS_FPGA
#include "utils/log.h"

#include <stdio.h>
#include <unistd.h>

#include "fpga.h"

#include "fpga_dma.h"
#include "fpga_mgmt.h"
#include "fpga_pci.h"
#include "utils/lcd.h"

static uint16_t pci_vendor_id = 0x1D0F; /* Amazon PCI Vendor ID */
static uint16_t pci_device_id =
    0xF000; /* PCI Device ID preassigned by Amazon for F1 applications */

static int check_afi_ready(int slot_id) {
  struct fpga_mgmt_image_info info = {0};

  /* get local image description, contains status, vendor id, and device id. */
  int rc = fpga_mgmt_describe_local_image(slot_id, &info, 0);
  fail_on(
      rc, out,
      "Unable to get AFI information from slot %d. Are you running as root?",
      slot_id);

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
    fail_on(rc, out, "Unable to update PF for slot %d", slot_id);
    /* get local image description, contains status, vendor id, and device id.
     */
    rc = fpga_mgmt_describe_local_image(slot_id, &info, 0);
    fail_on(rc, out, "Unable to get AFI information from slot %d", slot_id);

    printf("AFI PCI  Vendor ID: 0x%x, Device ID 0x%x\n",
           info.spec.map[FPGA_APP_PF].vendor_id,
           info.spec.map[FPGA_APP_PF].device_id);

    /* confirm that the AFI that we expect is in fact loaded after rescan */
    if (info.spec.map[FPGA_APP_PF].vendor_id != pci_vendor_id ||
        info.spec.map[FPGA_APP_PF].device_id != pci_device_id) {
      rc = 1;
      fail_on(rc, out,
              "The PCI vendor id and device of the loaded AFI are not "
              "the expected values.");
    }
  }

  return rc;

out:
  return 1;
}

int initFPGAContext(FPGAContext *c) {
  static const int slot_id = 0;
  static const struct logger *logger = &logger_stdout;

  // init log

  c->axil = PCI_BAR_HANDLE_INIT;
  c->read_fd = -1;
  c->write_fd = -1;

  int res = 0;

  res = log_init("FPGA Tester");
  fail_on(res, failed, "Cannot init log.");

  res = log_attach(logger, NULL, 0);
  fail_on(res, failed, "Cannot attach log.");

  res = fpga_mgmt_init();
  fail_on(res, failed, "Call fpga_mgmt_init failed.");

  res = check_afi_ready(slot_id);
  fail_on(res, failed, "Call check_afi_ready failed.");

  res = fpga_pci_attach(slot_id, FPGA_APP_PF, APP_PF_BAR1, 0, &c->axil);
  fail_on(res, failed, "Call fpga_pci_attach failed.");

  c->read_fd = fpga_dma_open_queue(FPGA_DMA_XDMA, slot_id, 0, true);
  if (c->read_fd < 0) {
    perror("Cannot open DMA read channel.");
    goto failed;
  }

  c->write_fd = fpga_dma_open_queue(FPGA_DMA_XDMA, slot_id, 0, false);
  if (c->write_fd < 0) {
    perror("Cannot open DMA write channel.");
    goto failed;
  }

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

  // reset the DRAM page table
  uint8_t zero_page[4096] = {0};
  for(uint64_t ppn = 0; ppn < (c->ppage_base_addr) / 4096; ++ppn){
    writeAXI(c, ppn << 12, zero_page, 4096);
  }

  return res;
failed:
  releaseFPGAContext(c);
  return -1;
}

int readAXIL(const FPGAContext *c, uint32_t addr, uint32_t *data) {
  int res = fpga_pci_peek(c->axil, addr, data);
  if (res != 0) {
    printf("readAXIL error. Error code = %d\n", res);
    perror("readAXIL error. ");
  }
  return res;
}

int writeAXIL(const FPGAContext *c, uint32_t addr, uint32_t data) {
  int res = fpga_pci_poke(c->axil, addr, data);
  if (res != 0) {
    printf("writeAXIL error. Error code = %d\n", res);
    perror("writeAXIL error. ");
  }
  return res;
}

int readAXI(const struct FPGAContext *c, uint64_t addr, void *data,
            uint64_t size_in_byte) {
  int res =
      fpga_dma_burst_read(c->read_fd, (uint8_t *)data, size_in_byte, addr);
  if (res != 0) {
    printf("readAXI error. Error code = %d\n", res);
    perror("readAXI error. ");
  }
  return res;
}

int writeAXI(const struct FPGAContext *c, uint64_t addr, void *data,
             uint64_t size_in_byte) {
  int res =
      fpga_dma_burst_write(c->write_fd, (uint8_t *)data, size_in_byte, addr);
  if (res != 0) {
    printf("writeAXI error. Error code = %d\n", res);
    perror("writeAXI error. ");
  }
  return res;
}

int releaseFPGAContext(FPGAContext *c) {
  int res = 0;
  if (c->axil != PCI_BAR_HANDLE_INIT) {
    res = fpga_pci_detach(c->axil);
  }
  if (c->read_fd > 0) {
    res = close(c->read_fd);
  }
  if (c->write_fd > 0) {
    res = close(c->write_fd);
  }
  return res;
}

#endif