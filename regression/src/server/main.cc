#include "dut.hh"
#include "ipc.hh"
#include "subroutine.hh"

#include <csignal>
#include <assert.h>
#include <cstddef>
#include <cstdio>
#include <functional>
#include <mutex>
#include <verilated.h>

double sc_time_stamp() { return TopDUT::time; }

bool closeprogram = false;
bool waitForIPC = false;

void signalHandler(int signum) {
  printf("Caught interrupt signal\n");
  closeprogram = true;
  if(waitForIPC) {
    printf("Caught interrupt signal while waiting for socket, closing program.\n");
    exit(1);
  }
}

int main(int argc, char **argv) {
  bool hasTraceGenerated = true;
  if(argc > 1) {
    hasTraceGenerated = false;
  }
  //Verilated::commandArgs(argc, argv);
  Verilated::assertOn(false);
  Verilated::traceEverOn(hasTraceGenerated);

  TopDUT dut (hasTraceGenerated);

  signal(SIGINT, signalHandler);

  dut.reset();

  // remove previous file
  std::remove("/dev/shm/axi");
  std::remove("/dev/shm/axi_lite");

  // setup the IPC
  IPCServer axil{"/dev/shm/axi_lite"};
  IPCServer axi{"/dev/shm/axi"};

  printf("Wait for connection.\n");

  waitForIPC = true;
  axil.waitForClient();
  axi.waitForClient();
  waitForIPC = false;

  puts("Connection accepted.");

  using namespace std::placeholders;

  dut.registerSubroutine(handleDRAMSubroute);
#ifdef DEBUG
  dut.registerSubroutine(DebugRoutine);
#endif
  dut.registerSubroutine(std::bind(AXILRoutine, _1, std::ref(axil)));
  dut.registerSubroutine(std::bind(AXIRoutine, _1, std::ref(axi)));

  // tick until error occurred.
  for (;;) {
    if (dut.tick())
      break;
    if (closeprogram) {
      break;
    }
  }

  dut.closeSimulation();
  dut.join();

  return 0;
}
