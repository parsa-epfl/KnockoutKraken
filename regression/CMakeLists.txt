project(KnockoutSimulator)
cmake_minimum_required(VERSION 3.12)

set(THREADS_PREFER_PTHREAD_FLAG ON)
find_package(Threads REQUIRED)

set(AWS_FPGA_LIBRARY_PATH /home/xusine/Code/aws-fpga/sdk/userspace)
unset(AWS_FPGA_LIBRARY_PATH) # Comment if simulator

add_executable(
  KnockoutTestGenerator
  src/client/main.cpp
  src/client/fpga_interface.c
  src/client/test-helpers.cc
  src/client/ipc.c
  src/client/tests/asm/asm_helpers.c
  src/client/tests/load-store.cc
  src/client/tests/back-pressure.cc
  src/client/tests/host-control.cc
)

if(AWS_FPGA_LIBRARY_PATH)
  message("Find AWS:" ${AWS_FPGA_LIBRARY_PATH})
  target_include_directories(KnockoutTestGenerator PUBLIC ${AWS_FPGA_LIBRARY_PATH}/include)
  target_link_directories(KnockoutTestGenerator PUBLIC ${AWS_FPGA_LIBRARY_PATH}/lib)
  target_compile_definitions(KnockoutTestGenerator PUBLIC AWS_FPGA)
  target_sources(KnockoutTestGenerator PRIVATE src/client/fpga_aws.c)
  target_link_libraries(KnockoutTestGenerator Threads::Threads fpga_mgmt)
else()
  find_package(verilator REQUIRED)
  target_sources(KnockoutTestGenerator PRIVATE src/client/fpga_rtl.c)
  add_executable(KnockoutSimulator src/server/main.cc src/server/dut.cc src/server/subroutine.cc src/server/ipc.cc)
  #set(VERILATOR_ARGS +define+STOP_COND=1'h0)
  verilate(KnockoutSimulator SOURCES rtl/ARMFlexTop.v rtl/BRAMTDP.v TOP_MODULE ARMFlexTopSimulator TRACE_FST VERILATOR_ARGS)
  target_link_libraries(KnockoutSimulator PRIVATE Threads::Threads)
endif()

#add_executable(garbage_server src/garbage_server.cc)
#add_executable(garbage_client src/garbage_client.cc)
# add_executable(FuckRoutine src/fuck_routine.cc)
#target_link_libraries(FuckRoutine Threads::Threads)
