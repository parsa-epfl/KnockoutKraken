project(KnockoutSimulator)
cmake_minimum_required(VERSION 3.12)

set(THREADS_PREFER_PTHREAD_FLAG ON)
find_package(Threads REQUIRED)

set(AWS_FPGA_LIBRARY_PATH /home/centos/DevteroFlex/aws-fpga/sdk/userspace)
unset(AWS_FPGA_LIBRARY_PATH) # Comment if simulator

set(SIMULATOR_TOP_MODULE DevteroFlexTopLevel) # set to ARMFlexTopInstrumented for instumented code.

add_executable(
  KnockoutTestGenerator
  src/client/main.cpp
  src/client/fpga_interface.c
  src/client/test-helpers.cc
  src/client/fpga_helpers.c
  src/client/ipc.c
  src/client/tests/asm/asm_helpers.c
  src/client/tests/load-store.cc
  src/client/tests/back-pressure.cc
  src/client/tests/trace-module.cc
  src/client/tests/host-control.cc
  src/client/tests/aws-shell-ops.cc
  src/client/tests/transplants.cc
  src/client/tests/memory-ordering.cc
)

add_executable(
  StressTest
  src/client/runs/stress-tests.c
  src/client/tests/asm/asm_helpers.c
  src/client/fpga_interface.c
  src/client/fpga_helpers.c
)

set(RTL_FOLDER cl_devteroflex/design)

if(AWS_FPGA_LIBRARY_PATH)
  message("Find AWS:" ${AWS_FPGA_LIBRARY_PATH})
  target_include_directories(KnockoutTestGenerator PUBLIC ${AWS_FPGA_LIBRARY_PATH}/include)
  target_link_directories(KnockoutTestGenerator PUBLIC ${AWS_FPGA_LIBRARY_PATH}/lib)
  target_compile_definitions(KnockoutTestGenerator PUBLIC AWS_FPGA)
  target_sources(KnockoutTestGenerator PRIVATE src/client/fpga_aws.c)
  target_link_libraries(KnockoutTestGenerator Threads::Threads fpga_mgmt)
  
  target_include_directories(StressTest PUBLIC ${AWS_FPGA_LIBRARY_PATH}/include)
  target_link_directories(StressTest PUBLIC ${AWS_FPGA_LIBRARY_PATH}/lib)
  target_compile_definitions(StressTest PUBLIC AWS_FPGA)
  target_sources(StressTest PRIVATE src/client/fpga_aws.c)
  target_link_libraries(StressTest Threads::Threads fpga_mgmt)

else()
  find_package(verilator REQUIRED)
  set(VERILATOR_ARGS +define+DEBUG=1'h1) #+define+STOP_COND=1'h0)
  #target_compile_definitions(KnockoutTestGenerator PUBLIC DEBUG)
  target_sources(KnockoutTestGenerator PRIVATE src/client/fpga_rtl.c)
  target_sources(StressTest PRIVATE src/client/fpga_rtl.c src/client/ipc.c)

  add_executable(KnockoutSimulator src/server/main.cc src/server/dut.cc src/server/subroutine.cc src/server/ipc.cc)
  verilate(KnockoutSimulator SOURCES ${RTL_FOLDER}/devteroflex_top.sv ${RTL_FOLDER}/ARMFlexTop.v ${RTL_FOLDER}/BRAMTDP.v ${RTL_FOLDER}/MultAdd_3input.v TOP_MODULE ${SIMULATOR_TOP_MODULE} TRACE_FST VERILATOR_ARGS)
  target_link_libraries(KnockoutSimulator PRIVATE Threads::Threads)  
endif()

