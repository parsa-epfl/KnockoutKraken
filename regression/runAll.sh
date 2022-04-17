#!/bin/bash

# 1. make sure two test file exists

if [ ! -f build/KnockoutSimulator ] || [ ! -f build/KnockoutTestGenerator ]; then
  echo "Please build the simulator first."
  exit -1
fi

# 1.1 switch into build folder

cd build

# 2. Generate test list

allTests=(
  "check-dram-address"
  "transplant-in" 
  "transplant-transplants" 
  "check-flag-undef"
  "check-flag-transplant"
  "host-cmd-stop-cpu"
  "host-cmd-force-transplant"
  "host-cmd-singlestep"
  "MMU-push-and-evict-pte" 
  "basic-transplant-with-initial-page-fault" 
  "execute-instruction" 
  "execute-instruction-with-context-in-dram" 
  "multiple-pages-in-a-row" 
  "test-ldst-pair-all-sizes"
  "ldr-wback-addr"
  "test-pressure-mmu-same-address"
  "test-pressure-ldp-stp-short"
  )

tbd=(
  "out-of-page-bound-pair-load"
)

# 3. Run each

for t in ${allTests[@]}; do
  # 3.1 start the server
  ./KnockoutSimulator noTrace &
  sleep 1
  # 3.2 start the client
  ./KnockoutTestGenerator $t
  if [ $? != 0 ]; then
    echo "Run test $t failed."
    exit 127
    cd ..
  fi
  sleep 1
done

cd ..
