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
  "multiple-pages-in-a-row" 
  "transplant-in" 
  "MMU-push-and-evict-pte" 
  "basic-transplant-with-initial-page-fault" 
  "transplant-transplants" 
  "execute-instruction-with-context-in-dram" 
  "execute-instruction" 
  "host-cmd-stop-cpu"
  "host-cmd-force-transplant"
  "host-cmd-singlestep"
  "check-flag-undef"
  "check-flag-transplant"
  "test-ldst-pair-all-sizes"
  "test-pressure-mmu-same-address"

  )

tbd=(
  "out-of-page-bound-pair-load"
)

# 3. Run each

for t in ${allTests[@]}; do
  # 3.1 start the server
  ./KnockoutSimulator &
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