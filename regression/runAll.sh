#!/bin/bash

# 1. make sure two test file exists

if [ ! -f build/KnockoutSimulator ] || [ ! -f build/KnockoutTestGenerator ]; then
  echo "Please build the simulator first."
  exit -1
fi



# 1.1 switch into build folder

cd build

# 2. Generate test list

fpgaTests=(
  "check-dram-address"
)

transplantTests=(
  "check-flag-undef"
  "check-flag-transplant"
  "host-cmd-stop-cpu"
  "host-cmd-force-transplant"
  "host-cmd-singlestep"
  "transplant-in" 
  "transplant-transplants" 
)

mmuTests=(
  "MMU-push-and-evict-pte" 
  "basic-transplant-with-initial-page-fault" 
  "multiple-pages-in-a-row" 
  "test-pressure-mmu-same-address"
)

executionTests=(
  "out-of-page-bound-pair-load"
  "execute-instruction" 
  "test-ldst-pair-all-sizes"
  "ldr-wback-addr"
  "test-pressure-ldp-stp-short"
)

microBenchTests=(
  "select-sort-1-threads"
  "select-sort-2-threads"
  "select-sort-15-threads"
  "select-sort-16-threads"
)

allTests=(
  "${fpgaTests[@]}"
  "${transplantTests[@]}"
  "${mmuTests[@]}"
  "${microBenchTests[@]}"
  "${executionTests[@]}"
)

echo """
You can run these types of tests:
  microBenchTests
  executionTests
  mmuTests
  transplantTests
  fpgaTests
"""

tests2run=()
if [ "$1" ==  "microBenchTests" ]; then
tests2run+=("${microBenchTests[@]}")
elif [ "$1" ==  "executionTests" ]; then
tests2run+=("${executionTests[@]}")
elif [ "$1" ==  "mmuTests" ]; then
tests2run+=("${mmuTests[@]}")
elif [ "$1" ==  "transplantTests" ]; then
tests2run+=("${transplantTests[@]}")
elif [ "$1" ==  "fpgaTests" ]; then
tests2run+=("${fpgaTests[@]}")
else
tests2run+=("${allTests[@]}")
fi

# 3. Run each
failed_tests=()
for t in ${tests2run[@]}; do
  # 3.1 start the server
  ./KnockoutSimulator noTrace &> sim-log &
  sleep 1
  # 3.2 start the client
  ./KnockoutTestGenerator $t
  if [ $? != 0 ]; then
    echo "FAIL:Run test $t"
    failed_tests+=("${t}")
  fi
  sleep 1
done

echo "TEST THAT FAILED==============================================================================="
for t in ${failed_tests[@]}; do
  echo "FAILED:$t."
done

echo "DONE"

cd ..
