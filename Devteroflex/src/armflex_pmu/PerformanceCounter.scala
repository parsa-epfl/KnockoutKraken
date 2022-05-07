package armflex_pmu

import chisel3._
import chisel3.util._
import chisel3.experimental._


class PerformanceCounter extends Module {
  // 5 counters:
  //  1. Time to resolve a single data cache miss.
  //  2. Time to resolve a data TLB miss
  //  3. Time to transplant
  //  4. Time to resolve a page fault.
  //  5. Cycles between start and stop. (Controlled by the software)

  

}